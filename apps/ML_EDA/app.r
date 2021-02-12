### "Primary" app.R -----
# options(shiny.error = FALSE)

#' {shiny} application for exploratory data analysis in principal 
#' component-space. The primary pipeline is to: input data, 
#' perform preprocessing, estimates the intrinsic data dimensionality,
#' explore ensemble graphics in PC-space with tooltips back to dataspace. 
#' Output options will include processed data, PC-space, ensemble graphics
#' 
#' @author Nicholas Spyrison
#' @export
#' @examples \dontrun{
#' spinifex::run_app("primary")
#' }

#TODO: Initalize code base to ML_EDA use.

source("ui.r", local = TRUE)
source("ggproto_screeplot_pca.r", local = TRUE)

server <- function(input, output, session){
  rv <- reactiveValues()
  rv$curr_dim <- NA_integer_
  
  ### raw_dat, data.frame
  raw_dat <- reactive({
    nm <- input$data_select
    data(list = nm, package = 'mlbench')
    assign("ret", get(nm))
    if(is.null(rownames(ret)) == TRUE) rownames(ret) <- 1L:nrow(ret)
    
    output$raw_dat_str <- renderPrint({str(ret)})
    return(as.data.frame(ret))
  })
  
  ### Vector, logical. Rows that contain any NA value.
  rows_na <- reactive({ ## Vector of each obs, TRUE means >1 obs value not complete
    req(raw_dat())
    apply(raw_dat(), 1L, anyNA)
  })
  output$na_msg <- renderText({
    req(rows_na())
    sum_na <- sum(rows_na())
    if(sum_na == 0L){
      return("No NA values found.")
    }else{
      n <- nrow(raw_dat())
      pct_na <- round(100L * sum_na / n, 2L)
      return(paste0(sum_na, " (", pct_na, "% of ", n,
                    ") rows contained NA values and have been removed."))
    }
  })
  rows_subset_nonna <- reactive({
    subsamp_pct <- input$subsample_slider
    if(subsamp_pct == 100L)
      return(TRUE)
    ## Else, apply subsampling:
    req(rows_na())
    raw_dat <- raw_dat()
    rows_na <- rows_na()
    
    nonna_dat <- raw_dat[!rows_na, ]
    n <- n_row(nonna_dat)
    tgt_n <- round(n * subsamp_pct / 100L)
    
    return(sample(1L:n, tgt_n, replace = FALSE))
  })
  ## A data.frame without na rows & within the subsample 
  truthy_dat <- reactive({
    ret <- raw_dat()[!rows_na(), ]
    return(ret[rows_subset_nonna(), ])
  })
  ## Vector, logical. Columns that are either a factor or character
  cols_fct_char <- reactive({
    req(raw_dat())
    sapply(type.convert(raw_dat()), function(c) {
      is.character(c) | is.factor(c)
    })
  })
  output$aes_var_nm <- renderUI({
    req(raw_dat())
    cols_fct_char <- cols_fct_char()
    if(sum(cols_fct_char) > 0L){## If any fct or char columns look there
      this_dat <- raw_dat()[, cols_fct_char]
    }else{ ## else look at whole data for less than 8 levels
      this_dat <- raw_dat()
    }
    ## Target columns with less than 8 unique values
    cols_lt8 <- apply(this_dat, 2L, function(x) length(unique(x)) <= 8L)
    opts <- colnames(this_dat)[cols_lt8]
    if(length(cols_lt8) == 0L) opts <- "<no suitable variable found>"
      
    selectInput("aes_var_nm", "Color/shape variable",
                choices = opts, selected = opts[1L], multiple = FALSE)
  })
 
  
  output$subsample_msg <- renderText({
    subsamp_pct <- input$subsample_slider
    n <- nrow(raw_dat()[!rows_na(), ])
    if(subsamp_pct == 100L)
      return(paste0("All ", n, " non-NA observations used"))
    ## Else message about subsampling
    tgt_n <- round(n * subsamp_pct / 100L)
    return(paste0(tgt_n, " observations randomly sampled (", subsamp_pct, "% of non-NA observations)"))
  })
  
  ### proc_dat 
  proc_dat <- reactive({
    ## No NA rows, in subset, and exclude columns that are factors or characters
    ret <- truthy_dat()[, !cols_fct_char()] %>%
      type.convert()
    ## type.convert, hopefully compresses numeric classes their most performant possible class.
    ## Warning, may converts text to factors and factors to numeric levels
    
    ## Scale if needed
    scale_mode <- input$scale_mode
    if(scale_mode == "std dev") ret <- spinifex::scale_sd(ret)
    if(scale_mode == "[0, 1]")  ret <- spinifex::scale_01(ret)
    ## Sphere if needed
    if(input$do_sphere == TRUE) ret <- tourr::sphere_data(ret)
    
    return(as.matrix(ret))
  })
  output$proc_dat_smry <- renderPrint({summary(proc_dat())})
  
  clas_df <- reactive({
    ## TODO: NEED CLASS SELECTION FIRST, class_nm()
    # truthy_dat()[class_nm()] ## data.frame return
  })
  
  rf_importance <- reactive({
    ## TODO: NEEDS class/col selection first, class_nm() and selec_col_nms()
    # truthy_dat <- truthy_dat()
    # selec_dat <- truthy_dat[selec_nms()]
    # 
    # rf <- 
    #   randomForest(get(clas_nm()) ~ ., datav = selec_dat, ntree = 1000,
    #                keep.forest = FALSE, importance = TRUE)
    # imp <- importance(rf)
    # ## Return
    # list(imp[order(imp[, 1], decreasing = TRUE), 1],
    #      imp[order(imp[, 2], decreasing = TRUE), 2])
  })
  ### Helpers
  p <- reactive({
    req(proc_dat())
    ncol(proc_dat())
  })
  pca_obj <- reactive({
    req(proc_dat())
    prcomp(proc_dat())
  })
  est_pca90 <- reactive({
    cum_var <- df_scree_pca(pca_obj())$cumsum_var
    est_dim <- min(which(cum_var > 90L))
  })
  alpha <- reactive({min(c(1L, 5L / sqrt(nrow(raw_dat()))))})
  pca_msg <- reactive({
    rv$curr_dim
    p <- p()
    cum_var <- df_scree_pca(pca_obj())$cumsum_var[rv$curr_dim]
    
    paste0("The first ", rv$curr_dim, #" (", round(100L * rv$curr_dim / p, 1L), "% of data space)",
           " principle components capture ",
           round(cum_var, 2L), "% of the variance in the processed data.")
  })
  #output$pca_msg <- renderText(pca_msg())
  output$pca_header <- renderText({
    paste0(rv$curr_dim, " of ", p()," principal components")
  })
  
  
  ### Proc dat density -----
  output$proc_dat_density <- renderCachedPlot(
    {
      req(proc_dat())
      df <- as.data.frame(proc_dat())
      df_long <-
        tidyr::pivot_longer(data = df,
                            cols = tidyr::everything(),
                            names_to = "variable",
                            values_to = "value")
      df_long$variable <- factor(df_long$variable, levels = colnames(df))
      
      egar_eval_pls <- est_pca90()
      
      ggplot() +
        geom_density(aes(value), df_long) +
        geom_rug(aes(value), df_long,
                 alpha = alpha()) +
        facet_wrap(vars(variable)) +
        theme_minimal() +
        theme(axis.text.y = element_blank())
    }, cacheKeyExpr = { list(proc_dat(), alpha()) }
  )
  
  ### PCA screeplot ----
  ggproto_scree <- reactive({
    req(pca_obj())
    pca_obj <- pca_obj()
    
    list(
      ggproto_screeplot_pca(pca_obj),
      theme_minimal(),
      theme(legend.position = "bottom",
            legend.direction = "horizontal"),
      labs(title = "PCA Screeplot",
           subtitle = pca_msg())
    )
  })
  ggproto_bkg_shade_scree <- reactive({
    req(rv$curr_dim)
    est_pca90 <- rv$curr_dim
    p <- p()
    .lb <- .5
    .mb <- est_pca90 +.5
    .ub <- p + .5
    
    list(
      annotate("rect", xmin = .lb, xmax = .mb, ymin = -Inf, ymax = Inf,
               alpha = 0.3, fill = "aquamarine"),
      annotate("rect", xmin = .mb, xmax = .ub, ymin = -Inf, ymax = Inf,
               alpha = 0.3, fill = "firebrick1")
    )
  })
  output$pc_screeplot <- renderPlot({
    ggplot() + 
      ggproto_bkg_shade_scree() +
      ggproto_scree()
    # ## randomForest paints quite a different picture with it's 2 feature importances
    # require(randomForest)
    # proj <- prcomp(mtcars[, 2:11])$x
    # proj_tgt <- data.frame(proj, mpg = mtcars$mpg)
    # proj_fit <- randomForest(mpg ~ ., data = proj_tgt, ntree=1000,
    #                          keep.forest=FALSE, importance=TRUE)
    # importance(proj_fit)
    # varImpPlot(proj_fit)
  })
  
  ### Tour plotly -----
  spinifex_aes_args <- reactive({
    aes_var_nm <- input$aes_var_nm
    truthy_dat <- truthy_dat()
    ret <- list() ## Initialize
    if(aes_var_nm %in% colnames(truthy_dat)){
      aes_vect <- as.factor(truthy_dat[aes_var_nm][, 1])
      rowname <- type.convert(rownames(truthy_dat))
      ret <- list(color = aes_vect, shape = aes_vect, rowname = rowname)
    }
    return(ret)
  })
  output$tour_plotly <- plotly::renderPlotly({
    req(rv$curr_dim)
    pca_obj <- pca_obj()
    bas <- t(pca_obj$rotation[, 1L:rv$curr_dim])[, 1L:2L]
    names(bas) <- paste("y", 1L:2L)
    dat <- pca_obj$x[, 1L:rv$curr_dim]
    
    tour_nm <- input$tour_mode
    if(tour_nm == "grand"){tour_func <- tourr::grand_tour()}
    if(tour_nm == "local"){tour_func <- tourr::local_tour(start = bas)}
    
    ## invisible() quietly() sink(), capture.output() not muting. sink() may be most promising.
    mute <- capture.output(
      t_hist <- save_history(
        data <- dat,
        tour_path = tour_func,
        max_bases = 10L
      )
    )
    
    ggp <- spinifex::play_tour_path(t_hist,
                                    dat,
                                    angle = 0.08,
                                    render_type = render_plotly,
                                    axes = "left",
                                    tooltip = "rowname",
                                    aes_args = spinifex_aes_args(),
                                    identity_args = list(alpha = alpha())
    ) %>% plotly::config(displayModeBar = FALSE)
    
    return(ggp)
  })
  
  ### Est idd -----
  idd_tbl <- reactive({
    req(est_pca90())
    proc_dat <- proc_dat()
    df <- data.frame(
      check.names = FALSE,
      `pca@90%` = est_pca90(),
      `%IncMSE@90%` = NA,
      `IncNodePurity@90%` = NA,
      #correlation = Rdimtools::est.correlation(proc_dat)$estdim, # correlation dimension
      ## Error in lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...) : 
      ####  NA/NaN/Inf in 'y'
      made = Rdimtools::est.made(proc_dat)$estdim, # manifold-adaptive dimension estimation
      mle2 = Rdimtools::est.mle2(proc_dat)$estdim, # MLE with Poisson process
      twonn = Rdimtools::est.twonn(proc_dat)$estdim, # minimal neighborhood information
      Ustat = Rdimtools::est.Ustat(proc_dat)$estdim # convergence rate of U-statistic on manifold
    )
    rownames(df) <- "est_idd"
    return(df)
  })
  output$idd_tbl <- renderTable(idd_tbl())
  est_idd <- reactive({
    vec <- as.data.frame(t(as.matrix(idd_tbl())))[, 1]
    ret <- rv$curr_dim <- ceiling(mean(vec, na.rm = TRUE))
    return(ret)
  })
  output$est_idd_msg <- renderText({
    paste0("Ceiling of mean estimated iid: ", est_idd())
  })
  
  ### PC density -----
  ## TODO: Remove or keep? what does this add?
  # ggproto_density <- reactive({
  #   df_proj <- as.data.frame(pca_obj()$x)
  #   df_long <-
  #     tidyr::pivot_longer(data = df_proj,
  #                         cols = starts_with("PC"),
  #                         names_to = "pc",
  #                         values_to = "value")
  #   df_long$pc <- factor(df_long$pc, levels = paste0("PC", 1L:p()))
  #   
  #   list(
  #     geom_density(aes(value), df_long),
  #     geom_rug(aes(value), df_long,
  #              alpha = alpha()),
  #     facet_wrap(vars(pc)),
  #     theme_minimal(),
  #     theme(axis.text.y = element_blank())
  #   )
  # })
  # ggproto_bkg_shade_density <- reactive({
  #   req(est_idd())
  #   curr_dim <- rv$curr_dim
  #   p <- p()
  #   .pc <- paste0("PC", 1L:p)
  #   df <- data.frame(pc = factor(.pc, levels = .pc),
  #                    fill = c(rep("aquamarine", curr_dim),
  #                             rep("firebrick1", p - curr_dim))
  #   )
  #   
  #   geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = I(fill)),
  #             df, alpha = 0.3)
  # })
  # output$pc_density_plot <- renderPlot({
  #   ggplot() +
  #     ggproto_density() +
  #     ggproto_bkg_shade_density()
  # })
  
  ### tsne plot -----
  output$tsne_plotly <- plotly::renderPlotly({
    req(est_idd())
    df_proj <- as.matrix(pca_obj()$x[, 1L:rv$curr_dim])
    .preplex <- (nrow(df_proj) - 1L) / 3L
    .iter <- 500L
    .theta <- .75
    tnsne_obj <- Rtsne::Rtsne(df_proj, dims = 2L, pca = FALSE,
                              perplexity = .preplex,
                              max_iter = .iter,
                              theta = .theta
                              )
    truthy_dat <- truthy_dat()
    tnsne_proj <- data.frame(
      tsne1 = tnsne_obj$Y[, 1], 
      tsne2 = tnsne_obj$Y[, 2],
      rowname = type.convert(rownames(truthy_dat))
      #, truthy_dat ## if orig data needed.
    )
    
    ## Find position to place hyper parameter text
    pos <- function(x, rate = .85){min(x) + diff(range(x)) * rate}
    .txt_x <- pos(tnsne_proj$tsne1)
    .txt_y <- pos(tnsne_proj$tsne2)
    
    ## Aesthetic variables
    aes_var_nm <- input$aes_var_nm
    tsne_aes <- aes(tsne1, tsne2, rowname = rowname) ## Initialize
    if(aes_var_nm %in% colnames(truthy_dat)){
      tnsne_proj$aes <- as.factor(truthy_dat[aes_var_nm][,1])
      tsne_aes <- aes(tsne1, tsne2, rowname = rowname,
                      color = aes, shape = aes)
    }
    
    gg <- ggplot(tnsne_proj) +
      geom_point(tsne_aes) +
      theme_minimal() +
      scale_color_brewer(palette = "Dark2") +
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            legend.position = "none") +
      
      annotate("text", x = .txt_x, y = .txt_y, color = "grey50",
               label = paste0("tSNE with hyperparameters \n",
                              "pc_dims = ", rv$curr_dim, " \n",
                              "perprelexity = ", round(.preplex, 2L), " \n",
                              "iterations = ", .iter, " \n",
                              "theta (learning) = ", .theta)
      )
    
    ggp <- plotly::ggplotly(gg, tooltip = "rowname") %>%
      plotly::layout(showlegend = FALSE,
                     yaxis = list(showgrid = FALSE, showline = FALSE),
                     xaxis = list(showgrid = FALSE, showline = FALSE)) %>% 
      plotly::config(displayModeBar = FALSE)
    return(ggp)
  })

  
  ### Display to facilitate observing whats going on in the backend.
  observeEvent(input$dev_browser, {browser()})
  output$dev_disp <- renderPrint({
    cat("--- output$Dev_disp --- ",
        paste0("Other content in paste0()s"),
        sep = " \n"
    )
  })
}

shinyApp(ui = ui, server = server)
