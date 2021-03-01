### ML_EDA/apps/poc/app.r -----

#' {shiny} application for exploratory data analysis in principal 
#' component-space. The primary pipeline is to: input data, 
#' perform preprocessing, estimates the intrinsic data dimensionality,
#' explore ensemble graphics in PC-space with tooltips back to dataspace. 
#' Output options will include processed data, PC-space, ensemble graphics
#' 
#' @author Nicholas Spyrison
#' Feb. 2021

source("ui.r", local = TRUE, encoding = "utf-8")
source("ggproto_screeplot_pca.r", local = TRUE, encoding = "utf-8")

server <- function(input, output, session){
  rv <- reactiveValues()
  rv$curr_dim <- NA_integer_
  observeEvent({input$remove_dim}, {rv$curr_dim <- rv$curr_dim - 1L})
  observeEvent({input$add_dim},    {rv$curr_dim <- rv$curr_dim + 1L})
  
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
  rows_subset_notna <- reactive({
    subsamp_pct <- input$subsample_slider
    if(subsamp_pct == 100L)
      return(TRUE)
    ## Else, apply subsampling:
    req(rows_na())
    raw_dat <- raw_dat()
    rows_na <- rows_na()
    
    notna_dat <- raw_dat[!rows_na, ]
    n <- n_row(notna_dat)
    tgt_n <- round(n * subsamp_pct / 100L)
    
    return(sample(1L:n, tgt_n, replace = FALSE))
  })
  ## A data.frame without na rows & within the subsample 
  truthy_dat <- reactive({
    ret <- raw_dat()
    return(ret[rows_subset_notna(), ])
  })
  ## Vector, logical. Columns that are either a factor or character
  cols_fct_char <- reactive({
    req(raw_dat())
    sapply(type.convert(raw_dat()), function(c){
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
    cols_lt8 <- sapply(this_dat, function(x) length(unique(x)) <= 8L)
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
  
  ### Helpers
  p <- reactive({
    req(proc_dat())
    ncol(proc_dat())
  })
  pca_obj <- reactive({
    req(proc_dat())
    prcomp(proc_dat())
  })
  est_pca80 <- reactive({
    est.pca(pca_obj(), var_cutoff = .8)
  })
  alpha <- reactive({min(c(1L, 5L / sqrt(nrow(raw_dat()))))})
  output$pca_msg <- renderText({
    cum_var <- df_scree_pca(pca_obj())$cumsum_var[rv$curr_dim]
    paste0("The first ", rv$curr_dim, #" (", round(100L * rv$curr_dim / p, 1L), "% of data space)",
           " principle components capture ",
           round(cum_var, 2L), "% of the variance in the processed data.")
  })
  output$pca_header <- renderText({
    paste0(rv$curr_dim, " of ", p()," principal components")
  })
  
  
  ### proc_dat_density -----
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
      
      ggplot() +
        geom_density(aes(value), df_long) +
        geom_rug(aes(value), df_long,
                 alpha = alpha() / 2L) +
        facet_wrap(vars(variable)) +
        theme_minimal() +
        theme(axis.text.y = element_blank())
    }, 
    cacheKeyExpr = {list(proc_dat(), alpha())},
    sizePolicy = sizeGrowthRatio(width = 1100L, height = 682L)
  )
  
  ### pc_screeplot ----
  ggproto_scree <- reactive({
    req(pca_obj())
    pca_obj <- pca_obj()
    
    list(
      ggproto_screeplot_pca(pca_obj),
      theme_minimal(),
      theme(legend.position = "bottom",
            legend.direction = "horizontal")
    )
  })
  ggproto_bkg_shade_scree <- reactive({
    req(rv$curr_dim)
    p <- p()
    .lb <- .5
    .mb <- rv$curr_dim +.5
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
  }, width = 550L, height = 341L)
  outputOptions(output, "pc_screeplot", suspendWhenHidden = FALSE) ## Eager evaluation
  
  ### tour_plotly -----
  spinifex_aes_args <- reactive({
    aes_var_nm <- input$aes_var_nm
    truthy_dat <- truthy_dat()
    ret <- list() ## Initialize
    if(aes_var_nm %in% colnames(truthy_dat)){
      aes_vect <- as.factor(truthy_dat[aes_var_nm][, 1L])
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
    if(tour_nm == "grand"){t_path <- tourr::grand_tour()}
    if(tour_nm == "local"){t_path <- tourr::local_tour(start = bas)}
    
    invisible(utils::capture.output( ## Mute the noisy function
      t_hist <- save_history(
        data <- dat,
        tour_path = t_path,
        max_bases = 10L
      )
    ))
    
    ggp <- spinifex::play_tour_path(t_hist,
                                    dat,
                                    angle = 0.1,
                                    render_type = render_plotly,
                                    axes = "left",
                                    tooltip = "rowname",
                                    aes_args = spinifex_aes_args(),
                                    identity_args = list(alpha = alpha())
    ) %>%
      plotly::layout(showlegend = FALSE,
                     yaxis = list(showgrid = FALSE, showline = FALSE, fixedrange = TRUE),
                     xaxis = list(showgrid = FALSE, showline = FALSE, fixedrange = TRUE)
      ) %>%
      plotly::config(displayModeBar = FALSE)
    
    return(ggp)
  })
  ##outputOptions(output, "tour_plotly", suspendWhenHidden = FALSE) ## Eager evaluation
  
  
  ### idd_tbl -----
  idd_tbl <- reactive({
    req(est_pca80())
    proc_dat <- proc_dat()
    df <- data.frame(
      check.names = FALSE,
      `pca@80%` = est_pca80(),
      # `%IncMSE@90%` = NA,
      # `IncNodePurity@90%` = NA,
      #correlation = Rdimtools::est.correlation(proc_dat)$estdim, # correlation dimension
      ## Error in lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...) : 
      ####  NA/NaN/Inf in 'y'
      made = Rdimtools::est.made(proc_dat)$estdim, ## manifold-adaptive dimension estimation
      mle2 = Rdimtools::est.mle2(proc_dat)$estdim, ## MLE with Poisson process
      twonn = Rdimtools::est.twonn(proc_dat)$estdim ## minimal neighborhood information
      #Ustat = Rdimtools::est.Ustat(proc_dat)$estdim ## ~ 5x slower## convergence rate of U-statistic on manifold
    )
    rownames(df) <- "est_idd"
    return(df)
  })
  output$idd_tbl <- renderTable(idd_tbl())
  outputOptions(output, "idd_tbl", suspendWhenHidden = FALSE) ## Eager evaluation
  
  est_idd <- reactive({
    vec <- as.data.frame(t(as.matrix(idd_tbl())))[, 1L]
    ret <- rv$curr_dim <- rv$anchor_dim <-ceiling(mean(vec, na.rm = TRUE))
    return(ret)
  })
  output$ide_msg <- renderText({
    paste0("Initialize to the first ", est_idd(), " PC.") ## Really the ceiling of the mean of the estimates.
  })
  outputOptions(output, "ide_msg", suspendWhenHidden = FALSE) ## Eager evaluation
  
  ### tsne_plotly -----
  output$tsne_plotly <- plotly::renderPlotly({
    req(est_idd())
    df_proj <- as.matrix(pca_obj()$x[, 1L:rv$curr_dim])
    truthy_dat <- truthy_dat()
    deduped <- unique(df_proj)
    dup_msg <- ""
    if(nrow(deduped) < nrow(df_proj)){
      dup_msg <- "Had to remove duplicated before tSNE. \n"
    }
    
    .preplex <- round(sqrt(nrow(deduped) - 1L) / 3L, 1L)
    .iter <- 500L
    .theta <- .75
    output$tsne_msg <-
      paste0(dup_msg, 
             "hyperparameters: PC_dims = ", rv$curr_dim, ", perplexity = ", .preplex,
             ", theta(learning) = ", .theta, " , max_iterations = ", .iter) %>% 
      renderText()
    
    tnsne_obj <- Rtsne::Rtsne(deduped, dims = 2L, pca = FALSE,
                              perplexity = .preplex,
                              max_iter = .iter,
                              theta = .theta
                              )
    tnsne_proj <- data.frame(
      tsne1 = tnsne_obj$Y[, 1L], 
      tsne2 = tnsne_obj$Y[, 2L],
      rowname = type.convert(rownames(truthy_dat))
    )
    
    ## Find position to place hyper parameter text
    pos <- function(x, rate = .85){min(x) + diff(range(x)) * rate}
    .txt_x <- pos(tnsne_proj$tsne1)
    .txt_y <- pos(tnsne_proj$tsne2)
    
    ## Aesthetic variables
    aes_var_nm <- input$aes_var_nm
    if(aes_var_nm %in% colnames(truthy_dat)){
      tnsne_proj$aes <- as.factor(truthy_dat[aes_var_nm][, 1L])
      tsne_aes <- aes(tsne1, tsne2, rowname = rowname,
                      color = aes, shape = aes)
    }else{
      tsne_aes <- aes(tsne1, tsne2, rowname = rowname)
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
                              "perplexity = ", .preplex, " \n",
                              "theta (learning) = ", .theta, " \n",
                              "max_iterations = ", .iter)
      )
    
    ggp <- plotly::ggplotly(gg, tooltip = "rowname") %>%
      plotly::layout(showlegend = FALSE,
                     yaxis = list(showgrid = FALSE, showline = FALSE, fixedrange = TRUE),
                     xaxis = list(showgrid = FALSE, showline = FALSE, fixedrange = TRUE)
      ) %>%
      plotly::config(displayModeBar = FALSE)
    return(ggp)
  })
  ##outputOptions(output, "tsne_plotly", suspendWhenHidden = FALSE) ## Eager evaluation
  
} ### close function, assigning server.

shinyApp(ui = ui, server = server)
