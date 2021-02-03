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
  
  ### raw_dat
  raw_dat <- reactive({
    nm <- input$data_select
    data(list = nm, package = 'mlbench')
    assign("ret", get(nm))
    if(is.null(rownames(ret)) == TRUE) rownames(ret) <- 1L:nrow(ret)
    
    output$raw_dat_str <- renderPrint({str(ret)})
    return(ret)
  })
  
  
  ### Vector, logical. Rows that contain any NA value.
  rows_na <- reactive({ ## Vector of each obs, TRUE means >1 obs value not complete
    req(raw_dat())
    apply(raw_dat(), 1L, anyNA)
  })
  output$na_msg <- renderPrint({
    req(rows_na())
    sum_na <- sum(rows_na())
    msg <- if(sum_na == 0L){
      "No NA values found."
    }else{
      n <- nrow(raw_dat())
      pct_na <- round(100L * n / sum_na, 2L)
      paste0(sum_na, " (", pct_na, "% of ", n,
             ") rows contained NA values and have been removed.")
    }
    msg
  })
  
  ## Vector, logical. Columns that are either a factor or character
  cols_fct_char <- reactive({
    req(raw_dat())
    apply(type.convert(raw_dat()), 2L, function(c) {
      is.character(c) | is.factor(c)
    })
  })
  
  ### proc_dat 
  proc_dat <- reactive({
    ## Remove NA rows and columns that arte factors or characters
    ret <- raw_dat()[!rows_na(), !cols_fct_char()]
    ## type.convert, hopefully compresses numeric classes their most performant possible class.
    ret <- type.convert(ret) ### Warning, converts text to factors and factors to numeric levels
    ## Scale
    scale_mode <- input$scale_mode
    if(scale_mode == "std dev"){ret <- spinifex::scale_sd(ret)}
    if(scale_mode == "[0, 1]") {ret <- spinifex::scale_01(ret)}
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
  est_idd <- reactive({
    cum_var <- df_scree_pca(pca_obj())$cumsum_var
    est_dim <- rv$curr_dim <- min(which(cum_var > 90L))
    est_dim
  })
  output$pca_msg <- renderText({
    req(est_idd())
    rv$curr_dim
    p <- p()
    cum_var <- df_scree_pca(pca_obj())$cumsum_var[rv$curr_dim]
    
    paste0("The first ", rv$curr_dim, #" (", round(100L * rv$curr_dim / p, 1L), "% of data space)",
           " principle components capture ",
           round(cum_var, 2L), "% of the variance in the processed data.")
  })
  
  ### Proc dat density -----
  output$proc_dat_density <- renderPlot({
    df <- as.data.frame(proc_dat())
    df_long <-
      tidyr::pivot_longer(data = df,
                          cols = tidyr::everything(),
                          names_to = "variable",
                          values_to = "value")
    df_long$variable <- factor(df_long$variable, levels = colnames(df))
    
    ggplot() +
      geom_density(aes(value), df_long) +
      facet_wrap(vars(variable)) +
      theme_minimal() +
      theme(axis.text.y = element_blank())
  })
  
  ### PCA screeplot ----
  ggproto_scree <- reactive({
    req(pca_obj())
    pca_obj <- pca_obj()
    
    list(
      ggproto_screeplot_pca(pca_obj),
      theme_minimal()
    )
  })
  ggproto_bkg_shade_scree <- reactive({
    req(rv$curr_dim)
    est_idd <- rv$curr_dim
    p <- p()
    .lb <- .5
    .mb <- est_idd +.5
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
        max_bases = 20L
      )
    )
    
    .alpha <- min(c(1L, 5L / sqrt(nrow(dat))))
    spinifex::play_tour_path(t_hist,
                             dat,
                             angle = 0.08,
                             render_type = render_plotly,
                             axes = "left",
                             #tooltip = "all",
                             identity_args = list(alpha = .alpha)
                             )
  })
  
  ### PC density -----
  ggproto_density <- reactive({
    df_proj <- as.data.frame(pca_obj()$x)
    df_long <-
      tidyr::pivot_longer(data = df_proj,
                          cols = starts_with("PC"),
                          names_to = "pc",
                          values_to = "value")
    df_long$pc <- factor(df_long$pc, levels = paste0("PC", 1L:p()))
    
    list(
      geom_density(aes(value), df_long),
      facet_wrap(vars(pc)),
      theme_minimal(),
      theme(axis.text.y = element_blank())
    )
  })
  ggproto_bkg_shade_density <- reactive({
    req(est_idd())
    curr_dim <- rv$curr_dim
    p <- p()
    .pc <- paste0("PC", 1L:p)
    df <- data.frame(pc = factor(.pc, levels = .pc),
                     fill = c(rep("aquamarine", curr_dim),
                              rep("firebrick1", p - curr_dim))
    )
    
    geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = I(fill)),
              df, alpha = 0.3)
  })
  output$pc_density_plot <- renderPlot({
    ggplot() +
      ggproto_density() +
      ggproto_bkg_shade_density()
  })
  
  ### tsne plot -----
  output$tsne_plot <- renderPlot({
    req(est_idd())
    df_proj <- as.matrix(pca_obj()$x[, 1L:rv$curr_dim])
    .preplex <- (nrow(df_proj) - 1L) / 3L
    .iter <- 250L
    .theta <- .75
    tnsne_obj <- Rtsne::Rtsne(df_proj, dims = 2L, pca = FALSE,
                              perplexity = .preplex,
                              max_iter = .iter,
                              theta = .theta
                              )
    tnsne_proj <- as.data.frame(tnsne_obj$Y)
    names(tnsne_proj) <- paste0("tsne", 1L:2L)
    pos <- function(x, rate = .95){min(x) + diff(range(x)) * rate}
    .txt_x <- pos(tnsne_proj$tsne1)
    .txt_y <- pos(tnsne_proj$tsne2)
    
    ggplot(tnsne_proj) +
      geom_point(aes(tsne1, tsne2)) +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.title = element_blank()) +
        annotate("text", x = .txt_x, y = .txt_y, color = "grey50",
                 label = paste0("tSNE with hyperparameters \n",
                                "pc_dims = ", rv$curr_dim, " \n",
                                "perprelexity = ", round(.preplex, 2L), " \n",
                                "iterations = ", .iter, " \n",
                                "theta (learning) = ", .theta)
        )
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
