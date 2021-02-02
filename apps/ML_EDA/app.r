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
  ### raw_dat
  raw_dat <- reactive({
    nm <- input$data_select
    data(list = nm, package = 'mlbench')
    assign("ret", get(nm))
    if(is.null(rownames(ret)) == TRUE) rownames(ret) <- 1L:nrow(ret)
    
    return(ret)
  })
  output$raw_dat_str <- renderPrint({str(raw_dat())})
  
  ### Vector, logical. Rows that contain any NA value.
  rows_na <- reactive({ ## Vector of each obs, TRUE means >1 obs value not complete
    req(raw_dat())
    apply(raw_dat(), 1L, anyNA)
  })
  output$na_msg <- renderPrint({
    req(rows_na())
    sum_na <- sum(rows_na())
    msg <- if(sum_na == 0L){
      ""
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
  output$proc_dat_smry <- renderPrint({
    summary(proc_dat())
  })
  
  ### pca_obj 
  pca_obj <- reactive({
    prcomp(proc_dat())
  })
  est_idd <- reactive({
    cum_var <- df_scree_pca(pca_obj())$cumsum_var
    min(which(cum_var > 90))
  })
  output$pc_screeplot <- renderPlot({
    req(est_idd())
    pca_obj <- pca_obj()
    est_idd <- est_idd()
    p <- length(pca_obj[[1]])
    .lb <- .5
    .mb <- est_idd +.5
    .ub <- p + .5

    ggplot() + 
      annotate("rect", xmin = .lb, xmax = .mb, ymin = -Inf, ymax = Inf,
               alpha = 0.3, fill = "aquamarine") +
      annotate("rect", xmin = .mb, xmax = .ub, ymin = -Inf, ymax = Inf,
               alpha = 0.3, fill = "firebrick1") +
      ggproto_screeplot_pca(pca_obj) + 
      theme_minimal()
    # ## randomForest paints quite a different picture.
    # require(randomForest)
    # proj <- prcomp(mtcars[, 2:11])$x
    # proj_tgt <- data.frame(proj, mpg = mtcars$mpg)
    # proj_fit <- randomForest(mpg ~ ., data = proj_tgt, ntree=1000,
    #                          keep.forest=FALSE, importance=TRUE)
    # importance(proj_fit)
    # varImpPlot(proj_fit)
  })
  output$tour_plotly <- plotly::renderPlotly({
    pca_obj <- pca_obj()
    bas <- t(pca_obj$rotation[, 1L:est_idd()])[, 1L:2L]
    names(bas) <- paste("y", 1L:2L)
    dat <- pca_obj$x[, 1L:est_idd()]
    
    tour_nm <- input$tour_mode 
    if(tour_nm == "grand"){tour_func <- tourr::grand_tour()}
    if(tour_nm == "local"){tour_func <- tourr::local_tour(start = bas)}

    t_hist <- save_history(
      data <- dat,
      tour_path = tour_func,
      max_bases = 20L
    )
    spinifex::play_tour_path(t_hist,
                             dat,
                             angle = 0.08,
                             render_type = render_plotly,
                             axes = "left",
                             tooltip = rownames(dat)
                             )
  })
  output$pc_density_plot <- renderPlot({
    df_proj <- as.data.frame(pca_obj()$x)
    est_idd <- est_idd()
    
    df_long <-
      tidyr::pivot_longer(data = df_proj,
                          cols = starts_with("PC"),
                          names_to = "PC",
                          values_to = "value")
    p <- ncol(df_proj)
    
    pc_num <- as.integer(substr(df_long$PC, 3L, nchar(df_long$PC)))
    df_long$bkg_fill <- ifelse(pc_num <= est_idd, "aquamarine", "firebrick1")
    df_long$PC <- factor(df_long$PC, levels = paste0("PC", 1L:p))

    ggplot(df_long) +
      ## geom_rect draws 1 rect for each obs, this may be expensive.
      geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, fill = I(bkg_fill)),
               alpha = 0.01) +
      geom_density(aes(value)) +
      facet_wrap(vars(PC)) +
      theme_minimal() +
      theme(axis.text.y = element_blank())
  })
  
  output$tsne_plot <- renderPlot({
    df_proj <- as.matrix(pca_obj()$x[, 1L:est_idd()])
    .preplex <- (nrow(df_proj) - 1L) / 3L
    .iter <- 250L
    tnsne_obj <- 
      Rtsne::Rtsne(df_proj, dims = 2L, pca = FALSE,
                   perplexity = .preplex,
                   max_iter = .iter)
    tnsne_proj <- as.data.frame(tnsne_obj$Y)
    names(tnsne_proj) <- paste0("tnse", 1L:2L)
    ggplot(tnsne_proj) + 
      geom_point(aes(tnse1, tnse2)) + 
      theme_minimal() +
      theme(axis.text = element_blank())
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
