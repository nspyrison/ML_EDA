### ./apps/cheem/app.r -----
#' {shiny} application to interactively explore Cheem and SHAP local attribtions
#' of a new observation, w.r.t. `n` premade RF models. This is performed on 
#' dataset: DALEX::fifa, from Kaggle about soccer data from the 2020 season. 
#' See ?DALEX::fifa for more info. 
#' 
#' @author Nicholas Spyrison
#' June 2021
source("ui.r", local = TRUE, encoding = "utf-8")

server <- function(input, output, session){
  
  
  ### dat_w_shap_dist -----
  shap_dist_quart <- reactive({
    as.factor(shap_dist_quartile[, input$lookup_rownum])
  })
  
  ### shap_ggpairs -----
  shap_ggpairs <- reactive({
    g <- GGally::ggpairs(as.data.frame(shap_df[, 1:input$plot_cols]),
                         mapping = aes(color = shap_dist_quart()),
                         lower = list(continuous = wrap("points", alpha = 0.3))) +
      ggtitle(paste0("SHAP space, colored by SHAP distance from obs# ", input$lookup_rownum)
      )
    plotly::ggplotly(g) %>%
      config(displayModeBar = FALSE)
  })
  output$shap_ggpairs <- plotly::renderPlotly({shap_ggpairs()})
  
  ### var_ggpairs -----
  var_ggpairs <- reactive({
    g <- GGally::ggpairs(as.data.frame(dat[, 1:input$plot_cols]),
                         mapping = aes(color = shap_dist_quart()),
                         lower = list(continuous = wrap("points", alpha = 0.3))) +
      ggtitle(paste0("Variable space, colored by SHAP distance from obs# ", input$lookup_rownum)
      )
    plotly::ggplotly(g) %>%
      plotly::config(displayModeBar = FALSE)
  })
  output$var_ggpairs <- plotly::renderPlotly({var_ggpairs()})
  
  ### maha_lookup_DT -----
  maha_lookup_DT <- reactive({
    return(DT::datatable(
      maha_lookup_df[, 1L:3L], rownames = FALSE, 
      options = list(pageLength = 5)
    ))
  })
  output$maha_lookup_DT <- DT::renderDT({maha_lookup_DT()})
  outputOptions(output, "maha_lookup_DT", suspendWhenHidden = FALSE) ## Eager evaluation
  
  ###nmds_* dat, shap -----
  nmds_shap_plot <- reactive({
    g <- ggplot(nmds_shap, 
                aes(NMDS1, NMDS2, id = id,
                    color = shap_dist_quart(), shape = shap_dist_quart())) +
      geom_point() +
      theme_minimal() +
      theme(legend.position = "bottom", legend.direction = "horizontal")
    plotly::ggplotly(g) %>%
      plotly::config(displayModeBar = FALSE)
  })
  output$nmds_shap_plot <- plotly::renderPlotly({nmds_shap_plot()})
  outputOptions(output, "nmds_shap_plot", suspendWhenHidden = FALSE)
  
  nmds_dat_plot <- reactive({
    g <- ggplot(nmds_dat, 
                aes(NMDS1, NMDS2, id = id,
                    color = shap_dist_quart(), shape = shap_dist_quart())) +
      geom_point() +
      theme_minimal() +
      theme(legend.position = "bottom", legend.direction = "horizontal")
    plotly::ggplotly(g) %>%
      plotly::config(displayModeBar = FALSE)
  })
  output$nmds_dat_plot <- plotly::renderPlotly({nmds_dat_plot()})
  outputOptions(output, "nmds_dat_plot", suspendWhenHidden = FALSE)
  
  ### tsne_plotly -----
  # output$tsne_plotly <- plotly::renderPlotly({
  #   req(rv$curr_dim)
  #   df_proj <- as.matrix(pca_obj()$x[, 1L:rv$curr_dim])
  #   truthy_dat <- truthy_dat()
  #   deduped <- unique(df_proj)
  #   dup_msg <- ""
  #   if(nrow(deduped) < nrow(df_proj)){
  #     dup_msg <- "Had to  duplicated before tSNE. \n"
  #   }
  #   
  #   .preplex <- round(sqrt(nrow(deduped) - 1L) / 3L, 1L)
  #   .iter <- 500L
  #   .theta <- .75
  #   output$tsne_msg <-
  #     paste0(dup_msg,
  #            "hyperparameters: PC_dims = ", rv$curr_dim, ", perplexity = ", .preplex,
  #            ", theta(learning) = ", .theta, " , max_iterations = ", .iter) %>%
  #     renderText()
  #   
  #   tnsne_obj <- Rtsne::Rtsne(deduped, dims = 2L, pca = FALSE,
  #                             perplexity = .preplex,
  #                             max_iter = .iter,
  #                             theta = .theta
  #                             )
  #   tnsne_proj <- data.frame(
  #     tsne1 = tnsne_obj$Y[, 1L],
  #     tsne2 = tnsne_obj$Y[, 2L],
  #     rowname = type.convert(rownames(truthy_dat))
  #   )
  #   
  #   ## Find position to place hyper parameter text
  #   pos <- function(x, rate = .85){min(x) + diff(range(x)) * rate}
  #   .txt_x <- pos(tnsne_proj$tsne1)
  #   .txt_y <- pos(tnsne_proj$tsne2)
  #   
  #   ## Aesthetic variables
  #   aes_var_nm <- input$aes_var_nm
  #   if(aes_var_nm %in% colnames(truthy_dat)){
  #     tnsne_proj$aes <- as.factor(truthy_dat[aes_var_nm][, 1L])
  #                     color = aes, shape = aes)
  #   }else{
  #     tsne_aes <- aes(tsne1, tsne2, rowname = rowname)
  #   }
  #   
  #   gg <- ggplot(tnsne_proj) +
  #     geom_point(tsne_aes) +
  #     theme_minimal() +
  #     scale_color_brewer(palette = "Dark2") +
  #     theme(axis.text = element_blank(),
  #           axis.title = element_blank(),
  #           legend.position = "none") +
  #     
  #     annotate("text", x = .txt_x, y = .txt_y, color = "grey50",
  #              label = paste0("tSNE with hyperparameters \n",
  #                             "pc_dims = ", rv$curr_dim, " \n",
  #                             "perplexity = ", .preplex, " \n",
  #                             "theta (learning) = ", .theta, " \n",
  #                             "max_iterations = ", .iter)
  #     )
  #   
  #   ggp <- plotly::ggplotly(gg, tooltip = "rowname") %>%
  #     plotly::layout(showlegend = FALSE,
  #                    yaxis = list(showgrid = FALSE, showline = FALSE, fixedrange = TRUE),
  #                    xaxis = list(showgrid = FALSE, showline = FALSE, fixedrange = TRUE)
  #     ) %>%
  #     plotly::config(displayModeBar = FALSE)
  #   return(ggp)
  # })
  ##outputOptions(output, "tsne_plotly", suspendWhenHidden = FALSE) ## Eager evaluation
  
} ### close function, assigning server.

shinyApp(ui = ui, server = server)
