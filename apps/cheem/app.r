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
  
  ### cheem_plot -----
  cheem_plot <- reactive({
    ## loaded objects: dat, tgt_var, maha_lookup_df, expl
    tictoc::tic(paste0("calc basis_cheem_INSAMP, input$lookup_rownum:", input$lookup_rownum))
    cheem_bas <- basis_cheem_INSAMP(dat, tgt_var, NULL, input$lookup_rownum, expl)
    tictoc::toc()
    
    gg <- plot.basis_cheem_INSAMP(cheem_bas)
    
    return(gg)
  })
  output$cheem_plot <- renderPlot({cheem_plot()})
  
  ### maha_lookup_DT -----
  maha_lookup_DT <- reactive({
    #maha_lookup_df
    # ptn = paste0('^.*', input$search_chr, '.*?')
    # ndx = grep(ptn, maha_lookup_df$name, perl=T)
    return(DT::datatable(maha_lookup_df[, 1L:3L], rownames = FALSE))
  })
  output$maha_lookup_DT <- DT::renderDT({maha_lookup_DT()})
  outputOptions(output, "maha_lookup_DT", suspendWhenHidden = FALSE) ## Eager evaluation
  
  
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
  #     tsne_aes <- aes(tsne1, tsne2, rowname = rowname,
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
