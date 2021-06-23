### ./apps/cheem_regression/app.r -----
#' 
#' @author Nicholas Spyrison
#' June 2021
source("ui.r", local = TRUE, encoding = "utf-8")

server <- function(input, output, session){
  
  # ### dat_w_shap_dist -----
  # ## for color, not using shap right now.
  # shap_dist_quart <- reactive({
  #   as.factor(shap_dist_quartile[, input$lookup_rownum])
  # })
  
  
  ### maha_lookup_DT -----
  output$maha_lookup_DT <- DT::renderDT({
    DT::datatable(
      maha_lookup_df[, 1L:3L], rownames = FALSE,
      options = list(pageLength = 5L))
  })
  outputOptions(output, "maha_lookup_DT", suspendWhenHidden = FALSE) ## Eager evaluation
  
  ## main_plot
  output$main_plot <- plotly::renderPlotly({
    ## CLICK SELECT
    ggplotly(g, tooltip = "rownum") %>% ## Tooltip by name of var name/aes mapping arg.
      config(displayModeBar = FALSE) %>% ## Remove html buttons
      layout(dragmode = FALSE) %>% ## Set drag left mouse to section box from zoom window
      event_register("plotly_selected") %>% ## Register based on "selected", on the release of th mouse button.
      highlight(on = 'plotly_click', off = "plotly_doubleclick",
                persistent = FALSE) ## Allow selection of many points?
  })
  
  ## Selection data lookup ------
  ## What ggplotly sees
  output$selected_plot_df <- renderPrint({
    d <- event_data("plotly_selected")
    if (is.null(d)) "Brushed points appear here (double-click to clear)" # else d
  })
  ## Rows of original data
  output$selected_df <- DT::renderDT({ ## Original data of selection
    d <- event_data("plotly_selected")
    if (is.null(d)) return(NULL)
    return(DT::datatable(raw_rmna[d$key, ], rownames = FALSE))
  })
  
} ### close function, assigning server.

shinyApp(ui = ui, server = server)
