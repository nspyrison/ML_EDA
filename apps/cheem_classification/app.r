# Penguin classification -----
## ./apps/cheem_classification/app.r
#' 
#' @author Nicholas Spyrison
#' June 2021
source("ui.r", local = TRUE, encoding = "utf-8")

server <- function(input, output, session){
  
  ### main plot -----
  
  output$main_plot <- plotly::renderPlotly({
    ## BOX SELECT
    ggplotly(g, tooltip = "rownum") %>% ## Tooltip by name of var name/aes mapping arg.
      config(displayModeBar = FALSE) %>% ## Remove html buttons
      layout(dragmode = "select") %>% ## Set drag left mouse to section box from zoom window
      event_register("plotly_selected") %>% ## Register based on "selected", on the release of th mouse button.
      highlight(on = "plotly_selected", off = "plotly_deselect")
  }) ## Loaded in UI.
  
  ## Selection data lookup ------
  ## What ggplotly sees
  output$selected_plot_df <- renderPrint({ 
    d <- event_data("plotly_selected")
    if (is.null(d)) "Brushed points appear here (double-click to clear)"# else d
  })
  ## Rows of original data
  output$selected_df <- DT::renderDT({ ## Original data of selection
    d <- event_data("plotly_selected")
    if (is.null(d)) return(NULL)
    return(DT::datatable(raw_rmna[d$key, ], rownames = FALSE))
  })
  
} ### Close function, assigning server.

shinyApp(ui = ui, server = server)
