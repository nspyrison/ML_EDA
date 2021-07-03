### ./apps/cheem_regression/app.r -----
#' 
#' @author Nicholas Spyrison
#' June 2021
source("ui.r", local = TRUE, encoding = "utf-8")

server <- function(input, output, session){
  ### maha_lookup_DT -----
  output$maha_lookup_DT <- DT::renderDT({
    DT::datatable(dat[, 1L:5L], rownames = TRUE,
                  options = list(pageLength = 5L))
  })
  outputOptions(output, "maha_lookup_DT", suspendWhenHidden = FALSE) ## Eager evaluation
  
  ## main_plot -----
  output$main_plot <- plotly::renderPlotly({
    ## BOX SELECT
    ggplotly(g, tooltip = "info") %>% ## Tooltip by name of var name/aes mapping arg.
      config(displayModeBar = FALSE) %>% ## Remove html buttons
      layout(dragmode = "select") %>% ## Set drag left mouse to section box from zoom window
      event_register("plotly_selected") %>% ## Register based on "selected", on the release of th mouse button.
      highlight(on = "plotly_selected", off = "plotly_deselect")
  })
  
  ## Selection data lookup ------
  output$selected_plot_df <- renderPrint({
    d <- event_data("plotly_selected")
    if (is.null(d)) "Selected point appears here (double-click to clear)" # else d
  })
  ## Rows of original data
  output$selected_df <- DT::renderDT({ ## Original data of selection
    d <- event_data("plotly_selected")
    if (is.null(d)) return(NULL)
    return(DT::datatable(dat[dat$rownum == d$key, ], rownames = TRUE))
  })
} ### close function, assigning server.

shinyApp(ui = ui, server = server)
