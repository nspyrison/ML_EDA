### ./apps/cheem_regression/app.r -----
#' 
#' @author Nicholas Spyrison
#' June 2021
source("ui.r", local = TRUE, encoding = "utf-8")

server <- function(input, output, session){
  ### maha_lookup_DT -----
  output$maha_lookup_DT <- DT::renderDT({
    DT::datatable(df_pts[, 1L:5L], rownames = TRUE,
                  options = list(pageLength = 5L))
  })
  outputOptions(output, "maha_lookup_DT", suspendWhenHidden = FALSE) ## Eager evaluation
  
  ## main_plot -----
  output$main_plot <- plotly::renderPlotly(ggp)
  output$qq_plot <- renderPlot(qq)
  
  ## Rows of original data
  output$selected_df <- DT::renderDT({ ## Original data of selection
    d <- event_data("plotly_selected")
    if (is.null(d)) return(NULL)
    return(DT::datatable(
      formated_ls$decode_df[formated_ls$decode_df$rownum %in% d$key, ],
      rownames = TRUE))
  })
  
  ## performance df
  output$performance_df <- shiny::renderPrint(formated_ls$performance_df)
} ### close function, assigning server.

shinyApp(ui = ui, server = server)
