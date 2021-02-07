## withSpinner() excample
## see: https://www.rdocumentation.org/packages/shinycssloaders/versions/1.0.0/topics/withSpinner
## https://www.rdocumentation.org/packages/shinycssloaders/versions/1.0.0
## Simply wrap a Shiny output in a call to withSpinner(). If you have %>% loaded, you can use it, for example plotOutput("myplot") %>% withSpinner().
# NOT RUN {
if (interactive()) {
  library(shiny)
  
  shinyApp(
    ui = fluidPage(
      actionButton("go", "Go"),
      shinycssloaders::withSpinner(plotOutput("plot"))
    ),
    server = function(input, output) {
      output$plot <- renderPlot({
        input$go
        
        Sys.sleep(1.5)
        plot(runif(10))
      })
    }
  )
}
# }