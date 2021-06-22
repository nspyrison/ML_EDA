## Working from:
# https://plotly-r.com/client-side-linking.html
# https://stackoverflow.com/questions/60668621/r-plotly-linked-subplot-with-percentage-histogram-and-categories-coloured#60672601
# https://rdrr.io/cran/plotly/src/inst/examples/shiny/event_data/app.R

### Basically, want to select off of:
# event_data("plotly_brushed")
# event_data("plotly_selected")

### Brushed plot take away; pick your poison:
# - faceted ggplot2
# - plotly api such as add_markers()

require("shiny")
require("plotly")
require("crosstalk")

nms <- row.names(mtcars)
g <- ggplot(mtcars, aes(x = mpg, y = wt, customdata = nms)) + geom_point()

ui <- fluidPage(
  plotlyOutput("plot"),
  plotlyOutput("plot2"),
  verbatimTextOutput("brushed"),
  verbatimTextOutput("selected")
)

server <- function(input, output, session) {
  output$plot <- renderPlotly({ ## master plot
    ggplotly(g) %>%
      layout(dragmode = "select") %>%
      event_register("plotly_selected")
  })
  
  selected_df <- reactive({ ## data within selection
    d <- event_data("plotly_selected")
    if (is.null(d)) "Brushed points appear here (double-click to clear)" else d
  })
  
  output$plot2 <- renderPlotly({ ## Slave plot?
    df <- selected_df()
    if(length(df) == 1L) return()
    #browser()
    .nms <- nms[df$pointNumber]
    p <- ggplot(df, aes(x = x, y = y, customdata = .nms)) +
      geom_point()
    
    ggplotly(p)
  })
  
  ## x,y points of lasso or bounding box of selected
  output$brushed <- renderPrint({
    d <- event_data("plotly_brushed")
    if (is.null(d)) "Brush extents appear here (double-click to clear)" else d
  })
  output$selected <- renderPrint(selected_df())
}

shinyApp(ui, server, options = list(display.mode = "showcase"))