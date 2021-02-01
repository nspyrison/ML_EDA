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

source("ui.R", local = TRUE)

server <- function(input, output, session){
  ### raw_dat
  raw_dat <- reactive({
    tourr::flea[]
  })
  output$raw_dat_str <- renderPrint({str(raw_dat())})
  verbatimTextOutput("raw_dat_dty")
  
  ### proc_dat 
  proc_dat <- reactive({
    raw_dat <- raw_dat()
    ret <- raw_dat[, apply(raw_dat, MARGIN = 2, is.numeric())]
    ret <- input$scale_mode(ret)
    if(input$do_sphere == TRUE) ret <- tourr::sphere_data(ret)
    retrn(ret)
  })
  output$proc_dat_smry <- renderPrint({summary(proc_dat())})
  

  output$pc_screeplot <- renderPlot({
    plot(0,0)
  })
  
  output$tour_plot <- renderPlot({
    plot(0,0)
  })
  
  output$pc_density_plot <- renderPlot({
    plot(0,0)
  })
  
  output$tsne_plot <- renderPlot({
    plot(0,0)
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
