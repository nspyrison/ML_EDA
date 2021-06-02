### ML_EDA/apps/poc/ui.r -----

#' @author Nicholas Spyrison
#' Feb. 2021

require("spinifex")
require("tourr")
require("Rdimtools")
require("randomForest")
require("ggplot2")
require("plotly")
require("mlbench") ## For toy datasets
require("tidyr")   ## Needed for pivoting, exports `%>%`
require("shiny")
require("shinythemes") ## Themes for shiny, think preset css styling.
require("shinycssloaders") ## Esp. for renderPlot() %>% withSpinner(type = 8L)
require("DT") ## For html table and buttons

## Initialize
#palette(RColorBrewer::brewer.pal(8L, "Dark2"))

##### tab1_input -----
### Input data, default to flea
tab1_input <- tabPanel("Input", fluidPage(
    sidebarPanel(width = 3L,
      ## Input csv file
      selectInput("data_select", "Data selection, from {mlbench}",
                  choices = mlb_dat),
      uiOutput("proc_dat_inputs"),
      ## Preprocessing
      hr(),
      h3("Preprocessing:"),
      textOutput("na_msg"),
      radioButtons("scale_mode", "scale",
                   choices = c("std dev",
                               "[0, 1]",
                               "none"),
                   selected = "std dev"),
      checkboxInput("do_sphere", "sphere",
                    value = FALSE),
      sliderInput("subsample_slider", label = "Subsample observations [%] -- WIP",
                  min = 10L, max = 100L, value = 100L, step = 10L),
      textOutput("subsample_msg"),
    ),
    ## Main panel display
    mainPanel(
      fluidRow(
        column(width = 4L,
               h3("Input data structure"),
               verbatimTextOutput("raw_dat_str")),
        column(width = 8L,
               h3("Processed data summary"),
               verbatimTextOutput("proc_dat_smry"))
      ),
      h3("Processed data univariate densities"),
      plotOutput("proc_dat_density", width = "1100px", height = "682px")
    ) ## close mainPanel
)) ## Assign tab1_input

##### tab2_explore ----
tab2_explore <- tabPanel("Explore", sidebarLayout(
  fluid = FALSE,
  ## sidebarPanel: IDE, PC screeplot
  sidebarPanel(width = 4L,
               ## Estimating dim
               h3("Intrinsic Data Dimensionality Estimate (IDE)"),
               tableOutput("ide_tbl") %>%
                 shinycssloaders::withSpinner(type = 8L),
               column(width = 12L, h3(textOutput("ide_msg"), align = "center")),
               fluidRow(
                 column(width = 6L, align = "center", actionButton("remove_dim", "< Remove a variable")),
                 column(width = 6L, align = "center", actionButton("add_dim", "Add a variable >")),
               ),
               column(width = 12L, h3(textOutput("pca_header"), align = "center")),
               ## PCA screeplot
               plotOutput("pc_screeplot") %>% withSpinner(type = 8L)
  ),
  ## mainPanel: Tourr, tSNE
  mainPanel(width = 8L,
            ## Tour
            h3("Linear embedding"),
            p("A tour -- animations of linear embeddings (orthonormally constrained)"),
            plotly::plotlyOutput("tour_plotly", width = "720px", height = "480") %>%
              shinycssloaders::withSpinner(type = 8L),
            radioButtons("tour_mode", "Tour mode",
                         choices = c("local",
                                     "grand",
                                     "stepwise (WIP)",
                                     "guided? (WIP)"),
                         selected = "local",
                         inline = TRUE),
            ## tSNE
            h3("Non-linear embedding"),
            p("tSNE, non-linear embedding -- distances not Euclidean; be careful with interpretive claims"),
            textOutput("tsne_msg"),
            plotly::plotlyOutput("tsne_plotly", width = "720px", height = "480px") %>%
              shinycssloaders::withSpinner(type = 8L)
  )
)) ## Assign tab2_explore


##### tab4_about -----
tab4_about <- tabPanel("About (WIP)", fluidPage(
  h3("Work in progress"),
  h2("Thoughts:"),
  p("-Context and scope"),
  p("-ML pyramid/ DS pipeline"),
  p("Sources/references"),
  img(src = "ML_EDA.PNG"),
  p('(top) Wickham, H., & Grolemund, G. (2016). R for data science. ', a(href = 'https://r4ds.had.co.nz/', 'https://r4ds.had.co.nz/', .noWS = "outside"), '!', .noWS = c("after-begin", "before-end")),
  p('(bottom) Biecek P. & Burzykowski T. (2020). Explanatory Model Analysis. ', a(href = 'http://ema.drwhy.ai/', 'http://ema.drwhy.ai/', .noWS = "outside"), '!', .noWS = c("after-begin", "before-end")) 
)) ## Assign tab4_about

###### Combined ui object ----
ui <- fluidPage(theme = shinythemes::shinytheme("flatly"), 
                ## Content:
                navbarPage("Machine learning exploratory data analysis, Proof of Concept",
                           tab1_input,
                           tab2_explore,
                           tab4_about)
)

