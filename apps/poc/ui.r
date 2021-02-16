### ML_EDA/apps/poc/ui.r -----

#' @author Nicholas Spyrison
#' Feb .2021

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

## Initialize
mlb_dat <- data(package = "mlbench")$results[, 3L]
palette(RColorBrewer::brewer.pal(8L, "Dark2"))

##### tab1_input -----
### Input data, default to flea
tab1_input <- tabPanel("Input", fluidPage(
    sidebarPanel(width = 3L,
      ## Input csv file
      selectInput("data_select", "Data selection, from {mlbench}",
                  choices = mlb_dat),
      uiOutput("aes_var_nm"),
      ## Preprocessing
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
    mainPanel(h3("Input data"),
              verbatimTextOutput("raw_dat_str"),
              h3("Processed data univariate densities"),
              plotOutput("proc_dat_density")
    ) ## close mainPanel
)) ## Assign tab1_input

##### tab2_eda ----
### Explore PC-space
tab2_eda <- tabPanel("Explore", fluidPage(
  ### Row 1, PC screeplot, tour
  fluidRow(
    ## Left column, screeplot, buttons
    column(width = 6L,
           plotOutput("pc_screeplot") %>% withSpinner(type = 8L),
           column(width = 3L, actionButton("sw_less", "< Remove a variable")),
           column(width = 6L, h3(textOutput("pca_header"), align = "center")),
           column(width = 3L, actionButton("sw_more", "Add a variable >")),
    ),
    ## Right column, tours
    column(width = 6L,
           h3("Linear embedding"),
           p("A tour -- animations of linear embeddings (orthonormally constrained)"),
           plotly::plotlyOutput("tour_plotly") %>% shinycssloaders::withSpinner(type = 8L),
           radioButtons("tour_mode", "Tour mode",
                        choices = c("stepwise (WIP)",
                                    "local",
                                    "grand"),
                        selected = "grand",
                        inline = TRUE)
    ),
  ), ## End of Row 1, PC screeplot, tour
  ### Row 2, Estimating idd (left), tSNE (right)
  fluidRow(
    ## Left column, pc_density_plot
    column(width = 6L,
           h3("Estimating iid"),
           tableOutput("idd_tbl") %>%
             shinycssloaders::withSpinner(type = 8L),
           column(width = 12L, h3(textOutput("est_idd_msg"), align = "center"))
    ),
    ## Right column, tSNE
    column(width = 6L,
           h3("Non-linear embedding"),
           p("tSNE, non-linear embedding -- distances not Euclidean; be carful with interpretive claims"),
           plotly::plotlyOutput("tsne_plotly") %>%
             shinycssloaders::withSpinner(type = 8L)
    ),
  ) ## End Row 2, Estimating idd
)) ## Assign tab2_eda

##### tab3_output -----
tab3_output <- tabPanel("Output (WIP)", fluidPage(
  h3("Work in progress"),
  h2("Thoughts:"),
  p("-PC space (.rds, .csv)"),
  p("-processed data (.rds, .csv?)"),
  p("-graphics (subset, all?, static?) (.png, .gif?)"),
  p("-Delayed email output of knitr report?")
)) ## Assign tab3_output

##### tab4_about -----
tab4_about <- tabPanel("About (WIP)", fluidPage(
  h3("Work in progress"),
  h2("Thoughts:"),
  p("-Context and scope"),
  p("-ML pyramid/ DS pipeline"),
  p("Sources/references"),
  img(src = "ML_EDA.PNG", align = "left"),
  p('(top) Wickham, H., & Grolemund, G. (2016). R for data science. ', a(href = 'https://r4ds.had.co.nz/', 'https://r4ds.had.co.nz/', .noWS = "outside"), '!', .noWS = c("after-begin", "before-end")),
  p('(bottom) Biecek P. & Burzykowski T. (2020). Explanatory Model Analysis. ', a(href = 'http://ema.drwhy.ai/', 'http://ema.drwhy.ai/', .noWS = "outside"), '!', .noWS = c("after-begin", "before-end")) 
)) ## Assign tab4_about

###### Combined ui object ----
ui <- fluidPage(theme = shinythemes::shinytheme("flatly"), 
                ## Content:
                navbarPage("Machine learning exploratory data analysis, Proof of Concept",
                           tab1_input,
                           tab2_eda,
                           tab3_output,
                           tab4_about)
)

