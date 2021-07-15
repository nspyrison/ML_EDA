### ./apps/cheem/ui.r -----
#' @author Nicholas Spyrison
#' June 2021

#### Dependencies -----
## EDA and utility
require("ggplot2")
require("plotly")
require("magrittr")
## Shiny specific
require("shiny")
require("shinythemes") ## Themes for shiny, think preset css styling.
require("shinycssloaders") ## Esp. for renderPlot() %>% withSpinner(type = 8L)
require("DT") ## For html table and buttons

## Load prepared objs
load("./data/5regression_rf_treeshap_nested_shaps.RData")

## Loads the objects: 
# formated_ls,
# ggp_expr,
# qq_expr,
if(F){ ## Not run
  ## Load objects
  load("./apps/cheem_regression/data/5regression_rf_treeshap_nested_shaps.RData")
  ## Open file
  file.edit("./apps/cheem_regression/5regression_rf_treeshap_nested_shaps.r")
}

## Prep gg plot -----
eval(ggp_expr) ## Makes gg, and ggp
eval(qq_expr)  ## Makes qq
names(formated_ls)


##### tab1_cheem ----
tab1_cheem <- tabPanel(title = "SHAP sensitivity -- FIFA", fluidPage(
  ## Top input row ----
  fluidRow(
    column(width = 8L,
           h3("FIFA 2020, preprocess:"),
           p("1) Take the original 42 attributes, hold wages (in Euros) as our target Y, aggregate correlated variables down to 8 X, skill attributes."),
           p("2) Remove the goalkeepers, 9.3% of rows."),
           p("3) Hold 20% of fielders to validate the fit."),
           p("4) Create a Random Forest model predicting wages given our 8 skill attributes. (~12 sec)"),
           p("5) Extract the SHAP matrix, that is SHAP values for EACH observation (in-sample, obs of a random forest model, via {treeshap}, (~900 sec))."),
           p("6) Extract pca, and mahalonobis distance for ploting."),
           p("7) Replicate steps 4 to 6 on the newly created shap sapces."),
           p("- Load above objects into shiny app; explore with ggplot2/plotly.")
    )
  ),
  
  ## main output row ----
  fluidRow(
    shiny::hr(),
    h3("FIFA 2020 Fielders"),
    h4("Explore sensitivity of the SHAP matrix against that the original data."),
    p("Highlighting points: click, or click and drag to select, double click to remove the selection."),
    p("Blue text: players with top 0.1% maha distances in data or shap space."),
    fluidRow(
      column(width = 7L,
             plotly::plotlyOutput("main_plot", width = "100%", height = "600px") %>%
               shinycssloaders::withSpinner(type = 8L)
      ),
      column(width = 5L,
             plotOutput("qq_plot", width = "100%", height = "600px") %>%
               shinycssloaders::withSpinner(type = 8L)
      )
    ),
    shiny::hr(),
    h4("Selected data:"),
    DT::DTOutput("selected_df"),
    shiny::hr(),
    h4("Model performance:"),
    verbatimTextOutput("performance_df")
  )
)) ## Assign tab1_cheem


##### tab2_about -----
tab2_about <- tabPanel("About", fluidPage(
  h3("Context & motivation:"),
  p("Modern modeling faces a trade of between interprebility and accuracy of a model. 
    Black-box models use increasingly more and complex interaction terms between features. 
    Doing so allows them to be more accurate, but makes them unrealistically complex to parse and interpret the reasoning and weights used. 
    We want to impove the interprebility of black box models."),
  img(src = "lime_nonlinear.PNG"),
  p('Ribeiro, M. et. al. (2017). Why Should I Trust You?. ', a(href = 'https://www.kdd.org/kdd2016/papers/files/rfp0573-ribeiroA.pdf', 'https://www.kdd.org/kdd2016/papers/files/rfp0573-ribeiroA.pdf', .noWS = "outside"), '!', .noWS = c("after-begin", "before-end")),
  p("Recently, there have been advances in interegating or explaining agnostic models within the local vacinity of a new observation. 
    Some of the original methods of such local explainations of models (Lundberg, 2017) include: LIME, DeepLIFT, and SHAP.
    Here, we build a random foest model (in light of speed), extract SHAP local attributions -- 
    the feature/variable weights in the vasinity of a new observations given the model. 
    Normalizing these features we explore an array of attempts to improve the interprebility of these SHAP-ley values loosely under the name of 'Trees of Cheem'."
  ),
  img(src = "cheem_workflow.png"),
  p('(top) Wickham, H., & Grolemund, G. (2016). R for data science. ', a(href = 'https://r4ds.had.co.nz/', 'https://r4ds.had.co.nz/', .noWS = "outside"), '!', .noWS = c("after-begin", "before-end")),
  p('(bottom) Biecek P. & Burzykowski T. (2020). Explanatory Model Analysis. ', a(href = 'http://ema.drwhy.ai/', 'http://ema.drwhy.ai/', .noWS = "outside"), '!', .noWS = c("after-begin", "before-end")),
  p(""),
  h3("Namesake"),
  img(src = "cheem_namesake.png")
)) ## Assign tab4_about

###### Combined ui object ----
ui <- fluidPage(theme = shinythemes::shinytheme("flatly"),
                ## Content:
                navbarPage("Cheem",
                           tab1_cheem,
                           tab2_about)
)

