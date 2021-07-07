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
if(F){ ## Not run, source/open local function files relative to proj
  source("./apps/cheem_classification/trees_of_cheem.r")
  load("./apps/cheem_classification/data/1preprocess_rf_treeshap.RData")
  load("./apps/cheem_classification/data/2preprocess_rf_dalex.RData")
  load("./apps/cheem_classification/data/3preprocess_svm_dalex.RData")
  ##
  file.edit("./apps/cheem_classification/trees_of_cheem.r")
  file.edit("./apps/cheem_classification/1preprocess_rf_treeshap.r")
  file.edit("./apps/cheem_classification/2preprocess_rf_dalex.r")
  file.edit("./apps/cheem_classification/3preprocess_svm_dalex.r")
}

## EXPRESSION to make plot
g_expr <- expression({
  bound_spaces_df %>%
    highlight_key(~rownum) %>%
    ggplot(aes(V1, V2, rownum = rownum,
               color = maha_delta, shape = species)) +
    #color = species, shape = sex)) +
    geom_point() +
    # ## Density contours, .99, .5, .1, .01
    # geom_density2d(aes(V1, V2), color = "black",
    #                contour_var = "ndensity", breaks = c(.1, .5, .9)) +
    facet_grid(rows = vars(obs_type), cols = vars(var_space)) +
    theme_bw() +
    theme(axis.text  = element_blank(),
          axis.ticks = element_blank()) +
    scale_color_gradient2(name = "Mahalonobis \n delta, shap - data",
                          low = "blue",
                          mid = "grey",
                          high = "red"
    )
})

## Load  -----
load("./data/1preprocess_rf_treeshap.RData") ## objs: dat_decode, bound_spaces_df
g_rf_treeshap <- eval(g_expr)
load("./data/2preprocess_rf_dalex.RData")
g_rf_dalex <- eval(g_expr)
load("./data/3preprocess_svm_dalex.RData")
g_svm_dalex <- eval(g_expr) 


##### tab1_cheem ----
tab1_cheem <- tabPanel(title = "linked brushing of SHAP- and data- spaces", fluidPage(
  ## Top input row ----
  fluidRow(
    column(width = 4L,
           p("- Data: palmer penguins, X: 4 continuous physical measurements, Classes: species, sex."),
           p("1) Remove NAs rows, remove most distant species."),
           p("2) Create a RF model predicting between remaining species."),
           p("3) Extract SHAP values of EACH observation, bring back to [nxp] observation space."),
           p("4) Create nMDS-, PCA-, and oLDA-, and maha spaces for the orginal and shap values"),
           p("- Load above objects into shiny app; Explore SHAP sensivity with ggplot2/plotly."),
    ),
    column(width = 8L,
     
    )
  ),
  ## main output row ----
  fluidRow(
    shiny::hr(),
    h3("Palmer penguins"),
    p("Color by the difference of the mahalonobis distances, shap - data"),
    p("Shape is the species of the penguin, the membership was the target variable of the model"),
    p("Click or click and drag to select points, double click to remove the selection."),
    selectInput("model_shap_type", label = "Model and shap to use:",
                choices = c("1) Random forest, {treeshap}" = "rf_treeshap",
                            "2) Random forest, {DALEX} shap" = "rf_dalex",
                            "3) SVM (radial), {DALEX} shap" = "svm_dalex")),
    plotly::plotlyOutput("main_plot", width = "100%", height = "700px") %>%
      shinycssloaders::withSpinner(type = 8L),
    hr(),
    h4("Selected data:"),
    verbatimTextOutput("selected_plot_df"), ## What ggplotly sees
    DT::DTOutput("selected_df")
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

