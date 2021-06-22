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
## Load objs
load("./data/1preprocess.RData")
## Loads the following objects: 
# dat, clas1, clas2, shap_df, dist_dat, dist_shap, bound_spaces_df,
## Initialize
.nn <- nrow(bound_spaces_df)
species <- rep_len(clas1, .nn)
sex <- rep_len(clas2, .nn)

hk <- bound_spaces_df %>%
  highlight_key(~rownum)
g <- ggplot(hk, aes(V1, V2, rownum = rownum,
                    color = species, shape = sex)) +
  geom_point() +
  facet_grid(rows = vars(data), cols = vars(space)) +
  theme_bw() +
  theme(axis.text  = element_blank(),
        axis.ticks = element_blank()) +
  scale_color_discrete(name = "") + ## Manual legend title
  scale_shape_discrete(name = "") ## Manual legend title


if(F){ ## Not run, source/open local function files relative to proj
  source("./apps/cheem_classification/trees_of_cheem.r")
  source("./apps/cheem_classification/spinifex_ggproto.r")
  load("./apps/cheem_classification/data/1preprocess.RData")
  ##
  file.edit("./apps/cheem_classification/trees_of_cheem.r")
  file.edit("./apps/cheem_classification/spinifex_ggproto.r")
  file.edit("./apps/cheem_classification/1preprocess.r")
}

##### tab1_cheem ----
tab1_cheem <- tabPanel(title = "linked brushing of SHAP- and data- spaces", fluidPage(
  ## Top input row ----
  fluidRow(
    column(width = 4L,
           p("- Data: palmer penguins, X: 4 continuous physical measurements, Classes: species, sex."),
           p("1) For each species (level of class 1), create a RF model predicting that species."),
           p("2) For each species model, extract SHAP values of EACH observation, bind for a full [n*p] set."),
           p("3) Create nMDS-, PCA-, and oLDA- spaces for the orginal and shap values, format together"),
           p("- Load above objects into shiny app; wait time is shiny/ggplot2/GGally/plotly."),
    ),
    column(width = 8L,
     
    )
  ),
  ## main output row ----
  fluidRow(
    shiny::hr(),
    h3("Palmer penguins, Color by species, shape by sex"),
    plotly::plotlyOutput("main_plot", width = "100%", height = "700px") %>%
      shinycssloaders::withSpinner(type = 8L),
    shiny::hr(),
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
  p('Ribeiro, M. et. al. (2017). Why Should I Trust You?. ', a(href = 'file:///C:/Users/spyri/Zotero/storage/52VPUVK6/1602.html', 'file:///C:/Users/spyri/Zotero/storage/52VPUVK6/1602.html', .noWS = "outside"), '!', .noWS = c("after-begin", "before-end")),
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

