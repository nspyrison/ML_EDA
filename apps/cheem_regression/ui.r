### ./apps/cheem/ui.r -----
#' @author Nicholas Spyrison
#' June 2021

#### Dependencies -----
# ## Primary work packages
# require("DALEX")
# require("spinifex")
# require("tourr")
# require("ggplot2")
## EDA and utility
require("GGally")
require("plotly")
require("magrittr")
## Shiny specific
require("shiny")
require("shinythemes") ## Themes for shiny, think preset css styling.
require("shinycssloaders") ## Esp. for renderPlot() %>% withSpinner(type = 8L)
require("DT") ## For html table and buttons

## Local functions
source("trees_of_cheem.r")   ## Cheem functions
source("spinifex_ggproto.r") ## New (spinifex) ggproto_* api
## Load objs
load("./data/1preprocess.RData")
## Loads the following objects:
## dat, tgt_var, maha_lookup_df, shap_df, bound_spaces_df
if(F)
  load("./apps/cheem_regression/data/1preprocess.RData")
if(F){ ## Not run, source/open local function files relative to proj
  source("./apps/cheem_regression/trees_of_cheem.r")
  source("./apps/cheem_regression/spinifex_ggproto.r")
  file.edit("./apps/cheem_regression/trees_of_cheem.r")
  file.edit("./apps/cheem_regression/spinifex_ggproto.r")
  file.edit("./apps/cheem_regression/1preprocess.r")
}

## Initialize
.nn <- nrow(bound_spaces_df)
.clr <- rep_len(maha_lookup_df$maha_dist, .nn)
## too busy; cut out lowest 90% by maha dist
.lb_maha <- quantile(maha_lookup_df$maha_dist, probs = .9)
.idx <- maha_lookup_df$maha_dist > .lb_maha
maha_lookup_df <- maha_lookup_df[.idx,]
dat <- dat[.idx,]
bound_spaces_df <- bound_spaces_df[rep_len(.idx, .nn),]
.clr <- log(.clr[rep_len(.idx, .nn)])
hk <- bound_spaces_df %>%
  highlight_key(~rownum)
g <- ggplot(hk, aes(V1, V2, info = info, color = .clr)) +
  geom_point() +
  facet_grid(rows = vars(data), cols = vars(space)) +
  theme_bw() +
  theme(axis.text  = element_blank(),
        axis.ticks = element_blank(),
        legend.text = element_blank()) +
  scale_color_continuous(name = "log \n mahalonobis \n distance") ## Manual legend title
  

##### tab1_cheem ----
tab1_cheem <- tabPanel(title = "SHAP matrix sensitivity -- FIFA", fluidPage(
  ## Top input row ----
  fluidRow(
    # column(width = 4L,
    #        ## Maha lookup
    #        h4("Mahalonobis lookup table"),
    #        DT::DTOutput("maha_lookup_DT", width = "100%") %>%
    #          shinycssloaders::withSpinner(type = 8L)
    # ),
    column(width = 8L,
           h3("FIFA 2020, preprocess:"),
           p("1) Take the original 42 attributes, Hold wages (in Euros) as our target Y, aggregate correlated, redundant variables in 8 X skill attributes."),
           p("2) Create a Random Forest model predicting wages given our 8 physical and skill attributes."),
           p("3) Extract the SHAP matrix, that is SHAP values for EACH observation (in-sample, obs of a random forest model, via {treeshap})."),
           p("4) Solve the nMDS of the distance matrices, and pca for the data and SHAP values."),
           p("5) For app performance, don't plot the players with the lowest 90% of mahalonobis distances."),
           p("- Load above objects into shiny app; explore with shiny/ggplot2/GGally/plotly."),
           # fluidRow(
           #   column(6L, numericInput("lookup_rownum", "Player id", 1L, 1L, 5000L)),
           #   column(6L, numericInput("plot_cols", "For first ? columns [1, 8]", 3L, 1L, 8L))
           # )
    )
  ),
  
  ## main output row ----
  fluidRow(
    shiny::hr(),
    h3("FIFA 2020 Fielders"),
    h4("Colored by SHAP matrix distance from this player:"),
    p("Explore sensitivity of the SHAP matrix against that the original data."),
    p("Drag to select 1 point, double click to remove the selection. (Box select causes app to hang)."),
    plotly::plotlyOutput("main_plot", width = "100%", height = "600px") %>%
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

