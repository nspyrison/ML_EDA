### ./apps/cheem/ui.r -----
#' @author Nicholas Spyrison
#' June 2021

#### Dependencies -----
## Primary work packages
require("DALEX")
require("spinifex")
require("tourr")
require("ggplot2")
## EDA and utility
require("GGally")
require("tictoc")
require("patchwork")
require("plotly")
## Local functions
source("trees_of_cheem.r")   ## Cheem functions
source("spinifex_ggproto.r") ## New (spinifex) ggproto_* api

source("1preprocess.r") ## New (spinifex) ggproto_* api
## Not run, open local function files
if(F){
  file.edit("./apps/cheem/trees_of_cheem.r")
  file.edit("./apps/cheem/spinifex_ggproto.r")
  file.edit("./apps/cheem/1preprocess.r")
}
## Shiny specific
require("tidyr") ## Needed for pivoting, exports `%>%`
require("shiny")
require("shinythemes") ## Themes for shiny, think preset css styling.
require("shinycssloaders") ## Esp. for renderPlot() %>% withSpinner(type = 8L)
require("DT") ## For html table and buttons

## Load & initialize ----

#palette(RColorBrewer::brewer.pal(8L, "Dark2"))



##### tab2_cheem ----
tab1_explore <- tabPanel("Cheem", sidebarLayout(
  fluid = FALSE,
  ## mainPanel: Tourr, tSNE
  mainPanel(width = 12L,
            h1("Fifa data, 2020 season"),
            
            ## Cheem plot 
            h2("Cheem plot"),
            numericInput("ooo_rownum", "Player id", 1L, 1L, 5000L),
            plotOutput("cheem_plot", width = "100%") %>%
              shinycssloaders::withSpinner(type = 8L),
            
            ## Maha lookup
            h2("Mahalonobis lookup table"),
            textInput("search_chr", "search player name:", "",
                      placeholder = "<try searching on sur name>"),
            DT::DTOutput("maha_lookup_DT", width = "100%") %>%
              shinycssloaders::withSpinner(type = 8L),
            
            # ## tSNE
            # h3("Non-linear embedding"),
            # p("tSNE, non-linear embedding -- distances not Euclidean; be careful with interpretive claims"),
            # textOutput("tsne_msg"),
            # plotly::plotlyOutput("tsne_plotly", width = "720px", height = "480px") %>%
            #   shinycssloaders::withSpinner(type = 8L)
  )
)) ## Assign tab1_cheem


##### tab2_about -----
tab2_about <- tabPanel("About", fluidPage(
  h2("Context & motivation:"),
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
                navbarPage("Cheem, Intro",
                           tab1_cheem,
                           tab2_about)
)

