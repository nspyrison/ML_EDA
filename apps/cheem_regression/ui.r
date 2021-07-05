### ./apps/cheem/ui.r -----
#' @author Nicholas Spyrison
#' June 2021

#### Dependencies -----
## EDA and utility
require("ggplot2")
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
# source("spinifex_ggproto.r") ## New (spinifex) ggproto_* api
## Load objs
load("./data/1preprocess.RData") ## Loads the objects: dat, bound_spaces_df
if(F)
  load("./apps/cheem_regression/data/1preprocess.RData")
if(F){ ## Not run, source/open local function files relative to proj
  source("./apps/cheem_regression/trees_of_cheem.r")
  source("./apps/cheem_regression/spinifex_ggproto.r")
  file.edit("./apps/cheem_regression/trees_of_cheem.r")
  file.edit("./apps/cheem_regression/spinifex_ggproto.r")
  file.edit("./apps/cheem_regression/1preprocess.r")
}

## Prep gg plot -----
## grey and color pts
df <- bound_spaces_df
idx_dat  <- bound_spaces_df$maha_dat  > quantile(bound_spaces_df$maha_dat, probs = .98)
idx_shap <- bound_spaces_df$maha_shap > quantile(bound_spaces_df$maha_shap, probs = .98)
pts_idx <- idx_dat | idx_shap
grey_pts_idx <- !pts_idx
sum(pts_idx)/4
## find txt pts
idx_dat  <- bound_spaces_df$maha_dat  > quantile(bound_spaces_df$maha_dat, probs = .999)
idx_shap <- bound_spaces_df$maha_shap > quantile(bound_spaces_df$maha_shap, probs = .999)
txt_pts_idx <- idx_dat | idx_shap
sum(txt_pts_idx)/4
## Plot
g <- df[pts_idx, ] %>%
  ## Plotly interaction key
  plotly::highlight_key(~rownum) %>%
  ggplot(aes(V1, V2)) +
  ## Grey points
  geom_point(aes(shape = maha_shape), data = df[grey_pts_idx, ], color = "grey") +
  ## Density contours, .99, .5, .1, .01
  geom_density2d(aes(V1, V2), df, color = "black",
                 contour_var = "ndensity", breaks = c(.1, .5, .9, .99)) +
  ## Color points
  geom_point(aes(info = info, color = maha_cross,
                 shape = maha_shape)) +
  ## Text points
  geom_text(aes(label = rowname), df[txt_pts_idx, ], color = "blue") +
  facet_grid(rows = vars(obs_type), cols = vars(var_space), scales = "free") +
  theme_bw() +
  theme(axis.text  = element_blank(),
        axis.ticks = element_blank()) +
  scale_color_continuous(name = "Normal \n Mahalonobis \n distances, \n crossed", 
                         type = "viridis") +
  scale_shape_discrete(name = "")


##### tab1_cheem ----
tab1_cheem <- tabPanel(title = "SHAP sensitivity -- FIFA", fluidPage(
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
           p("1) Take the original 42 attributes, hold wages (in Euros) as our target Y, aggregate correlated variables down to 8 X, skill attributes."),
           p("2) Remove the goalkeepers, (should be fit with different model), 9.3% of rows."),
           p("3) Create a Random Forest model predicting wages given our 8 skill attributes. (~12 sec)"),
           p("4) Extract the SHAP matrix, that is SHAP values for EACH observation (in-sample, obs of a random forest model, via {treeshap}, (~900 sec))."),
           p("5) Extract mMDS (~480 sec), pca, and mahalonobist dist for both data and SHAP values."),
           p("- Load above objects into shiny app; explore with ggplot2/plotly.")
    )
  ),
  
  ## main output row ----
  fluidRow(
    shiny::hr(),
    h3("FIFA 2020 Fielders"),
    h4("Explore sensitivity of the SHAP matrix against that the original data."),
    p("Highlighting points: click, or click and drag to select, double click to remove the selection."),
    p("Black contours: contours on projection density c(.1, ,.5, .9, .99)"),
    p("Colored points: players with top 2% maha distances in data or shap space."),
    p("Blue text: players with top 0.1% maha distances in data or shap space."),
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

