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
  load("./apps/cheem_classification/data/1preprocess_rf_treeshap.RData")
  load("./apps/cheem_classification/data/2preprocess_rf_dalex.RData")
  load("./apps/cheem_classification/data/3preprocess_svm_dalex.RData")
  load("./apps/cheem_classification/data/4nested_rf_dalexshap.RData")
  ##
  file.edit("./apps/cheem_classification/1preprocess_rf_treeshap.r")
  file.edit("./apps/cheem_classification/2preprocess_rf_dalex.r")
  file.edit("./apps/cheem_classification/3preprocess_svm_dalex.r")
  file.edit("./apps/cheem_classification/4nested_rf_dalexshap.r")
}


## Load  -----
ls_dat_decode <- ls_bound_spaces_df <- ls_bound_qq_df <- list()
# load("./data/1preprocess_rf_treeshap.RData") ## objs: dat_decode, bound_spaces_df, bound_qq_df
file_nms <- c("rf_treeshap", "rf_dalex", "svm_dalex")
for(i in 1:length(file_nms)){
  .fp <- paste0("./data/", i, "preprocess_", file_nms[i], ".RData") ## local: ./apps/cheem_classification/*
  load(.fp)
  ls_dat_decode[[i]] <- dat_decode
  ls_bound_spaces_df[[i]] <- bound_spaces_df
  ls_bound_qq_df[[i]] <- bound_qq_df
}
names(ls_dat_decode) <- names(ls_bound_spaces_df) <- names(ls_bound_qq_df) <- file_nms

## Load nested shap
load("./data/4nested_rf_dalexshap.RData")
gg_nest_shap <- b_plot_df %>%
  plotly::highlight_key(~rownum) %>%
  ggplot(aes(V1, V2, rownum = rownum,
             color = sqrt(maha_dist), shape = species)) +
  ## Black Misclassified pts:
  geom_point(aes(V1, V2, rownum = rownum), 
             data = b_plot_df[b_plot_df$is_misclassified == TRUE, ],
             color = "black", size = 3) +
  geom_point() +
  # ## Density contours, .99, .5, .1, .01
  # geom_density2d(aes((V1, V2), color = "black",
  #                contour_var = "ndensity", breaks = c(.1, .5, .9)) +
  facet_grid(rows = vars(var_layer), cols = vars(view),
             scales = "free") +
  theme_bw() +
  theme(axis.text  = element_blank(),
        axis.ticks = element_blank()) +
  scale_color_gradient2(name = "sqrt mahalonobis \n distance, within layer",
                        low = "blue", mid = "grey", high = "red")
qq_nest_shap <- NULL

## plot expressions ------
## EXPRESSION to make plot
gg_expr <- expression({
  this_bound_spaces_df %>%
    highlight_key(~rownum) %>%
    ggplot(aes(V1, V2, rownum = rownum,
               color = maha_delta, shape = species)) +
    geom_point() +
    # ## Density contours, .99, .5, .1, .01
    # geom_density2d(aes(V1, V2), color = "black",
    #                contour_var = "ndensity", breaks = c(.1, .5, .9)) +
    facet_grid(rows = vars(obs_type), cols = vars(var_space)) +
    theme_bw() +
    theme(axis.text  = element_blank(),
          axis.ticks = element_blank()) +
    scale_color_gradient2(name = "Mahalonobis \n delta, shap - data",
                          low = "blue",mid = "grey",high = "red")
})
## EXPRESSION to make qq plot
qq_expr <- expression({
  manual_color <- colorRampPalette(c("blue", "grey", "red"))(100)[
    as.numeric(cut(this_bound_qq_df[input$color_var],breaks=100))]
  this_bound_qq_df %>%
    ggplot(aes(sample = y^(1/2))) +
    facet_grid(rows = vars(type)) +
    stat_qq() + stat_qq_line() +
    theme_bw() + 
    labs(x = "theoretical", y = "Square root of observations", title = "Q-Q plots, (square root)") +
    # scale_color_gradient2(name = "Mahalonobis \n delta, shap - data",
    #                       low = "blue",mid = "grey",high = "red") +
    theme(axis.text  = element_blank(),
          axis.ticks = element_blank())
})



##### tab1_cheem -----
tab1_cheem <- tabPanel(title = "linked brushing of SHAP- and data- spaces", fluidPage(
  ## Top input row -----
  fluidRow(
    p("- Data: palmer penguins, X: 4 continuous physical measurements, Classes: species, sex."),
    p("1) Remove NAs rows, remove most distant species."),
    p("2) Create a RF model predicting between remaining species."),
    p("3) Extract SHAP values of EACH observation, bring back to [nxp] observation space."),
    p("4) create maha spaces for the orginal and shap values"),
    p("- Load above objects into shiny app; Explore SHAP sensivity with ggplot2/plotly."),
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
    fluidRow(
      column(7,
             plotly::plotlyOutput("main_plot", width = "100%", height = "700px") %>%
               shinycssloaders::withSpinner(type = 8L)
      ), 
      column(5,
             shiny::plotOutput("qq_plot", width = "100%", height = "700px") %>%
               shinycssloaders::withSpinner(type = 8L)
      )
    )
  )
)) ## Assign tab1_cheem

## tab2_nested_shap ----
tab2_nested_shap <- tabPanel(title = "I heard you like SHAP...", fluidPage(
  ## Top input row -----
  fluidRow(
    p("- Data: palmer penguins, X: 4 continuous physical measurements, Classes: species, sex."),
    p("1) Remove NAs rows, remove most distant species."),
    p("2) Create a RF model predicting between remaining species."),
    p("3) Extract SHAP values of EACH observation, bring back to [nxp] observation space."),
    p("4) Repeat steps 1:3 on the output space!"),
  ),
  ## main output row ----
  fluidRow(
    shiny::hr(),
    h3("Palmer penguins"),
    p("Color by the square root of the mahalonobis distances, within layer."),
    p("Shape is the species of the penguin, the target variable of the model"),
    p("Click or click and drag to select points, double click to remove the selection."),
    p("Models are random forest, shap from DALEX."),
    plotly::plotlyOutput("main_plot2", width = "40%", height = "800px") %>%
      shinycssloaders::withSpinner(type = 8L),
    hr(),
    h4("Selected data:"),
    DT::DTOutput("selected_df2")
  )
)) ## Assign tab1_cheem


##### tabz_about -----
tabz_about <- tabPanel("About", fluidPage(
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
  p('(top) Wickham, H. & Grolemund, G. (2016). R for data science. ', a(href = 'https://r4ds.had.co.nz/', 'https://r4ds.had.co.nz/', .noWS = "outside"), '!', .noWS = c("after-begin", "before-end")),
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
                           tab2_nested_shap,
                           tabz_about),
                hr(),
                h4("Selected data:"),
                verbatimTextOutput("selected_plot_df"), ## What ggplotly sees
                DT::DTOutput("selected_df")
)

