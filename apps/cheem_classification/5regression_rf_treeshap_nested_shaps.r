## Creates objs for use in shiny app

## Dependencies ------
require("DALEX")
require("spinifex")
require("ggplot2")
require("plotly") ## Linked brushing
##
require("tictoc")
require("dplyr")
require("corrplot") 
## Local files
source("./apps/trees_of_cheem.r") ## Local functions, esp. for basis_cheem() and view_cheem()
if(F) ## Manually run to view file:
  file.edit("./apps/trees_of_cheem.r")

## Setup ------
## Data setup, palmerpenguins::penguins
raw <- palmerpenguins::penguins ## Missing values, visdat::vis_miss(raw)
raw_rmna <- raw[!is.na(raw$sex), ]
lvls <- levels(raw_rmna$species)
## Filter to closest 2 classes
raw_rmna <- raw_rmna[raw_rmna$species %in% lvls[1:2], ]
dat <- spinifex::scale_sd(raw_rmna[, 3:6]) %>% as.data.frame()
clas1 <- factor(raw_rmna$species, levels = lvls[1:2]) ## Manually remove 3rd lvl
clas2 <- raw_rmna$sex

## penguin regression
X <- dat
Y <- as.integer(clas1 == lvls[1])
## Split for train/test.
set.seed(303)
.idx_test <- sample(1:nrow(X), size = round(.2 * nrow(X))) ### HOLD OUT TEST DATA.
X_test  <- X[.idx_test,  ]
X_train <- X[-.idx_test, ]
Y_test  <- Y[ .idx_test]
Y_train <- Y[-.idx_test]

## Run nested shaps
formated_ls <- nested_shap_layers(X_train, Y_train,
                                  X_test, Y_test,
                                  n_shap_layers = 5)
## nested_shap_layers(): 9.22 sec elapsed
formated_ls$performance_df
names(formated_ls)


## visual expr ------
ggp_expr <- expression({ ## Expression to assigning gg and ggp.
  gg <- formated_ls$plot_df %>%
    plotly::highlight_key(~rownum) %>%
    ggplot(aes(V1, V2, rownum = rownum,
               color = (maha_dist))) +
    ## Black Mis-classified points:
    geom_point(aes(V1, V2, rownum = rownum),
               data = formated_ls$plot_df[formated_ls$plot_df$is_misclassified == TRUE,],
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
    scale_color_gradient2(name = "mahalonobis \n distance, within layer",
                          low = "blue", mid = "grey", high = "red")
  ## BOX SELECT
  ggp <- ggplotly(gg, tooltip = "rownum") %>% ## Tooltip by name of var name/aes mapping arg.
    config(displayModeBar = FALSE) %>% ## Remove html buttons
    layout(dragmode = "select") %>% ## Set drag left mouse to section box from zoom window
    event_register("plotly_selected") %>% ## Register based on "selected", on the release of th mouse button.
    highlight(on = "plotly_selected", off = "plotly_deselect")
})
if(F)
  eval(ggp_expr); ggp


qq_expr <- expression({
  qq <- formated_ls$plot_df %>%
    plotly::highlight_key(~rownum) %>%
    ggplot() +
    facet_grid(rows = vars(var_layer)) +
      geom_point(aes(x = quantile_theoretical,
                     y = quantile_maha_dist ^(1/2)),
                 color = formated_ls$plot_df$qq_color, stat="identity") +
    ggplot2::scale_color_identity() +
    # geom_segment(aes(x    = quantile(quantile_theoretical,   probs = .25, na.rm = T),
    #                  y    = quantile(quantile_maha_dist ^.5, probs = .25, na.rm = T),
    #                  xend = quantile(quantile_theoretical,   probs = .75, na.rm = T),
    #                  yend = quantile(quantile_maha_dist ^.5, probs = .75, na.rm = T))
    # ) +
    theme_bw() +
    labs(x = "Theoretical quantiles", y = "Square root of Maha distance", 
         title = "QQ plots, (square root maha dist)") +
    theme(axis.text  = element_blank(),
          axis.ticks = element_blank())
  # ggplotly(q, tooltip = "rownum") %>% ## Tooltip by name of var name/aes mapping arg.
  #   config(displayModeBar = FALSE) %>% ## Remove html buttons
  #   layout(dragmode = "select") %>% ## Set drag left mouse to section box from zoom window
  #   event_register("plotly_selected") %>% ## Register based on "selected", on the release of th mouse button.
  #   highlight(on = "plotly_selected", off = "plotly_deselect")
})

ggtime_expr <- expression({
  print("##TODO, ggtime_expr")
})

## EXPORT OBJECTS ----
if(F){
  formated_ls$model_ls <- NULL ## models are 97% of the size.
  save(formated_ls,
       ggp_expr,
       qq_expr, ## null
       ggtime_expr, ## null
       file = "5regression_rf_treeshap_nested_shaps.RData")
  file.copy("./5regression_rf_treeshap_nested_shaps.RData", to = "./apps/cheem_classification/data/5regression_rf_treeshap_nested_shaps.RData", overwrite = TRUE)
  file.remove("./5regression_rf_treeshap_nested_shaps.RData")
}
if(F)
  load("./apps/cheem_classification/data/5regression_rf_treeshap_nested_shaps.RData")



if(F){
  ## Mock-up visual ------
  str(b_plot_df)
  (g <- b_plot_df %>%
      plotly::highlight_key(~rownum) %>%
      ggplot(aes(V1, V2, rownum = rownum,
                 color = sqrt(maha_dist))) +
      ## Black Mis classified:
      geom_point(aes(V1, V2, rownum = rownum), 
                 data = b_plot_df[b_plot_df$is_misclassified == TRUE,],
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
  )
  
  ## BOX SELECT
  ggplotly(g, tooltip = "rownum") %>% ## Tooltip by name of var name/aes mapping arg.
    config(displayModeBar = FALSE) %>% ## Remove html buttons
    layout(dragmode = "select") %>% ## Set drag left mouse to section box from zoom window
    event_register("plotly_selected") %>% ## Register based on "selected", on the release of th mouse button.
    highlight(on = "plotly_selected", off = "plotly_deselect")
}
