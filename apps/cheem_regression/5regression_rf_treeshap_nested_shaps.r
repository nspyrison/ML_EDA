# FIFA regression, Iterated nesting -----
## RF & treeshap

## Dependencies ------
require("DALEX")
require("spinifex")
require("ggplot2")
require("plotly") ## Linked brushing
##
require("dplyr")
require("tictoc")
require("beepr")
## Local files
source("./apps/trees_of_cheem.r") ## Local functions, esp. for basis_cheem() and view_cheem()
if(F) ## Manually run to view file:
  file.edit("./apps/trees_of_cheem.r")


## FOR TESTING ##
#' @examples
#' X <- tourr::flea[, 2:6]
#' Y <- tourr::flea[, 1]
#' set.seed(303)
#' .idx_test <- sample(1:nrow(X), size = round(.2 * nrow(X))) ### HOLD OUT TEST DATA.
#' X_test  <- X[.idx_test,  ]
#' X_train <- X[-.idx_test, ]
#' Y_test  <- Y[ .idx_test]
#' Y_train <- Y[-.idx_test]
#' formated_ls <- nested_shap_layers(X_train, Y_train,
#'                                   X_test, Y_test)
#' formated_ls$performance_df
#' 
#' ## with "olda"
#' print("Rdimtools::do.olda() not working atm.")
#' # formated_ls <- nested_shap_layers(X_train, Y_train,
#' #                                   X_test, Y_test,
#' #                                   basis_type = "olda",
#' #                                   class = tourr::flea$species)
#' # formated_ls$performance_df


## Setup ------
.raw <- DALEX::fifa
.scaled <- .raw %>%
  dplyr::select(-c(`nationality`, ## useless class
                   `overall`, `potential`, `value_eur`, `wage_eur`)) %>% ## potential target vars.
  spinifex::scale_01() %>% as.data.frame()

if(F) ## View corrplot?
  corrplot::corrplot(cor(.scaled), 
                     method = "circle", ## geom
                     type = "upper", ## only upper triangle
                     diag = F, ## remove auto correlation
                     order = "FPC", ## First principal component
                     tl.col = "black", tl.srt = 90, ## Text label color and rotation
                     tl.pos = "td")

### Munging -----
## Agg some highly correlated vars.
dat <- .scaled %>% dplyr::mutate(
  .keep = "none",
  bdy = (weight_kg+height_cm)/2, ## bmi wasn't working well after 01 scaling.
  age = age,
  react = movement_reactions,
  atk = (attacking_finishing+skill_long_passing+attacking_volleys+
           power_long_shots+skill_curve+mentality_positioning+attacking_crossing+
           attacking_short_passing+skill_dribbling+skill_ball_control)/10,
  def = (defending_sliding_tackle+mentality_interceptions+
           defending_standing_tackle+defending_marking+mentality_aggression)/5,
  acc = (attacking_heading_accuracy+power_shot_power)/2,
  mvm = (movement_sprint_speed+movement_balance+movement_acceleration+
           mentality_vision+mentality_composure+movement_agility+
           mentality_penalties+skill_fk_accuracy+power_stamina+movement_reactions)/10,
  pwr = (power_strength+power_jumping)/2,
  gk = (goalkeeping_diving+goalkeeping_positioning+goalkeeping_reflexes+
          goalkeeping_handling+goalkeeping_kicking)/5,
)
## Starting with 42 variables, we remove `nationality`, and some potential Y vars,
#### and aggregate into 9 aggregate 'aspect' dimensions based on var correlation 

## Filter and remove Goal keepers to be on the safe side.
#dat_gk  <- dat[dat$gk >= .5, ] ## 467 obs, 9.34%
.idx_is_fld <- dat$gk < .5 ## remaining 91%, fielders
X <- dat_fld <- scale_01(dat[.idx_is_fld, -9]) ## scale again after removing.
Y <- tgt_var <- .raw$wage_eur[.idx_is_fld] ## Unscaled wages of the fielders
set.seed(303)
.idx_test <- sample(1:nrow(X), size = round(.2 * nrow(X))) ### HOLD OUT TEST DATA.
X_test  <- X[.idx_test,  ]
X_train <- X[-.idx_test, ]
Y_test  <- Y[ .idx_test]
Y_train <- Y[-.idx_test]

### RUN SHAP LAYERS

## FOR TESTING ##
#' @examples
#' X <- tourr::flea[, 2:6]
#' Y <- tourr::flea[, 1]
#' set.seed(303)
#' .idx_test <- sample(1:nrow(X), size = round(.5 * nrow(X))) ### HOLD OUT TEST DATA.
#' X_test  <- X[.idx_test,  ]
#' X_train <- X[-.idx_test, ]
#' Y_test  <- Y[ .idx_test]
#' Y_train <- Y[-.idx_test]
#' x <- X_train; y = Y_train; x_test = X_test; y_test = Y_test;
################=

if(F) ## Not run auto, ~32 min process::
  formated_ls <- nested_shap_layers(X_train, Y_train) ## ~ 3 x 16 min ~ 48 min.
### Fifa, 80% training data
# shap_layer_of shap^1: 674.57 sec elapsed
# shap_layer_of shap^2: 616.25 sec elapsed
# shap_layer_of shap^3: 621.67 sec elapsed

names(formated_ls)
formated_ls$plot_df
formated_ls$decode_df
formated_ls$performance_df
# formated_ls$performance_df2
formated_ls$time_df
##names(formated_ls$model_ls)
## performance doesn't seem to be commensurate with the performance I create manually


## visual expr ------
ggp_expr <- expression({ ## Expression to assigning gg and ggp.
  gg <- formated_ls$plot_df %>%
    plotly::highlight_key(~rownum) %>%
    ggplot(aes(V1, V2, rownum = rownum,
               color = sqrt(maha_dist))) +
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
    scale_color_gradient2(name = "sqrt mahalonobis \n distance, within layer",
                          low = "blue", mid = "grey", high = "red")
  ## BOX SELECT
  ggp <- ggplotly(gg, tooltip = "rownum") %>% ## Tooltip by name of var name/aes mapping arg.
    config(displayModeBar = FALSE) %>% ## Remove html buttons
    layout(dragmode = "select") %>% ## Set drag left mouse to section box from zoom window
    event_register("plotly_selected") %>% ## Register based on "selected", on the release of th mouse button.
    highlight(on = "plotly_selected", off = "plotly_deselect")
})
if(F){
  eval(ggp_expr); ggp
}

qq_expr <- expression({
  print("##TODO, qq_expr")
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
       file = "5regression_rf_dalex_nested_shaps.RData")
  file.copy("./5regression_rf_dalex_nested_shaps.RData", to = "./apps/cheem_regression/data/5regression_rf_dalex_nested_shaps.RData", overwrite = TRUE)
  file.remove("./5regression_rf_dalex_nested_shaps.RData")
}
if(F)
  load("./apps/cheem_regression/data/5regression_rf_dalex_nested_shaps.RData")

