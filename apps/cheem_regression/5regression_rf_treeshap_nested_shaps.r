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
source("./apps/cheem_classification/trees_of_cheem.r") ## Local functions, esp. for basis_cheem() and view_cheem()
if(F) ## Manually run to view file:
  file.edit("./apps/cheem_classification/trees_of_cheem.r")

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

## Local functions -----
## Normalized mahalonobis distances | given median, covar
maha_vect_of <- function(x, do_normalize = TRUE){ ## distance from median(x), cov(x)
  maha <- mahalanobis(x, apply(x, 2, median), cov(x)) %>%
    matrix(ncol = 1)
  if(do_normalize) maha <- spinifex::scale_01(maha)
  return(maha)
}
pca_df_of <- function(x, class, d = 2, do_normalize = TRUE){
  pca <- as.matrix(x) %*% spinifex::basis_pca(x, d)
  if(do_normalize) pca <- spinifex::scale_01(pca)
  return(as.data.frame(pca))
}
plot_df_of <- function(x, y, d = 2, model = NULL, layer_name){ ## consumes maha&pca
  .maha <- maha_vect_of(x)
  .pca <- pca_df_of(x, class, d)
  if(is.null(model)){
    .resid <- NA
    .layer_ext <- layer_name
  }else{
    .resid <- y - predict(model)
    .layer_ext <- paste0(layer_name, "\n SMSE: ", round(sum(model$mse), 1))
  }
  .qq_color <- colorRampPalette(c("grey", "red"))(100)[
    as.numeric(cut(.maha, breaks = 100))]
  .plot_df <- cbind(.pca, .maha, 1:nrow(x), y, .layer_ext, "PCA",
                    .resid, .qq_color, order(.maha))
  names(.plot_df) <- c("V1", "V2", "maha_dist", "rownum", "species", "var_layer",
                       "view", "residual", "manual_qq_color", "idx_maha_ord")
  return(.plot_df)
}
### shap nesting function, and private functions/expressions -----

### One shap layer
shap_layer_of <- function(x, y, layer_name = "UNAMED", d = 2,
                          verbose = TRUE, noisy = TRUE){
  if(verbose == TRUE) tictoc::tic(paste0("shap_layer_of ", layer_name))
  .is_y_disc <- is.factor(y) || is.character(y) || is.logical(y)
  
  ## RF model
  sec_rf <- system.time({
    .m <- capture.output(gc())
    .hp_mtry <- if(.is_y_disc == TRUE) sqrt(ncol(x)) else ncol(x) / 3L
    .hp_node <- max(if(.is_y_disc == TRUE) 1 else 5, ceiling(nrow(x) / 500))
    .rf <- randomForest::randomForest(y~., data = data.frame(y, x),
                                      mtry = .hp_mtry, nodesize = .hp_node)
  })[3]
  ## treeshap
  sec_shap <- system.time({
    .m <- capture.output(gc())
    .shap <- treeshap_df(.rf, x)
  })[3]
  ## plot_df_of shap
  sec_maha_pca <- system.time({
    .m <- capture.output(gc())
    .plot_df <- plot_df_of(.shap, y, d, .rf, layer_name)
  })[3]
  
  ## Watching performance
  time_df <- data.frame(runtime_seconds = c(sec_rf, sec_shap, sec_maha_pca),
                        chunk = c("rf model", "(rf) treeshap shap", "maha/pca"),
                        layer = layer_name)
  
  if(verbose == TRUE) tictoc::toc()
  if(noisy == TRUE) beepr::beep(1)
  return(list(plot_df = .plot_df,
              rf_model = .rf,
              shap_df = .shap,
              time_df = time_df))
}

### Format many shap layers to usable df list.
format_nested_layers <- function(shap_layer_ls, x, y){
  sec_data_plot_df <- system.time({
    .m <- capture.output(gc())
    data_plot_df <- plot_df_of(x, y, d = 2, model = NULL, layer_name = "data")
  })[3]
  ### rbind plot_df -----
  b_plot_df <- data.frame(data_plot_df)
  .mute <- sapply(1:length(shap_layer_ls), function(i){
    this_plot_df <- shap_layer_ls[[i]]$plot_df
    b_plot_df <<- rbind(b_plot_df, this_plot_df)
  })
  
  ## performance of the layers
  .nms <- names(shap_layer_ls)
  performance_df <- data.frame(NULL)
  model_ls <- list()
  .mute <- sapply(1:length(shap_layer_ls), function(i){
    this_model <- shap_layer_ls[[i]]$rf_model
    model_ls[[i]] <<- this_model
    performance_df <<- rbind(
      performance_df,
      data.frame(.nms[i],
                 median(this_model$mse),
                 median(sqrt(this_model$mse)),
                 median(this_model$rsq),
                 sum(shap_layer_ls[[i]]$time_df$runtime_seconds))
    )
  })
  names(model_ls) <- names(shap_layer_ls)
  colnames(performance_df) <- c("layer", "median_mse",
                                "median_rmse", "median_rsq", "runtime_seconds")
  
  ## Cbind decode table
  decode_df <- data.frame(rownum = 1:nrow(x), y)
  .mute <- sapply(1:length(shap_layer_ls), function(i){
    this_rf_model <- shap_layer_ls[[i]]$rf_model
    decode_df <<- cbind(decode_df, y - predict(this_rf_model))
  })
  decode_df <- cbind(decode_df, x)
  names(decode_df) <- c("rownum", "y", paste0("residual_", names(shap_layer_ls)), names(x))
  
  ## Cbind time_df
  b_time_df <- data.frame(runtime_seconds = sec_data_plot_df,
                          chunk = "maha/pca", layer = "data")
  .mute <- sapply(1:length(shap_layer_ls), function(i){
    b_time_df <<- rbind(b_time_df, shap_layer_ls[[i]]$time_df)
  })
  
  if(verbose == TRUE) tictoc::toc()
  return(list(plot_df = b_plot_df,
              decode_df = decode_df,
              performance_df = performance_df,
              time_df = b_time_df,
              model_ls = model_ls))
}


## Create many layers and format
nested_shap_layers <- function(x, y, n_shap_layers = 3, x_test, d = 2,
                               verbose = TRUE, noisy = TRUE){
  loc_attr_nm <- "shap"
  if(verbose == TRUE) {
    print(paste0("nested_shap_layers() started at ", Sys.time()))
    tictoc::tic("nested_shap_layers()")
  }
  
  ## Create shap layers in a list -----
  ### Initialize
  .next_layers_x <- x
  shap_layer_ls <- list()
  layer_nms <- paste0(loc_attr_nm, "^", 1:n_shap_layers)
  layer_runtimes <- c(NULL)
  .mute <- sapply(1:n_shap_layers, function(i){
    shap_layer_ls[[i]] <<- shap_layer_of(.next_layers_x, y, layer_nms[i], d,
                                         verbose, noisy)
    .next_layers_x <<- shap_layer_ls[[i]]$shap_df
    if(verbose == TRUE & i != n_shap_layers){
      layer_runtimes[i] <- sum(shap_layer_ls[[i]]$time_df$runtime_seconds)
      est_seconds_remaining <- round(sum(layer_runtimes) * n_shap_layers / 1)
      print(paste0("Estimated seconds of runtime remaining: ", est_seconds_remaining,
                   ". Estimated completion time: ", round(Sys.time() + est_seconds_remaining)
      ))
    }
  })
  names(shap_layer_ls) <- layer_nms
  
  ## Format into more usable dfs rather than layer lists
  formated <- format_nested_layers(shap_layer_ls, x, y)
  
  if(noisy == TRUE) beepr::beep(2)
  return(formated)
}

## FOR TESTING ##
#### @examples
#' X_train <- tourr::flea[, 2:6]
#' Y <- tourr::flea[, 1]
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
formated_ls$time_df
names(formated_ls$model_ls)
## performance doesn't seem to be commensurate with the performance I create manually

### Validate against test data:
#model_ls = formated_ls$model_ls; x = X_test; y = Y_test;
model_ls_performance <- function(model_ls, x, y){
  
  performance_df <- data.frame(NULL)
  .mute <- sapply(1:length(model_ls), function(i){
    resid_vect <- 
      rss <- sum((y - predict(model_ls[[i]], x))^2)
    tss <- sum((y - mean(y))^2)
    rsq <- 1 - (tss/rss)
    new_row <- data.frame(
      .nms[i], mean(rss), sqrt(mean(rss)), rsq)
    performance_df <<- rbind(performance_df, new_row)
  })
  colnames(performance_df) <- c("layer", "mse", "rmse", "rsq")
  
  return(performance_df)
}
perf_train <- model_ls_performance(formated_ls$model_ls, x = X_train, y = Y_train)
perf_test  <- model_ls_performance(formated_ls$model_ls, x = X_test,  y = Y_test)
perf_train
perf_test 

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

