# FIFA regression, Interated nesting -----
## RF & treeshap

## Dependencies ------
require("DALEX")
require("spinifex")
require("ggplot2")
require("plotly") ## Linked brushing
##
require("tictoc")
require("dplyr")
require("beepr")
## Local files
source("./apps/cheem_classification/trees_of_cheem.r") ## Local functions, esp. for basis_cheem() and view_cheem()
if(F) ## Manually run to view file:
  file.edit("./apps/cheem_classification/trees_of_cheem.r")

## Setup ------
set.seed(303) ## DALEX Shap performance changed, we are using treeshap here.
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

## Local functions -----
## Normalized mahalonobis distances | given median, covar
maha_vect_of <- function(x, do_normalize = TRUE){ ## distance from median(x), cov(x)
  maha <- mahalanobis(x, apply(x, 2, median), cov(x)) %>%
    matrix(ncol = 1)
  if(do_normalize) maha <- spinifex::scale_01(maha)
  return(maha)
}
pca_df_of <- function(x, class, d = 2, do_normalize = TRUE){
  pca <- as.matrix(x) %*% spinifex::basis_pca(x, d = d) 
  if(do_normalize) pca <- spinifex::scale_01(pca) 
  return(as.data.frame(pca))
}
plot_df_of <- function(x, d = 2, model = NULL, layer_name){ ## uses maha/pca
  .maha <- maha_vect_of(x)
  .pca <- pca_df_of(x, class, d = d)
  if(is.null(model)){
    .resid <- NA
    .layer_ext <- layer_name
  }else{
    .resid <- Y - predict(model)
    .layer_ext <- paste0(layer_name, "\n SMSE: ", round(sum(model$mse), 1))
  }
  .qq_color <- colorRampPalette(c("grey", "red"))(100)[
    as.numeric(cut(.maha, breaks = 100))]
  .plot_df <- cbind(.pca, .maha, 1:nrow(X), Y, .layer_ext, "PCA",
                    .resid, .qq_color, order(.maha))
  names(.plot_df) <- c("V1", "V2", "maha_dist", "rownum", "species", "var_layer",
                       "view", "residual", "manual_qq_color", "idx_maha_ord")
  return(.plot_df)
}
### shap nesting function, and private functions/expressions -----
is.discrete <- function(x) ## See plyr::is.discrete().
  is.factor(x) || is.character(x) || is.logical(x)
legwork_expr <- expression({
  ## RF model
  sec_rf <- system.time({
    .m <- capture.output(gc())
    .rf <- randomForest::randomForest(Y~., data = data.frame(Y, X), mtry = .rf_mtry)
    print(.rf$confusion) ## Confusion matrix of rf model.
  })[3]
  ## treeshap
    sec_shap <- system.time({
      .m <- capture.output(gc())
      .shap <- treeshap_df(.rf, X)
    })[3]
  ## plot_df_of of shap
    sec_maha_pca <- system.time({
      .m <- capture.output(gc())
      .plot_df <- plot_df_of(.shap, d, .rf, layer_name)
    })[3]
})

shap_layer_of <- function(X, Y, layer_name = "UNAMED", d = 2){
  tic(paste0("shap_layer_of ", layer_name))
  eval(legwork_expr)
  ## Watching performance
  time_df <- data.frame(runtime_seconds = c(sec_rf, sec_shap, sec_maha_pca),
                        chunk = c("rf model", "rf treeshap shap", "maha/pca"),
                        layer = layer_name)
  toc()
  beepr::beep(1)
  return(list(plot_df = .plot_df,
              rf_model = .rf,
              shap_df = .shap,
              time_df = time_df))
}

## Formatting for consumtion, esp in shiny
nested_shap_layers <- function(X, Y, N_shap_layers = 3, d =2){
  loc_attr_nm <- "shap"
  
  ## data layer, plot_df only ----
  tic("Data layer, plot_df")
  data_plot_df <- plot_df_of(X, d = 2, model = NULL, layer_name = "data")
  toc()
  .next_layers_X <- list(X)
  ls_layer_lists <- list()
  for(i in 1:N_shap_layers){
    this_layer_nm <- paste0(loc_attr_nm, "^", i)
    ls_layer_lists[[i]] <- shap_layer_of(.next_layers_X, Y, clas, this_layer_nm)
  }
  
  ##TODO CONTINUE HERE>
  ### rbind plot_df -----
  b_plot_df <- data.frame(data_plot_df)
  .mute <- sapply(1:length(layer_lists_ls), function(i){
    this_plot_df <- layer_lists_ls[[i]]$plot_df
    b_plot_df <<- rbind(b_plot_df, this_plot_df)
  })
  
  ## Example of print sum or MSE
  model_ls <- list()
  .mute <- sapply(1:length(layer_lists_ls), function(i){
    model_ls[[i]] <- layer_lists_ls[[i]]$rf_model
    print(paste0(names(layer_lists_ls)[[i]], " SMSE: ", round(sum(model_ls[[i]]$mse), 1)))
  })
  
  ## Cbind decode table; I think just leave it to rownum, names, X, Y, pred1? shap values seem heavy
  decode_df <- data.frame(rownum = 1:nrow(X), Y)
  .mute <- sapply(1:length(layer_lists_ls), function(i){
    this_rf_model <- layer_lists_ls[[i]]$rf_model
    decode_df <<- cbind(decode_df, predict(this_rf_model))
  })
  decode_df <- cbind(decode_df, X)
  names(decode_df) <- c("rownum", "species", paste0("prediction_", names(layer_lists_ls)), names(X))
  beepr::beep(2)
}

## EXPORT OBJECTS ----
if(F){
  save(decode_df,
       b_plot_df,
       model_ls,
       file = "5regression_rf_dalex_nested_shaps.RData")
  file.copy("./5regression_rf_dalex_nested_shaps.RData", to = "./apps/cheem_classification/data/5regression_rf_dalex_nested_shaps.RData", overwrite = TRUE)
  file.remove("./5regression_rf_dalex_nested_shaps.RData")
}
if(F)
  load("./apps/cheem_classification/data/5regression_rf_dalex_nested_shaps.RData")



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
