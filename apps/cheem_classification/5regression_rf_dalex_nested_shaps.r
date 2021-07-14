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

set.seed(303)
## Setup data ------
dat <- tourr::flea[,-7]
if(F)
  corrplot::corrplot(cor(dat))
## FLEA regression
X <- dat[, -5]
Y <- dat[, 5]
clas <- tourr::flea$species

## Local functions -----
## Normalized mahalonobis distances | given median, covar
maha_vect_of <- function(x, do_normalize = TRUE){ ## dist from in-class column median(x), cov(x)
  maha <- mahalanobis(x, apply(x, 2, median), cov(x)) %>%
    matrix(ncol = 1)
  if(do_normalize) maha <- spinifex::scale_01(maha)
  return(maha)
}
olda_df_of <- function(x, class, d = 2, do_normalize = TRUE){
  olda <- as.matrix(x) %*% spinifex::basis_olda(x, class, d = d) 
  if(do_normalize) olda <- spinifex::scale_01(olda) 
  return(as.data.frame(olda))
}
plot_df_of <- function(x, class, d = 2, model = NULL, layer_name){ ## uses maha/ olda
  .maha <- maha_vect_of(x)
  .olda <- olda_df_of(x, class, d = d)
  if(is.null(model)){
    .resid <- NA
    .layer_ext <- layer_name
  }else{
    .resid <- Y - predict(model)
    .layer_ext <- paste0(layer_name, "\n SMSE: ", round(sum(model$mse), 1))
  }
  .qq_color <- colorRampPalette(c("grey", "red"))(100)[
    as.numeric(cut(.maha, breaks = 100))]
  .plot_df <- cbind(.olda, .maha, 1:nrow(X), Y, .layer_ext, "oLD",
                    .resid, .qq_color, order(.maha))
  names(.plot_df) <- c("V1", "V2", "maha_dist", "rownum", "species", "var_layer",
                       "view", "residual", "manual_qq_color", "idx_maha_ord")
  return(.plot_df)
}
### shap nesting function -----
if(is.discrete(Y)) .rf_mtry <- sqrt(ncol(X)) else .rf_mtry <- ncol(X) / 3
shap_layer_of <- function(X, Y, clas, layer_name = "UNAMED", d = 2){ ## ASSUMES X, Y, 
  tic(paste0("shap_layer_of ", layer_name))
  ### RF model -----
  rf_expr <- expression({
    sec_rf <- system.time({
      gc()
      .rf <- randomForest::randomForest(Y~., data = data.frame(Y, X), mtry = .rf_mtry)
      print(.rf$confusion) ## Confusion matrix of rf model.
    })[3]
  })
  ### DALEX shap -----
  dalex_shap_expr <- expression({
    sec_shap <- system.time({
      gc()
      .shap <- local_attribution_df(X, Y, .rf)
    })[3]
  })
  ### plot_df_of of shap -----
  maha_n_olda_expr <- expression({
    sec_maha_olda <- system.time({
      gc()
      .plot_df <- plot_df_of(.shap, clas, d, .rf, layer_name)
    })[3]
  })
  
  eval(rf_expr)
  eval(dalex_shap_expr)
  eval(maha_n_olda_expr)
  
  time_df <- data.frame(runtime_seconds = c(sec_rf, sec_shap, sec_maha_olda),
                        chunk = c("rf model", "dalex shap", "maha/olda"),
                        layer = layer_name)
  toc()
  beepr::beep(1)
  return(list(plot_df = .plot_df,
              rf_model = .rf,
              shap_df = .shap, 
              time_df = time_df))
}

{
  ## data layer, plot_df only ----
  tic("Data layer, plot_df")
  data_layer <- plot_df_of(X, clas, d = 2, model = NULL, layer_name = "data")
  toc()
  str(data_layer)
  
  ## NEEDS ITERATION
  ## first Shap level from function
  shap1_ls <- shap_layer_of(X, Y, clas, "shap^1")
  shap2_ls <- shap_layer_of(shap1_ls$shap_df, Y, clas, "shap^2")
  shap3_ls <- shap_layer_of(shap2_ls$shap_df, Y, clas, "shap^3")
  shap4_ls <- shap_layer_of(shap3_ls$shap_df, Y, clas, "shap^4")
  
  
  ## NEEDS ITERATION
  layer_lists_ls <- list(shap1_ls = shap1_ls,
                         shap2_ls = shap2_ls,
                         shap3_ls = shap3_ls,
                         shap4_ls = shap4_ls)
  
  ### rbind plot_df -----
  b_plot_df <- data.frame(data_layer)
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
