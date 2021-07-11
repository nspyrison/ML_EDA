## Creates objs for use in shiny app

## Dependencies ------
require("DALEX")
require("spinifex")
require("tourr")
require("ggplot2")
require("tictoc")
require("dplyr")
##
require("palmerpenguins") ## data
require("plotly") ## Linked brushing
require("microbenchmark")
## Local files
source("./apps/cheem_classification/trees_of_cheem.r") ## Local functions, esp. for basis_cheem() and view_cheem()
if(F) ## Manually run to view file:
  file.edit("./apps/cheem_classification/trees_of_cheem.r")


## Setup data ------
## Data setup, palmerpenguins::penguins
raw <- palmerpenguins::penguins ## Missing values, visdat::vis_miss(raw)
raw_rmna <- raw[!is.na(raw$sex), ]
lvls <- levels(raw_rmna$species)
## Filter to closest 2 classes
raw_rmna <- raw_rmna[raw_rmna$species %in% lvls[1:2], ]
dat <- spinifex::scale_sd(raw_rmna[, 3:6]) %>% as.data.frame()
clas1 <- factor(raw_rmna$species, levels = lvls[1:2]) ## Manually remove 3rd lvl
clas2 <- raw_rmna$sex


## Local functions -----
## Normalized mahalonobis distances | given median, covar
maha_vect_of <- function(x, do_normalize = TRUE){ ## dist from in-class column median(x), cov(x)
  maha <- mahalanobis(x, apply(x, 2, median), cov(x)) %>%
    matrix(ncol = 1)
  if(do_normalize) maha <- spinifex::scale_01(maha)
  return(maha)
}
olda_df_of  <- function(x, class, d = 2, do_normalize = TRUE){
  olda <- as.matrix(x) %*% spinifex::basis_olda(x, class, d = d) 
  if(do_normalize) olda <- spinifex::scale_01(olda) 
  return(as.data.frame(olda))
}
plot_df_of <- function(x, clas, d = 2){ ## uses maha/ olda
  .maha <- maha_vect_of(x)
  .olda <- olda_df_of(x, clas, d = d)
  .is_misclas <- predict(.rf) != Y
  .qq_color <- colorRampPalette(c("grey", "red"))(100)[
    as.numeric(cut(.maha, breaks = 100))]
  .plot_df <- cbind(.olda, .maha, 1:nrow(X), Y, layer_name, "oLD",
                    .is_misclas, .qq_color, order(.maha))
  names(.plot_df) <- c("V1", "V2", "maha_dist", "rownum", "species", "var_layer",
                       "view", "is_misclassified", "manual_qq_color", "idx_maha_ord")
}

### shap nesting function -----
shap_layer_of <- function(X, Y, layer_name = "UNAMED", d = 2){ ## ASSUMES X, Y, 
  tic(paste0("shap_layer_of ", layer_name))
  ### RF model -----
  rf_expr <- expression({
    sec_rf <- system.time({
      gc()
      if(is.discrete(Y)) .rf_mtry <- sqrt(ncol(X)) else .rf_mtry <- ncol(X) / 3
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
  ### maha and olda of shap -----
  maha_n_olda_expr <- expression({
    sec_maha_olda <- system.time({
      gc()
      .plot_df  <- plot_df_of(.shap, Y, d)
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


## data layer, plot_df only -----
tic("Data layer, plot_df")
X <- dat
Y <- clas1
layer_name <- "data"
data_plot_df  <- plot_df_of(X, Y, 2)
toc()
str(data_plot_df)

## NEEDS ITERATION
## first Shap level from function
shap1_ls <- shap_layer_of(X, Y, "shap^1")
shap2_ls <- shap_layer_of(shap1_ls$shap_df, Y, "shap^2")
shap3_ls <- shap_layer_of(shap2_ls$shap_df, Y, "shap^3")
shap4_ls <- shap_layer_of(shap3_ls$shap_df, Y, "shap^4")
beepr::beep(2)

## NEEDS ITERATION
layer_lists_ls <- list(shap1_ls = shap1_ls,
                       shap2_ls = shap2_ls,
                       shap3_ls = shap3_ls,
                       shap4_ls = shap4_ls)

### rbind plot_df -----
b_plot_df <- data.frame(data_plot_df)
.mute <- sapply(1:length(layer_lists_ls), function(i){
  this_plot_df <- layer_lists_ls[[i]]$plot_df
  b_plot_df <<- rbind(b_plot_df, this_plot_df)
})

## Example of print confusion matrix
model_ls <- list()
.mute <- sapply(1:length(layer_lists_ls), function(i){
  model_ls[[i]] <- layer_lists_ls[[i]]$rf_model
  print(paste0(names(layer_lists_ls)[[i]], ":"))
  print(model_ls[[i]]$confusion)
})

## Cbind decode table; I think just leave it to rownum, names, X, Y, pred1? shap values seem heavy
decode_df <- data.frame(rownum = 1:nrow(X), Y)
.mute <- sapply(1:length(layer_lists_ls), function(i){
  this_rf_model <- layer_lists_ls[[i]]$rf_model
  decode_df <<- cbind(decode_df, predict(this_rf_model))
})
decode_df <- cbind(decode_df, X)
names(decode_df) <- c("rownum", "species", paste0("prediction_", names(layer_lists_ls)), names(X))



## EXPORT OBJECTS ----
if(F){
  save(decode_df,
       b_plot_df,
       model_ls,
       file = "4nested_rf_dalexshap.RData")
  file.copy("./4nested_rf_dalexshap.RData", to = "./apps/cheem_classification/data/4nested_rf_dalexshap.RData", overwrite = TRUE)
  file.remove("./4nested_rf_dalexshap.RData")
}
if(F)
  load("./apps/cheem_classification/data/4nested_rf_dalexshap.RData")



if(F){
  ## Mock-up visual ------
  str(b_plot_df)
  g <- b_plot_df %>%
    plotly::highlight_key(~rownum) %>%
    ggplot(aes(V1, V2, rownum = rownum,
               color = sqrt(maha_dist), shape = species)) +
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
  
  ## BOX SELECT
  ggplotly(g, tooltip = "rownum") %>% ## Tooltip by name of var name/aes mapping arg.
    config(displayModeBar = FALSE) %>% ## Remove html buttons
    layout(dragmode = "select") %>% ## Set drag left mouse to section box from zoom window
    event_register("plotly_selected") %>% ## Register based on "selected", on the release of th mouse button.
    highlight(on = "plotly_selected", off = "plotly_deselect")
}
