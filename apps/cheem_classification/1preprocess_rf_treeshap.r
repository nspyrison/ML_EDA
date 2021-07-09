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
require("caret") ## One hot encoding class.
require("plotly") ## Linked brushing
## Local files
source("./apps/cheem_classification/trees_of_cheem.r") ## Local functions, esp. for basis_cheem() and view_cheem()
if(F) ## Manually run to view file:
  file.edit("./apps/cheem_classification/trees_of_cheem.r")


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
if(F){
  table(clas1) ## Unbalanced island obs
  table(clas2) ## Balanced sex
}

## Random forest model {randomForest} -----
.p <- ncol(dat)
.rf_mtry <- if(is.discrete(clas1)) sqrt(.p) else .p / 3L
.lvls <- levels(clas1)
i <- 1
tic("RF fit")
## Boolean test for treeshap, :()
test1 <- as.integer(clas1 == .lvls[i])
rf1 <- randomForest::randomForest(test1~.,
                                      data = data.frame(test1, dat),
                                      mtry = .rf_mtry,
                                      do.trace = TRUE)

toc() ## .22 sec
pred <- predict(rf1, newdata = dat) ## newdata is only Xs
pred_clas <- .lvls[2 - as.integer(pred >= .5)]
resid <- test1 - pred
table(pred_clas, clas1)



## shap_df {treeshap} ------
gc()
tic("treeshap")
shap_df <- treeshap_df(rf_bool, dat)
toc() ## 1.3 sec

## class RF for correct measures
rf_clas <- randomForest::randomForest(clas1~.,
                                      data = data.frame(clas1, dat),
                                      mtry = .rf_mtry,
                                      do.trace = TRUE)
rf_clas$confusion


## Normalized mahalonobis distances (median, covar) ----
maha_vect_of <- function(x, do_normalize = TRUE){ ## dist from in-class column median(x), cov(x)
  maha <- mahalanobis(x, apply(x, 2L, median), cov(x)) %>%
    matrix(ncol = 1)
  if(do_normalize) maha <- spinifex::scale_01(maha) 
  return(maha)
}
maha_dat   <- maha_vect_of(dat)
maha_shap  <- maha_vect_of(shap_df)
maha_delta <- maha_shap - maha_dat
hist(maha_delta) ## Not as right skewed as the regression; artifact of wages?
maha_color <- maha_delta
maha_shape <- factor(maha_delta >= 0,
                     levels = c(FALSE, TRUE),
                     labels = c("maha SHAP larger", "maha data larger"))

## Create view space ------
# ### PCA
# pca_dat <- as.matrix(dat) %*% spinifex::basis_pca(dat) %>%
#   scale_01() %>% as.data.frame() %>% cbind(1:.n, "data", "PCA")
# pca_shap <- as.matrix(shap_df) %*% spinifex::basis_pca(shap_df) %>%
#   scale_01() %>% as.data.frame() %>% cbind(1:.n, "shap", "PCA")

### oLDA
olda_dat  <- as.matrix(dat) %*% spinifex::basis_olda(dat, clas1) %>%
  scale_01() %>% as.data.frame() %>% cbind(1:.n, "data", "oLD")
olda_shap <- as.matrix(shap_df) %*% spinifex::basis_olda(shap_df, clas1) %>%
  scale_01() %>% as.data.frame() %>% cbind(1:.n, "shap", "oLD")


### maha cross
bolda_dat  <- cbind(olda_dat,  maha_shap)
bolda_shap <- cbind(olda_shap, maha_dat)

## combine
names(bolda_dat) <- names(bolda_shap) <- c(paste0("V", 1:2), "rownum", "obs_type", "var_space", "maha_cross")
bound_spaces_df <- rbind(bolda_dat,
                         bolda_shap)
beepr::beep(4)
## Add replicated classes
.nn <- nrow(bound_spaces_df)
bound_spaces_df$species    <- rep_len(clas1, .nn)
bound_spaces_df$sex        <- rep_len(clas2, .nn)
bound_spaces_df$maha_delta <- rep_len(maha_delta, .nn)
bound_spaces_df$residual   <- rep_len(resid, .nn)

## reconstruct dat with features
dat_decode <- data.frame(1:nrow(dat),
                         round(maha_dat, 2),
                         round(maha_shap, 2),
                         clas1,
                         pred_clas,
                         round(pred, 2),
                         round(resid, 2),
                         clas2,
                         round(dat, 2))
colnames(dat_decode) <- c("rownum", "maha_dist_dat", "maha_dist_shap",
                          "obs_species", "pred_species", "prediction",
                          "residual", "sex", colnames(dat))

## qq df
.n <- nrow(dat)
bound_qq_df <- data.frame(rownum = rep(1:nrow(dat), 2),
                          y = c(maha_dat, maha_shap),
                          maha_dat = rep(maha_dat, 2),
                          maha_shap = rep(maha_shap, 2),
                          maha_delta = rep(maha_delta, 2),
                          manual_color = colorRampPalette(c("blue", "grey", "red"))(100)[
                            as.numeric(cut(maha_delta,breaks=100))],
                          type = c(rep("maha(data)", .n), rep("maha(shap)", .n)))


## EXPORT OBJECTS ----
if(F){
  save(dat_decode,
       bound_spaces_df,
       bound_qq_df,
       file = "1preprocess_rf_treeshap.RData")
  file.copy("./1preprocess_rf_treeshap.RData", to = "./apps/cheem_classification/data/1preprocess_rf_treeshap.RData", overwrite = TRUE)
  file.remove("./1preprocess_rf_treeshap.RData")
}
if(F)
  load("./apps/cheem_classification/data/1preprocess_rf_treeshap.RData")


## Experimental shapshap -----
## WHAT IF WE FIT A MODEL ON SHAP SPACE?!

## I don't trust flipping back and forth between the factor and boolean test, 
##let's stick with dalex more reliable comparison:
rf_clas$confusion ## confusion on data space.

gc()
tic("dalex LA")
shap_df2 <- local_attribution_df(dat, clas1, rf_clas)
toc() ## 41.25 sec

## class RF ON SHAP SPACE
rf_clas_shap <- randomForest::randomForest(clas1~.,
                                           data = data.frame(clas1, shap_df2),
                                           mtry = .rf_mtry,
                                           do.trace = TRUE)
rf_clas_shap$confusion ## BETTER PERFORMANCE!

## Lets see if we can repeat for 100% accuracy
gc()
tic("dalex LA on shap space")
shapshap_df <- local_attribution_df(shap_df2, clas1, rf_clas_shap)
toc() ## 41.25 sec
rf_clas_shapshap <- randomForest::randomForest(clas1~.,
                                               data = data.frame(clas1, shapshap_df),
                                               mtry = .rf_mtry,
                                               do.trace = TRUE)
rf_clas_shapshap$confusion ##  no better than performance on rf_clas_shap.
classification_report(y_test, y_pred_test)
classification_report(y_test, y_pred_test)

##
olda_shapshap <- as.matrix(shapshap_df) %*% spinifex::basis_olda(shapshap_df, clas1) %>%
  scale_01() %>% as.data.frame() %>% cbind(1:.n, "shapshap", "oLD")


bolda_shapshap <- data.frame(olda_shapshap, 
                             maha_delta)
names(bolda_shapshap) <- c(paste0("V", 1:2), "rownum", "obs_type", "var_space", "MAHA_DELTA")


if(F){ ## QQ mockup
  idx <- order(bound_qq_df[, "maha_delta"][1:(nrow(bound_qq_df) / 2)])
  bound_qq_df %>%
    highlight_key(~rownum) %>% 
    ggplot(aes(sample = y^(1/2), )) +
    facet_grid(rows = vars(type)) +
    geom_qq(color = manual_color[rep(idx, 2)]) + geom_qq_line() +
    theme_bw() +
    labs(x = "theoretical", y = "Square root of observations", title = "Q-Q plots, (square root)") +
    theme(axis.text  = element_blank(),
          axis.ticks = element_blank())
  # ggplotly(q, tooltip = "rownum") %>% ## Tooltip by name of var name/aes mapping arg.
  #   config(displayModeBar = FALSE) %>% ## Remove html buttons
  #   layout(dragmode = "select") %>% ## Set drag left mouse to section box from zoom window
  #   event_register("plotly_selected") %>% ## Register based on "selected", on the release of th mouse button.
  #   highlight(on = "plotly_selected", off = "plotly_deselect")
}

if(F){
  ## Mock-up visual ------
  require("ggplot2")
  tic("prep ggplot ")
  str(bound_spaces_df)
  
  
  g <- bound_spaces_df %>%
    highlight_key(~rownum) %>% 
    ggplot(aes(V1, V2, rownum = rownum,
               color = maha_delta, shape = species)) +
               #color = species, shape = sex)) +
    geom_point() +
    # ## Density contours, .99, .5, .1, .01
    # geom_density2d(aes(V1, V2), color = "black",
    #                contour_var = "ndensity", breaks = c(.1, .5, .9)) +
    facet_grid(rows = vars(obs_type), cols = vars(var_space)) +
    theme_bw() +
    theme(axis.text  = element_blank(),
          axis.ticks = element_blank()) +
    scale_color_gradient2(name = "Mahalonobis \n delta, shap - data",
                         low = "blue", mid = "grey", high = "red")
  
  ## BOX SELECT
  ggplotly(g, tooltip = "rownum") %>% ## Tooltip by name of var name/aes mapping arg.
    config(displayModeBar = FALSE) %>% ## Remove html buttons
    layout(dragmode = "select") %>% ## Set drag left mouse to section box from zoom window
    event_register("plotly_selected") %>% ## Register based on "selected", on the release of th mouse button.
    highlight(on = "plotly_selected", off = "plotly_deselect")
}



## Other Selection options
if(F){
  ## BOX SELECT
  ggplotly(g, tooltip = "rownum") %>% ## Tooltip by name of var name/aes mapping arg.
    config(displayModeBar = FALSE) %>% ## Remove html buttons
    layout(dragmode = "select") %>% ## Set drag left mouse to section box from zoom window
    event_register("plotly_selected") %>% ## Register based on "selected", on the release of th mouse button.
    highlight(on = "plotly_selected", off = "plotly_deselect")
  ## CLICK SELECT
  ggplotly(g, tooltip = "rownum") %>% ## Tooltip by name of var name/aes mapping arg.
    config(displayModeBar = FALSE) %>% ## Remove html buttons
    layout(dragmode = FALSE) %>% ## Set drag left mouse to section box from zoom window
    event_register("plotly_selected") %>% ## Register based on "selected", on the release of th mouse button.
    highlight(on = 'plotly_click', off = "plotly_doubleclick",
              persistent = TRUE) ## Allow selection of many points?
  ## LASSO SELECT
  ggplotly(g, tooltip = "rownum") %>% ## Tooltip by name of var name/aes mapping arg.
    config(displayModeBar = FALSE) %>% ## Remove html buttons
    layout(dragmode = "lasso") %>% ## Set drag left mouse to section box from zoom window
    event_register("plotly_selected") %>% ## Register based on "selected", on the release of th mouse button.
    highlight(on = 'plotly_selected', off = "plotly_deselect")
}
