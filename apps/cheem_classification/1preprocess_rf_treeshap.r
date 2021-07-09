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
require("caret") ## Modeling
require("plotly") ## Linked brushing
## Local files
source("./apps/cheem_classification/trees_of_cheem.r") ## Local functions, esp. for basis_cheem() and view_cheem()
if(F) ## Manually run to view file:
  file.edit("./apps/cheem_classification/trees_of_cheem.r")


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

## Init:
.n <- nrow(dat)
.p <- ncol(dat)
.rf_mtry <- if(is.discrete(clas1)) sqrt(.p) else .p / 3L
.lvls <- levels(clas1)
##
X <- dat
Y <- clas1 
Y_bool <- as.integer(.lvls[1] == Y)

# tic("RF fit")
# ## Boolean test for treeshap, :()
# rf1 <- randomForest::randomForest(
#   Y_bool~., data = data.frame(Y_bool, dat), mtry = .rf_mtry)
# toc() ## .22 sec
# pred <- predict(rf1, newdata = X) ## newdata is only Xs
# pred_clas <- .lvls[2 - as.integer(pred >= .5)]
# resid <- Y_bool - pred

## modeling {caret} -----
require("caret")
if(F)
  browseURL("https://bookdown.org/gaetan_lovey/data_analytics/modeling.html#logistic-regression")
set.seed(123)
metric <- "Accuracy"

## GLM
fit_glm_aic = train(form = Y ~ .,
                    data = data.frame(Y, X),
                    #trControl = train_control,
                    method = "glmStepAIC",
                    metric = metric,
                    family = "binomial")
pred_glm_aic <- predict(fit_glm_AIC, newdata = X)
## performance:
confusionMatrix(data = pred_glm_aic, reference = Y)
fit_glm_AIC$finalModel
## KNN classification
fit_knn_tuned = train(form = Y ~ .,
                      data = data.frame(Y, X),
                      method = "knn",
                      metric = metric,
                      tuneGrid = expand.grid(k = seq(1, 101, by = 1)))

## SVM
hp_svm <- expand.grid(cost = 10 ^ ((-2):1))
fit_svm <- train(form = Y ~ .,
                 data = data.frame(Y, X),
                 tuneGrid = hp_svm,
                 method = "svmLinear2",
                 metric = metric)
## NN
hp_nn <- expand.grid(size = 2:10,
                     decay = seq(0, 0.5, 0.05))
fit_nn <- fit_nn <- train(form = Y ~ .,
                          data = data.frame(Y, X),
                          tuneGrid = hp_nn,
                          method = "nnet",
                          metric = metric, 
                          verbose = FALSE)
## LDA
fit_LDA <- train(form = Y ~ .,
                 data = data.frame(Y, X),
                 method = "lda",
                 metric = metric)
## Random forest 
hp_rf <- expand.grid(.mtry = (1:15))
fit_rf <- train(form = Y ~ .,
                data = data.frame(Y, X),
                method = "rf",
                metric = metric,
                tuneGrid = hp_rf)

## randomForest rf
.rf_mtry <- if(is.discrete(clas1)) sqrt(.p) else .p / 3L
fit_rf_rF <- randomForest::randomForest(
  Y_bool~., data = data.frame(Y_bool, dat), mtry = .rf_mtry)

## ranger rf
fir_rf_ranger <- ranger::ranger(Y_bool~., data = data.frame(Y_bool, dat))

## dalex timer:
dalex_shap_expr <- expression({
  sec_shap <- system.time({
    gc()
    .shap <- local_attribution_df(X, Y, .rf)
  })[3]
})

## shap_df {treeshap} ------
gc()
tic("treeshap")
shap_df1 <- treeshap_df(rf1, dat)
toc() ## 1.3 sec

## oLDA & shap -----
olda_dat  <- olda_df_of(X, Y)
olda_shap1 <- olda_df_of(shap_df1, Y)
maha_dat   <- maha_vect_of(dat)
maha_shap1  <- maha_vect_of(shap_df1)

## bind and formate for ploting with facets
plot_df  <- data.frame(
  V1 = c(olda_dat[1], olda_shap)
)
## combine
names(bolda_dat) <- names(bolda_shap) <- c(paste0("V", 1:2), "rownum", "obs_type", "var_space", "maha_dist")
bound_spaces_df <- rbind(bolda_dat,
                         bolda_shap)
beepr::beep(4)
## Add replicated classes
.nn <- nrow(bound_spaces_df)
bound_spaces_df$species    <- rep_len(clas1, .nn)
bound_spaces_df$sex        <- rep_len(clas2, .nn)
bound_spaces_df$residual   <- rep_len(resid, .nn)

## reconstruct dat with features
dat_decode <- data.frame(1:nrow(dat),
                         clas1,
                         pred_clas,
                         round(pred, 2),
                         round(resid, 2),
                         clas2,
                         round(dat, 2))
colnames(dat_decode) <- c("rownum", "obs_species", "pred_species",
                          "prediction", "residual", "sex", colnames(dat))

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
