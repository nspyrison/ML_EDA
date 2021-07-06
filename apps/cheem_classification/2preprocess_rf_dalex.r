## Creates objs for use in shiny app

## Dependencies ------
require("DALEX")
require("spinifex")
require("tourr")
require("ggplot2")
require("tictoc")
require("dplyr")
require("treeshap")
require("randomForest")
##
require("palmerpenguins") ## data
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


## Random forest model {randomForest} -----
.p <- ncol(dat)
.rf_mtry <- if(is.discrete(clas1)) sqrt(.p) else .p / 3L
.lvls <- levels(clas1)
tic("RF fit")
i <- 1
test1 <- as.integer(clas1 == .lvls[i])
rf1 <- randomForest::randomForest(test1~.,
                                  data = data.frame(test1, dat),
                                  mtry = .rf_mtry)
toc() ## .22 sec
pred <- predict(.rf1, newdata = dat) ## newdata is only Xs
pred_clas <- .lvls[2 - as.integer(abs(pred) >= .5)]
resid <- as.integer(clas1 == .lvls[i]) - pred


## shap_df RF {DALEX} ------
gc()
tic("DALEX RF shap")
shap_df <- local_attribution_df(data = dat,
                                target_var = test1,
                                model = rf1)

attr(shap_df, "data") <- dat
toc() 
## 1.3 sec for treeshap_df, 
## 22 sec for DALEX SVM local_attribution_df
## 58 sec for DALEX RF local_attribution_df


## Normalized mahalonobis distances (median, covar) ----
maha_vect_of <- function(x){ ## dist from in-class column median(x), cov(x)
  mahalanobis(x, apply(x, 2L, median), cov(x)) %>%
    matrix(ncol = 1) %>%
    scale_01() %>%
    return
}
maha_dat   <- maha_vect_of(dat)
maha_shap  <- maha_vect_of(shap_df)
maha_delta <- maha_shap - maha_dat
hist(maha_delta) ## Not as right skewed as the regression; artifact of wages?
maha_color <- maha_delta
maha_shape <- factor(maha_delta >= 0,
                     levels = c(FALSE, TRUE),
                     labels = c("maha SHAP larger, ", "maha data larger"))

## Create variable spaces! ------

### nMDS
.n <- nrow(dat)
nmds_dat  <- as.data.frame(MASS::isoMDS(dist(dat))$points) %>%
  scale_01() %>% as.data.frame() %>% cbind(1:.n, "data", "nMDS")
nmds_shap <- as.data.frame(MASS::isoMDS(dist(shap_df))$points) %>%
  scale_01() %>% as.data.frame() %>% cbind(1:.n, "shap", "nMDS")

### PCA
pca_dat <- as.matrix(dat) %*% spinifex::basis_pca(dat) %>%
  scale_01() %>% as.data.frame() %>% cbind(1:.n, "data", "PCA")
pca_shap <- as.matrix(shap_df) %*% spinifex::basis_pca(shap_df) %>%
  scale_01() %>% as.data.frame() %>% cbind(1:.n, "shap", "PCA")

### oLDA
olda_dat  <- as.matrix(dat) %*% spinifex::basis_olda(dat, clas1) %>%
  scale_01() %>% as.data.frame() %>% cbind(1:.n, "data", "oLD")
olda_shap <- as.matrix(shap_df) %*% spinifex::basis_olda(shap_df, clas1) %>%
  scale_01() %>% as.data.frame() %>% cbind(1:.n, "shap", "oLD")

### maha cross
bnmds_dat  <- cbind(nmds_dat,  maha_shap)
bnmds_shap <- cbind(nmds_shap, maha_dat)
bpca_dat   <- cbind(pca_dat,   maha_shap)
bpca_shap  <- cbind(pca_shap,  maha_dat)
bolda_dat  <- cbind(olda_dat,  maha_shap)
bolda_shap <- cbind(olda_shap, maha_dat)
## combine
names(bnmds_dat) <- names(bnmds_shap) <- names(bpca_dat) <- names(bpca_shap) <-
  names(bolda_dat) <- names(bolda_shap) <- c(paste0("V", 1:2), "rownum", "obs_type", "var_space", "maha_cross")
bound_spaces_df <- rbind(bnmds_dat,
                         bnmds_shap,
                         bpca_dat,
                         bpca_shap,
                         bolda_dat,
                         bolda_shap)
beepr::beep(4)
## Add replicated classes
.nn <- nrow(bound_spaces_df)
bound_spaces_df$species    <- rep_len(clas1, .nn)
bound_spaces_df$sex        <- rep_len(clas2, .nn)
bound_spaces_df$maha_color <- rep_len(maha_color, .nn)

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

## EXPORT OBJECTS ----
if(F){
  save(dat_decode,
       bound_spaces_df,
       file = "2preprocess_rf_dalex.RData")
  file.copy("./2preprocess_rf_dalex.RData", to = "./apps/cheem_classification/data/2preprocess_rf_dalex.RData", overwrite = TRUE)
  file.remove("./2preprocess_rf_dalex.RData")
}
if(F)
  load("./apps/cheem_classification/data/2preprocess_rf_dalex.RData")


if(F){
  ## Mock-up visual ------
  require("ggplot2")
  tic("prep ggplot ")
  str(bound_spaces_df)
  
  g <- bound_spaces_df %>%
    highlight_key(~rownum) %>% 
    ggplot(aes(V1, V2, rownum = rownum,
               color = maha_color, shape = species)) +
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
                         low = "blue",
                         mid = "grey",
                         high = "red"
    )
    
  
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