### MANUAL C+P PREPROCESS1 TRYING TO FIX NMDS PIPING -----

## Creates objs for use in shiny app:

## Dependencies ------
require("DALEX")
require("spinifex")
require("tourr")
require("ggplot2")
require("tictoc")
require("dplyr")
## Local files
source("./apps/cheem_classification/trees_of_cheem.r") ## Local functions, esp. for basis_cheem() and view_cheem()
source("./apps/cheem_classification/spinifex_ggproto.r") ## New spinifex ggproto_* api
if(F){ ## Manually run to view file:
  file.edit("./apps/cheem_classification/trees_of_cheem.r")
  file.edit("./apps/cheem_classification/spinifex_ggproto.r")
}

## Setup ------
.raw <- DALEX::fifa
.scaled <- .raw %>% dplyr::select(-c(`nationality`, `value_eur`)) %>%
  spinifex::scale_01() %>% as.data.frame()

## Correlation embedding -----
## There are too many correlated features, so we'll aggregate them
if(F){
  str(.scaled)
  tic("correlation")
  .scaled %>%
    cor() %>%
    corrplot::corrplot(method = "circle", ## geom
                       type = "upper", ## only upper triangle
                       diag = F, ## remove auto correlation
                       order = "FPC", ## First principal component
                       tl.col = "black", tl.srt = 90, ## Text label color and rotation
                       tl.pos = "td")
  toc()
}


## Munging -----
## Agg some highly correlated vars.
dat <- .scaled %>% dplyr::mutate(
  .keep = "none",
  ## Target variables, agg skill, and financial 
  #wage = wage_eur,
  bdy = (weight_kg+height_cm)/2, ## bmi wasn't working well after 01 scaling.
  age = age,
  skl = (potential+overall+movement_reactions)/3,
  ## Attack
  atk = (attacking_finishing+skill_long_passing+attacking_volleys+
           power_long_shots+skill_curve+mentality_positioning+attacking_crossing+
           attacking_short_passing+skill_dribbling+skill_ball_control)/10,
  ## Defense
  def = (defending_sliding_tackle+mentality_interceptions+
           defending_standing_tackle+defending_marking+mentality_aggression)/5,
  ## Accuracy
  acc = (attacking_heading_accuracy+power_shot_power)/2,
  ## Movement
  mvm = (movement_sprint_speed+movement_balance+movement_acceleration+
           mentality_vision+mentality_composure+movement_agility+
           mentality_penalties+skill_fk_accuracy+power_stamina+movement_reactions)/10,
  ## Power
  pwr = (power_strength+power_jumping)/2,
  ## Goalkeeping
  gk = (goalkeeping_diving+goalkeeping_positioning+goalkeeping_reflexes+
          goalkeeping_handling+goalkeeping_kicking)/5,
)
## Starting with 42 variables, we remove `nationality`, and aggregate the 
#### other 41 variables into 9 aggregate dimensions based on like correlation 
#### of variable, including `value` our tgt variable.

## Assume the 2nd cluster is goalkeepers, filter on GK and then remove:
dat_gk  <- dat[dat$gk >= .5, ] ## 467 obs, 9.34%
dat_fld <- dat[dat$gk < .5, -9] ## remaining 91%, fielders

## Normalize each column by its standard deviations
tgt_var <- .raw$wage_eur[dat$gk < .5] ## Unscaled wages of the fielders

## Validation projection, 2nd cluster gone after removing gk!
if(F){
  proj <- as.matrix(dat_fld) %*% basis_pca(dat_fld, d = 5) %>% as.data.frame()
  colnames(proj) <- paste0("PC", 1:5)
  GGally::ggpairs(proj)
}

## Random forest model {randomForest} -----
is.discrete <- function(x) ## see plyr::is.discrete(). !! not on levels, class only
  is.factor(x) || is.character(x) || is.logical(x)
.p <- ncol(dat_fld)
.rf_mtry <- if(is.discrete(tgt_var)) sqrt(.p) else .p / 3L
system.time(
  .rf <- randomForest::randomForest(tgt_var~.,
                                    data = data.frame(tgt_var, dat_fld),
                                    mtry = .rf_mtry)
)
pred <- predict(.rf, newdata = dat_fld) ## newdata doesn't have Y
resid <- tgt_var - pred

## shap_df {treeshap} ------
dat_1 <- dat_fld[0001:1100, ]
dat_2 <- dat_fld[1101:2200, ]
dat_3 <- dat_fld[2201:3300, ]
dat_4 <- dat_fld[3301:nrow(dat_fld), ] ## nrow = 4533
tic("treeshap")
gc();Sys.time()
(mbm1 <- microbenchmark::microbenchmark(
  fifa_fifth1 = shap_df1 <- treeshap_df(.rf, dat_1),
  fifa_fifth2 = shap_df2 <- treeshap_df(.rf, dat_2),
  times = 1L
))
beepr::beep(1)
gc();Sys.time()
(mbm2 <- microbenchmark::microbenchmark(
  fifa_fifth3 = shap_df3 <- treeshap_df(.rf, dat_3),
  fifa_fifth4 = shap_df4 <- treeshap_df(.rf, dat_4),
  times = 1L
))
gc();Sys.time()
shap_df <- rbind(shap_df1, shap_df2, shap_df3, shap_df4)
attr(shap_df, "data") <- dat_fld ## Similarly append data
toc() ## ~900sec, 15 min
beepr::beep(4)
if(F){
  save(shap_df, file = "z_shap_df.RData") ## Single obj: shap_df
  file.copy("./z_shap_df.RData", to = "./apps/cheem_regression/data/z_shap_df.RData", overwrite = TRUE)
  file.remove("./z_shap_df.RData")
  rm(list = c(paste0("shap_df", 1L:4L), paste0("dat_", 1L:4L)))
}
if(F)
  load("./apps/cheem_regression/data/z_shap_df.RData")


## Cross mahalonobis dist ----
maha_df_of <- function(x){ ## dist from in-class column median(x), cov(x)
  maha_vect <- mahalanobis(x, apply(x, 2L, median), cov(x))
  data.frame(rownum = 1:nrow(x),
             name = rownames(x),
             maha_dist = round(maha_vect, digits = 1L),
             maha_rank = rank(maha_vect)) %>%
    return()
}
maha_dat  <- maha_df_of(dat_fld)
maha_shap <- maha_df_of(shap_df)

## Create spaces! ------
### nMDS
.n <- nrow(dat_fld)
.nms <- rownames(dat_fld)
nmds_df_of <- function(x, data_nm = substitute(x)){
  nmds_mat <- MASS::isoMDS(dist(x))$points
  nmds_mat %>%
    scale_01 %>%
    as.data.frame %>%
    cbind(1L:nrow(x), data_nm, "nMDS") %>%
    return
}
tic("nMDS");Sys.time()
nmds_dat  <- nmds_df_of(dat_fld, "data") %>%
  cbind(maha_shap$maha_dist) ## nmds data with maha of shap
nmds_shap <- nmds_df_of(shap_df, "shap") %>%
  cbind(maha_dat$maha_dist) ## nmds shap with maha of data
toc() ## ~480 sec
beepr::beep(4)

### PCA
pca_df_of <- function(x, data_nm = substitute(x)){
  x %>% as.matrix() %*% spinifex::basis_pca(dat_fld) %>%
    scale_01 %>% as.data.frame %>% cbind(1:.n, data_nm, "PCA") %>%
    return
}
tic("pca")
pca_dat <- pca_df_of(dat_fld, "data") %>%
  cbind(maha_shap$maha_dist) ## nmds data with maha of shap
pca_shap <- pca_df_of(shap_df, "shap") %>%
  cbind(maha_dat$maha_dist) ## nmds shap with maha of data
toc() ## ~4 sec

### Combine
#df_ls <- list(nmds_dat, nmds_shap, pca_dat, pca_shap)
#lapply(df_ls, dim)
names(nmds_dat) <- names(nmds_shap) <- names(pca_dat) <- names(pca_shap) <-
  c(paste0("V", 1:2), "rownum", "obs_type", "var_space", "x_maha_dist")
### this is hard coded in the plot production if changed will need to be changed in the app, and mock-up below. 
bound_spaces_df <- rbind(nmds_dat, nmds_shap, pca_dat, pca_shap)
.nms <- rep_len(rownames(dat_fld), nrow(bound_spaces_df))
bound_spaces_df <- bound_spaces_df %>%
  mutate(info = paste0("row: ", rownum, ", ", .nms))
beepr::beep(4)

## Combine X, Y and decode info for disp.
dat <- data.frame(1:nrow(dat_fld),
                  maha_dat$maha_dist,
                  maha_shap$maha_dist,
                  pred,
                  tgt_var,
                  resid,
                  dat_fld)
colnames(dat) <- c("rownum", "maha_dist_dat", "maha_dist_shap",
                   "predicted_wage", "residual", "wage_eruo", colnames(dat_fld))
## EXPORT OBJECTS ----
if(F){
  save(dat, bound_spaces_df, file = "1preprocess.RData")
  file.copy("./1preprocess.RData", to = "./apps/cheem_regression/data/1preprocess.RData", overwrite = TRUE)
  file.remove("./1preprocess.RData")
}
if(F)
  load("./apps/cheem_regression/data/1preprocess.RData")

## Mock-up visual ------
if(F){
  require("ggplot2")
  tic("prep ggplot")
  str(bound_spaces_df)
  
  g <- bound_spaces_df %>%
    highlight_key(~rownum) %>% 
    ggplot(hk, aes(V1, V2, rownum = rownum)) +
    geom_point() +
    facet_grid(rows = vars(obs_type), cols = vars(var_space)) +
    theme_bw() +
    theme(axis.text  = element_blank(),
          axis.ticks = element_blank()) +
    scale_color_discrete(name = "") + ## Manual legend title
    scale_shape_discrete(name = "") ## Manual legend title
  toc()
  
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