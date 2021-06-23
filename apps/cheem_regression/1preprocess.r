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


## Mahalonobis lookup ----
.maha_dist <-
  mahalanobis(dat_fld, colMeans(dat_fld), cov(dat_fld)) %>%
  sort(decreasing = TRUE)
maha_lookup_df <- data.frame(rownum = 1:nrow(dat_fld),
                             name = rownames(dat_fld),
                             maha_dist = round(.maha_dist, digits = 1L))


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
toc()
beepr::beep(4)
if(F)
  rm(list= c(paste0("shap_df", 1:4L), paste0("dat_", 1:4L)))


# ## shap_dist_mat, shap_dist_quartile ------
# tic("shap distance matrix, shap_dist_quartile")
# shap_dist_mat <- as.matrix(dist(shap_df))
# colnames(shap_dist_mat) <- NULL
# rownames(shap_dist_mat) <- NULL
# ## Init
# quantiles <- data.frame(matrix(NA, ncol=5))
# colnames(quantiles) <- paste0(seq(0, 100, 25), "pct")
# shap_dist_quartile <- data.frame(matrix(NA, nrow(shap_df), nrow(shap_df)))
# sapply(1L:ncol(shap_dist_mat), function(i){
#   vect <- shap_dist_mat[, i]
#   quantiles[i,] <<- quantile(vect, probs = seq(0, 1, .25))
#   shap_dist_quartile[, i] <<- dplyr::case_when(
#     vect >= quantiles[i, 1] & vect <= quantiles[i, 2] ~ 1L,
#     vect >= quantiles[i, 2] & vect <= quantiles[i, 3] ~ 2L,
#     vect >= quantiles[i, 3] & vect <= quantiles[i, 4] ~ 3L,
#     vect >= quantiles[i, 4] & vect <= quantiles[i, 5] ~ 4L)
# })
# hist(quantiles[,5])
# toc() ## 1.46 Sec

## Create spaces! ------
### nMDS
.n <- nrow(dat_fld)
.nms <- rownames(dat_fld)
tic("nMDS");Sys.time()
nmds_dat  <- as.data.frame(MASS::isoMDS(dist(dat_fld))$points) %>%
  scale_01() %>% as.data.frame() %>% cbind(1:.n, "data", "nMDS")
beepr::beep(1)
nmds_shap <- as.data.frame(MASS::isoMDS(dist(shap_df))$points) %>%
  scale_01() %>% as.data.frame() %>% cbind(1:.n, "shap", "nMDS")
beepr::beep(4)
toc()

### PCA
tic("pca")
pca_dat  <- as.matrix(dat_fld) %*% spinifex::basis_pca(dat_fld) %>%
  scale_01() %>% as.data.frame() %>% cbind(1:.n, "data", "PCA")
pca_shap <- as.matrix(shap_df) %*% spinifex::basis_pca(shap_df) %>%
  scale_01() %>% as.data.frame() %>% cbind(1:.n, "shap", "PCA")
toc()


### Combine
names(nmds_dat) <- names(nmds_shap) <- names(pca_dat) <- names(pca_shap) <-
  c(paste0("V", 1:2), "rownum", "data", "space")
bound_spaces_df <- rbind(nmds_dat, nmds_shap, pca_dat, pca_shap)
.nms <- rep_len(rownames(dat_fld), nrow(bound_spaces_df))
bound_spaces_df <- bound_spaces_df %>% 
  mutate(info = paste0("row: ", rownum, ", ", .nms))
beepr::beep(4)

## combine X, Y and decode info for disp.
dat <- data.frame(1:nrow(dat_fld), 
                  paste0("row: ", 1:nrow(dat_fld), ",", rownames(dat_fld)), 
                  maha_lookup_df$maha_dist,
                  tgt_var, 
                  dat_fld)
colnames(dat) <- c("rownum", "info", "maha_dist", "wage_eruo", colnames(dat_fld))
## EXPORT OBJECTS ----
if(F){
  save(dat,
       tgt_var,
       maha_lookup_df,
       bound_spaces_df,
       file = "1preprocess.RData")
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
  .nn <- nrow(bound_spaces_df)
  
  hk <- bound_spaces_df %>%
    highlight_key(~rownum)
  g <- ggplot(hk, aes(V1, V2, rownum = rownum)) +
    geom_point() +
    facet_grid(rows = vars(data), cols = vars(space)) +
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