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
if(F) ## Manually run to view file:
  file.edit("./apps/cheem_classification/trees_of_cheem.r")


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
dat_fld <- scale_01(dat_fld) ## scale again after removing.
 
## Normalize each column by its standard deviations
tgt_var <- .raw$wage_eur[dat$gk < .5] ## Unsealed wages of the fielders

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
) ## ~ 12 sec
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
  save(shap_df, file = "z_shap_df.RData") ## Single obj: .rf, shap_df, 
  file.copy("./z_shap_df.RData", to = "./apps/cheem_regression/data/z_shap_df.RData", overwrite = TRUE)
  file.remove("./z_shap_df.RData")
  rm(list = c(paste0("shap_df", 1L:4L), paste0("dat_", 1L:4L)))
}
if(F)
  load("./apps/cheem_regression/data/z_shap_df.RData")


## mahalonobis dist?, then normalize ----
maha_vect_of <- function(x){ ## dist from in-class column median(x), cov(x)
  mahalanobis(x, apply(x, 2L, median), cov(x)) %>%
    matrix(ncol = 1) %>% #log() %>% 
    scale_01() %>% 
    return
}
maha_dat  <- maha_vect_of(dat_fld)
maha_shap <- maha_vect_of(shap_df)
maha_delta  <- maha_shap - maha_dat
summary(maha_delta) ## there are few negative values; 
### fewer pts are further away in shap sp than data space
### will these be under represented? i think so, may need to handle alpha differently.
#maha_alpha <- .2 + .8 * scale_01(abs(maha_delta - median(maha_delta)))^2 ## distance from mediant of delta to balence fewer neg.
maha_color <- maha_delta
maha_shape <- factor(maha_delta >= 0, 
                     levels = c(FALSE, TRUE),
                     labels = c("maha SHAP larger, ", "maha data larger"))

## DOES REMOVING 90% lowest maha_dat remove the correct points!?
if(F){
  hist(maha_del)
  .lb <- quantile(maha_dat, probs = .9)
  summary(maha_alpha[dat_fld >= .lb]) ## NOT REALLY removing lots high deltas too.
}
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
nmds_dat  <- nmds_df_of(dat_fld, "data")
nmds_shap <- nmds_df_of(shap_df, "shap")
toc() ## ~480 sec, 8 min
beepr::beep(4)

### PCA
pca_df_of <- function(x, data_nm = substitute(x)){
  x %>% as.matrix() %*% spinifex::basis_pca(dat_fld) %>%
    scale_01 %>% as.data.frame %>% cbind(1:.n, data_nm, "PCA") %>%
    return
}
tic("pca")
pca_dat  <- pca_df_of(dat_fld, "data")
pca_shap <- pca_df_of(shap_df, "shap")
toc() ## ~4 sec

### Combine
#df_ls <- list(nmds_dat, nmds_shap, pca_dat, pca_shap)
#lapply(df_ls, dim)
bnmds_dat  <- cbind(nmds_dat,  maha_shap)
bnmds_shap <- cbind(nmds_shap, maha_dat)
bpca_dat   <- cbind(pca_dat,   maha_shap)
bpca_shap  <- cbind(pca_shap,  maha_dat)
names(bnmds_dat) <- names(bnmds_shap) <- names(bpca_dat) <- names(bpca_shap) <-
  c(paste0("V", 1:2), "rownum", "obs_type", "var_space", "maha_cross")

bound_spaces_df <- rbind(bnmds_dat ,
                         bnmds_shap,
                         bpca_dat  ,
                         bpca_shap)
.NN <- nrow(bound_spaces_df)
.nms          <- rep_len(rownames(dat_fld), .NN)
bound_spaces_df <- bound_spaces_df %>%
  mutate(info = paste0("row: ", rownum, ", ", .nms),
         rowname = .nms,
         maha_dat   = rep_len(maha_dat,   .NN),
         maha_shap  = rep_len(maha_shap,  .NN),
         maha_color = rep_len(maha_color, .NN),
         maha_alpha = rep_len(maha_alpha, .NN),
         maha_shape = rep_len(maha_shape, .NN)
  )
beepr::beep(4)

## Combine X, Y and decode info for display
dat <- data.frame(1:nrow(dat_fld),
                  maha_dat,
                  maha_shap,
                  tgt_var,
                  round(pred),
                  round(resid),
                  dat_fld)
colnames(dat) <- c("rownum", "maha_dist_dat", "maha_dist_shap",
                   "obs_wage_euro", "predicted_wage", "residual", colnames(dat_fld))
## Top 2% by maha_data or maha_shap, the colored points
inc_idx <- maha_dat > quantile(maha_dat, probs = .98) | maha_shap > quantile(maha_shap, probs = .98)
DT_data <- dat[inc_idx, ] ## onoly the colored rows.
## EXPORT OBJECTS ----
if(F){
  save(DT_data, bound_spaces_df, file = "1preprocess.RData")
  file.copy("./1preprocess.RData", to = "./apps/cheem_regression/data/1preprocess.RData", overwrite = TRUE)
  file.remove("./1preprocess.RData")
}
if(F)
  load("./apps/cheem_regression/data/1preprocess.RData")

## Mock-up visual ------
if(F){
  require("ggplot2"); require("plotly")
  tic("prep ggplot")
  #str(bound_spaces_df)
  ## grey and color pts
  df <- bound_spaces_df
  idx_dat  <- bound_spaces_df$maha_dat  > quantile(bound_spaces_df$maha_dat, probs = .98)
  idx_shap <- bound_spaces_df$maha_shap > quantile(bound_spaces_df$maha_shap, probs = .98)
  pts_idx <- idx_dat | idx_shap
  grey_pts_idx <- !pts_idx
  sum(pts_idx)/4
  ## find txt pts
  idx_dat  <- bound_spaces_df$maha_dat  > quantile(bound_spaces_df$maha_dat, probs = .999)
  idx_shap <- bound_spaces_df$maha_shap > quantile(bound_spaces_df$maha_shap, probs = .999)
  txt_pts_idx <- idx_dat | idx_shap
  sum(txt_pts_idx)/4
  ## Plot
  g <- df[pts_idx, ] %>%
    ## Plotly interaction key
    plotly::highlight_key(~rownum) %>%
    ggplot(aes(V1, V2)) +
    ## Grey points
    geom_point(aes(shape = maha_shape), data = df[grey_pts_idx, ], color = "grey") +
    ## Density contours, .99, .5, .1, .01
    geom_density2d(aes(V1, V2), df, color = "black",
                   contour_var = "ndensity", breaks = c(.1, .5, .9, .99)) +
    ## Color points
    geom_point(aes(info = info, color = maha_cross,
                   shape = maha_shape)) +
    ## Text points
    geom_text(aes(label = rowname), df[txt_pts_idx, ], color = "blue") +
    facet_grid(rows = vars(obs_type), cols = vars(var_space), scales = "free") +
    theme_bw() +
    theme(axis.text  = element_blank(),
          axis.ticks = element_blank()) +
    scale_color_continuous(name = "Normal \n Mahalonobis \n distances, \n crossed", 
                           type = "viridis") +
    scale_shape_discrete(name = "")
  
  ## BOX SELECT, FROM APP:
  ggplotly(g, tooltip = "info") %>% ## Tooltip by name of var name/aes mapping arg.
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
