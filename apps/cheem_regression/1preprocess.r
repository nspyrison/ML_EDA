## Creates objs for use in shiny app:
if(F){
  dat
  tgt_var
  maha_lookup_df
  #expl
  treeshap_df
  #shap_dist_mat
  shap_dist_quartile
  ## Created from analysis in:
  file.edit("./vignettes/cheem_fifa.rmd")
  file.edit("./vignettes/cheem_varieties.rmd")
}

## Dependencies ------
require("DALEX")
require("spinifex")
require("tourr")
require("ggplot2")
require("tictoc")
require("dplyr")
## Local files
source("./apps/cheem/trees_of_cheem.r") ## Local functions, esp. for basis_cheem() and view_cheem()
source("./apps/cheem/spinifex_ggproto.r") ## New spinifex ggproto_* api
if(F){ ## Manually run to view file:
  file.edit("./apps/cheem/trees_of_cheem.r")
  file.edit("./apps/cheem/spinifex_ggproto.r")
}

## Setup ------
.raw <- DALEX::fifa

## I know there are too many features, so we'll remove some correlated variables.

if(F){
  ## Correlation embedding -----
  str(.raw)
  tic("correlation")
  .raw %>% 
    dplyr::select(-c(`nationality`)) %>%
    cor() %>%
    corrplot::corrplot(method = "circle", ## geom
                       type = "upper", ## only upper triangle
                       diag = F, ## remove auto correlation
                       order = "FPC", ## First principal component
                       tl.col = "black", tl.srt = 90, ## Text label color and rotation
                       tl.pos = "td")
  toc()
}

## Agg some highly correlated vars.
dat <- .raw %>% dplyr::select(-c(`nationality`)) %>% dplyr::mutate(
  .keep = "unused",
  ## Target variables, agg skill, and financial 
  value = (potential+overall+wage_eur+value_eur+movement_reactions)/5,
  bmi = weight_kg / (height_cm / 100)^2,
  age = age,
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
           mentality_penalties+skill_fk_accuracy+power_stamina)/9,
  ## Power
  pwr = (power_strength+power_jumping)/2,
  ## Goalkeeping
  gk = (goalkeeping_diving+goalkeeping_positioning+goalkeeping_reflexes+
          goalkeeping_handling+goalkeeping_kicking)/5,
)
## Starting with 42 variables, we remove `nationality`, and aggregate the 
#### other 41 variables into 9 aggregate dimensions based on like correlation 
#### of variable, including `value` our tgt variable.

## Normalize each column by its standard deviations
dat <- spinifex::scale_sd(dat) %>% as.data.frame()
tgt_var <- dat$value
dat <- dat %>% select(-value)

## Mahalonobis lookup ----
.maha_dist <-
  mahalanobis(dat, colMeans(dat), cov(dat)) %>%
  sort(decreasing = TRUE)
maha_lookup_df <- data.frame(id = 1:nrow(dat),
                             name = rownames(dat),
                             dist = round(.maha_dist, digits = 1L))#,
                             # value = tgt_var,
                             # bmi = dat$bmi,
                             # age = dat$age,
                             # atk = dat$atk,
                             # def = dat$def,
                             # acc = dat$acc,
                             # mvm = dat$mvm,
                             # pwr = dat$pwr,
                             # gk  = dat$gk)


## Random forest model {randomForest} -----
.p <- ncol(dat)
.rf_mtry <- if(is.discrete(tgt_var)) sqrt(.p) else .p / 3L
system.time(
  .rf <- randomForest::randomForest(tgt_var~.,
                                    data = data.frame(tgt_var, dat),
                                    mtry = .rf_mtry)
)

## Explainer {DALEX} -----
# system.time(
# expl <- DALEX::explain(model = .rf,
#                        data = dat,
#                        y = tgt_var,
#                        label = "SHAP-ley values of random forest")
# )

## shap_df {treeshap} ------
dat_1 <- dat[0001:1000, ]
dat_2 <- dat[1001:2000, ]
dat_3 <- dat[2001:3000, ]
dat_4 <- dat[3001:4000, ]
dat_5 <- dat[4001:5000, ]

tic("treeshap")
gc();Sys.time()
(mbm1 <- microbenchmark::microbenchmark(
  fifa_fifth1 = shap_df1 <- treeshap_df(.rf, dat_1),
  fifa_fifth2 = shap_df2 <- treeshap_df(.rf, dat_2),
  fifa_fifth3 = shap_df3 <- treeshap_df(.rf, dat_3),
  times = 1L
))
beepr::beep(1)
gc();Sys.time()
(mbm2 <- microbenchmark::microbenchmark(
  fifa_fifth4 = shap_df4 <- treeshap_df(.rf, dat_4),
  fifa_fifth5 = shap_df5 <- treeshap_df(.rf, dat_5),
  times = 1L
))
beepr::beep(4)
gc();Sys.time()
toc()
shap_df <- rbind(shap_df1, shap_df2, shap_df3, shap_df4, shap_df5)
attr(shap_df, "data")  <- dat ## Similarly append data
if(F)
  rm(list= c(paste0("shap_df", 1:5L), paste0("dat_", 1:5L)))


## shap_dist_mat, shap_dist_quartile ------
tic("shap distance matrix, shap_dist_quartile")
shap_dist_mat <- as.matrix(dist(shap_df))
colnames(shap_dist_mat) <- NULL
rownames(shap_dist_mat) <- NULL
## Init
quantiles <- data.frame(matrix(NA, ncol=5))
colnames(quantiles) <- paste0(seq(0, 100, 25), "pct")
shap_dist_quartile <- data.frame(matrix(NA, 5000, 5000))
sapply(1L:ncol(shap_dist_mat), function(i){
  vect <- shap_dist_mat[, i]
  quantiles[i,] <<- quantile(vect, probs = seq(0, 1, .25))
  shap_dist_quartile[, i] <<- dplyr::case_when(
    vect >= quantiles[i, 1] & vect <= quantiles[i, 2] ~ 1L,
    vect >= quantiles[i, 2] & vect <= quantiles[i, 3] ~ 2L,
    vect >= quantiles[i, 3] & vect <= quantiles[i, 4] ~ 3L,
    vect >= quantiles[i, 4] & vect <= quantiles[i, 5] ~ 4L)
})
hist(quantiles[,5])
toc() ## 1.46 Sec

## NMDS, nmds_dat, nmds_shap -----
nmds_dat  <- as.data.frame(MASS::isoMDS(dist(dat))$points)
nmds_shap <- as.data.frame(MASS::isoMDS(dist(shap_df))$points)
colnames(nmds_dat) <- colnames(nmds_shap) <- paste0("NMDS", 1:2)



## EXPORT OBJECTS ----
if(F)
  load("./apps/cheem/data/1preprocess.RData")
if(F){
  save(dat,
       tgt_var,
       maha_lookup_df,
       # expl,
       shap_df,
       #shap_dist_mat,
       shap_dist_quartile,
       nmds_dat, 
       nmds_shap,
       file = "1preprocess.RData")
  file.copy("./1preprocess.RData", to = "./apps/cheem/data/1preprocess.RData", overwrite = TRUE)
  file.remove("./1preprocess.RData")
  
}

