## Creates objs for use in shiny app:
if(F){
  dat
  tgt_var
  maha_lookup_df
  rf
  expl
}

## Created from analysis in:
if(F){
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
  cor_mat <- .raw %>% dplyr::select(-c(`nationality`)) %>% 
    cor()
  corrplot::corrplot(cor_mat,
                     method = "circle", ## geom
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
## starting with 42 variables, we remove `nationality`, and 
## aggregate the other 41 variables into 9 aggregate dimensions based on like correlation of variable, including `value` our tgt variable.

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
                             dist = .maha_dist,
                             value = tgt_var,
                             bmi = dat$bmi,
                             age = dat$age,
                             atk = dat$atk,
                             def = dat$def,
                             acc = dat$acc,
                             mvm = dat$mvm,
                             pwr = dat$pwr,
                             gk  = dat$gk)



## Random forest model -----
.p <- ncol(dat)
.rf_mtry <- if(is.discrete(tgt_var)) sqrt(.p) else .p / 3L
system.time(
  .rf <- randomForest::randomForest(tgt_var~.,
                                    data = data.frame(tgt_var, dat),
                                    mtry = .rf_mtry)
)

## Explainer -----
system.time(
expl <- DALEX::explain(model = .rf,
                       data = dat,
                       y = tgt_var,
                       label = "SHAP-ley values of Rand. Forest")
)


## EXPORT OBJECTS ----
if(F){
  save(dat,
       tgt_var,
       maha_lookup_df,
       expl,
       file = "1preprocess.RData")
  file.copy("./1preprocess.RData", to = "./apps/cheem/data/1preprocess.RData")
  file.remove("./1preprocess.RData")
}



## local_attribution_list NOT RUN -----
if(F){
  tictoc::tic("local_attribution_list")
  la_ls <- local_attribution_list(dat, tgt_var)
  tictoc::toc()
  ### Failing in the loop; DALEX::predict_parts? not when manually run, may want to try  running outside of doParallel, with reduced RFs...
  # Error in { : 
  #     task 1 failed - "no applicable method for 'predict' applied to an object of class "c('randomForest.formula', 'randomForest')""
  str(la_ls)
}

## testing and planning ----

if(F){
  tictoc::tic()
  cheem <- basis_cheem(dat[1:500,], 42, tgt_var[1:500,])
  tictoc::toc()
  str(cheem)
  object.size(cheem)
  
  ## 3.8 Hrs for 5000x30sec / 11 cores
  ## 27.8 Gb for 694736 bytes * 5000 in Gb
  ## .67 Gb  for  16912 bytes * 5000 in Gb
  z <- cheem
  attr(z, "data_else") <- NULL
  str(z)
  object.size(z)
  
  
  for(i in c(1, 3, 7, 10)){
    tictoc::tic(paste0("B = ",i))
    print(basis_cheem(dat, 42, tgt_var, parts_B = i))
    tictoc::toc()
  }
  
  
}

