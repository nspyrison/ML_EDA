## Created from analysis in:
if(F){
  file.edit("./vignettes/cheem_fifa.rmd")
  file.edit("./vignettes/cheem_varieties.rmd")
}


## Dependancies ------
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
raw <- DALEX::fifa
str(raw)
## Remove factor, target variable `overall`, and highly correlated var, `potential`
dat <- raw %>% dplyr::select(-c(`nationality`)) ##, `potential`, `overall`))
## I know there are too many features, so we'll also bemove some correlatted variables.
tic("correlation")

## Correlation embedding -----
cor_mat <- cor(dat)
corrplot::corrplot(cor_mat,
                   method = "circle", ## geom
                   type = "upper", ## only upper triangle
                   diag = F, ## remove auto correlation
                   order = "FPC", ## First principal component
                   tl.col = "black", tl.srt = 90, ## Text label color and rotation
                   tl.pos = "td")
toc()
## Further avg some highly correlated vars.
dat <- dat %>% dplyr::mutate(
  .keep = "unused",
  ## Target variables,  agg skill, and financial 
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
## starting with 42 variables, we remove 1, and 
## aggregate the other 41 variables into 9 aggregate dimensions based on like correlation of variable, including `value` our tgt variable.
if(F){
  dim(raw)
  dim(dat)
  cor_mat <- cor(dat)
  corrplot::corrplot(cor_mat,
                     method = "circle", ## geom
                     type = "upper", ## only upper triangle
                     diag = F, ## remove auto correlation
                     order = "FPC", ## First principal component
                     tl.col = "black", tl.srt = 90, ## Text label color and rotation
                     tl.pos = "td")
}

## Normalize each column by its standard deviations
dat <- spinifex::scale_sd(dat) %>% as.data.frame()
tgt_var <- dat$value
dat <- dat %>% select(-value)

## Mahalonobis lookup ----
maha_dist <-
  mahalanobis(dat, colMeans(dat), cov(dat)) %>%
  sort(decreasing = TRUE)
maha_lookup_df <- data.frame(id = 1:nrow(dat),
                             name = rownames(dat),
                             dist = maha_dist,
                             value = tgt_var,
                             bmi = dat$bmi,
                             age = dat$age,
                             atk = dat$atk,
                             def = dat$def,
                             acc = dat$acc,
                             mvm = dat$mvm,
                             pwr = dat$pwr,
                             gk  = dat$gk)

## local_attribution_matrix -----
la_mat <- local_attribution_matrix(dat, tgt_var)
