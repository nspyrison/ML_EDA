## Creates objs for use in shiny app:
if(F){
  dat
  tgt_var
  maha_lookup_df
  #expl
  treeshap_df
  #shap_dist_mat
  shap_dist_quartile
}

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
## Local files
source("./apps/cheem/trees_of_cheem.r") ## Local functions, esp. for basis_cheem() and view_cheem()
source("./apps/cheem/spinifex_ggproto.r") ## New spinifex ggproto_* api
if(F){ ## Manually run to view file:
  file.edit("./apps/cheem/trees_of_cheem.r")
  file.edit("./apps/cheem/spinifex_ggproto.r")
}

## Setup ------

# Data, palmerpenguins::penguins


## Data setup
raw <- palmerpenguins::penguins ## 5 missings, visdat::vis_miss(raw)
raw_rmna <- raw[!is.na(raw$sex),]
lvls <- levels(raw_rmna$species)
## Filter to closest 2 classes
raw_rmna <- raw_rmna[raw_rmna$species %in% lvls[1:2], ]
xdat <- spinifex::scale_sd(raw_rmna[, 3:6])
clas1 <- factor(raw_rmna$species, levels = lvls[1:2]) ## manually remove 3rd lvl
clas2 <- raw_rmna$sex
if(F){
  table(clas1) ## Unbalenced island obs
  table(clas2) ## Balanced sex
}


# ## Additional setup not used in local attribution matrix
# new_obs_x <- rnorm_observation(xdat)

## Encode classes {caret}
require("caret")
dv <- caret::dummyVars(" ~ .", data = raw_rmna)
encoding <- data.frame(predict(dv, raw_rmna))
detach("package:caret", unload = TRUE)
encoding <- encoding[, c(-7:-10, -13)]
str(encoding)

## Normalize each column by its standard deviations
dat <- spinifex::scale_sd(xdat) %>%
  as.data.frame()

## Random forest model {randomForest} -----
.p <- ncol(dat)
.rf_mtry <- if(is.discrete(clas1)) sqrt(.p) else .p / 3L
system.time(
  .rf <- randomForest::randomForest(clas1~.,
                                    data = data.frame(clas1, dat),
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
##TODO: ERROR here with mbm1
# Error in treeshap::randomForest.unify(.rf, data) : 
#   Models built on data with categorical features are not supported - please encode them before training.

tic("treeshap")
gc();Sys.time()
(mbm1 <- microbenchmark::microbenchmark(
  treeshap = shap_df1 <- treeshap_df(.rf, dat),
  times = 1L))
beepr::beep(4)
toc()

####### TODO: below is unedited from fifa ===
################################################
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

