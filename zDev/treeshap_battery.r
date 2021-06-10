## requires -----
require("tictoc")
require("treeshap")
require("randomForest")
require("microbenchmark")
?treeshap::treeshap
set.seed(101) ## Reproducibility, fastshap is spproximate

require("DALEX")
require("spinifex")
## Data -----
dim(DALEX::apartments[, 1:5])
dim(spinifex::PimaIndiansDiabetes_wide)
dim(spinifex::PimaIndiansDiabetes_long)

dat1 <- DALEX::apartments[, 2:5] ## -c(m2.price, district))
y1 <- DALEX::apartments$m2.price

# str(spinifex::PimaIndiansDiabetes_wide)
dat2 <- spinifex::PimaIndiansDiabetes_wide[,1:7]
y2 <- spinifex::PimaIndiansDiabetes_wide$age

# str(spinifex::PimaIndiansDiabetes_long)
dat3 <- spinifex::PimaIndiansDiabetes_long[,1:5]
y3 <- spinifex::PimaIndiansDiabetes_long$age

## RF -----
.rf1 <- randomForest::randomForest(y1 ~ ., data = data.frame(y1, dat1))
.rf2 <- randomForest::randomForest(y2 ~ ., data = data.frame(y2, dat2))
.rf3 <- randomForest::randomForest(y3 ~ ., data = data.frame(y3, dat3))


## Summary ----
(mbm <- microbenchmark::microbenchmark(
  treeshap_rf1 = {treeshap::treeshap(treeshap::randomForest.unify(.rf1, dat1), x = dat1)},
  treeshap_rf2 = {treeshap::treeshap(treeshap::randomForest.unify(.rf2, dat2), x = dat2)},
  treeshap_rf3 = {treeshap::treeshap(treeshap::randomForest.unify(.rf3, dat3), x = dat3)},
  times = 3
))
ggplot2::autoplot(mbm)

## 2) Vary n, linear time... ----

## Complexity of treeshap looks to be all a func of the tree and not the data. 
## (maybe indirectly?) any rf2 is fast, lets try crossing working data
gc()
dat2_.5 <- dat2[1:200, ]
dat2_2 <- rbind(dat2, dat2)
(mbm2 <- microbenchmark::microbenchmark(
  rf2_1 = treeshap::treeshap(treeshap::randomForest.unify(.rf2, dat2), x = dat2),
  rf2_.5 = treeshap::treeshap(treeshap::randomForest.unify(.rf2, dat2_.5), x = dat2_.5),
  rf2_2 = treeshap::treeshap(treeshap::randomForest.unify(.rf2, dat2_2), x = dat2_2),
  times = 1
))
# Unit: seconds
# expr    min     lq   mean median     uq    max neval
# rf2_1  9.110  9.110  9.110  9.110  9.110  9.110     1
# rf2_.5  4.431  4.431  4.431  4.431  4.431  4.431     1
# rf2_2 17.276 17.276 17.276 17.276 17.276 17.276     1

## 3) fifa case ----

### Over n, linear -----

## RUNNING 1preprcoess here.
system.time(
  source("./apps/cheem/1preprocess.r")
)[3]
str(dat) ## same str, class as test data
str(tgt_var) ## same numeric vector
.rf ## RF fit.

dat.100 <- dat[1:100, ]
dat.500 <- dat[1:500, ]
dat.1000 <- dat[1:1000, ]
dat.3000 <- dat[1:3000, ]
dat.5000 <- dat[1:5000, ]


(mbm3 <- microbenchmark::microbenchmark(
  fifa_lowp_100 = treeshap::treeshap(treeshap::randomForest.unify(.rf, dat.100), x = dat.100),
  fifa_lowp_500 = treeshap::treeshap(treeshap::randomForest.unify(.rf, dat.500), x = dat.500),
  fifa_lowp_1000 = treeshap::treeshap(treeshap::randomForest.unify(.rf, dat.1000), x = dat.1000),
  times = 1L  
))
# Unit: seconds
# expr    min     lq   mean median     uq    max neval
# fifa_lowp_100  13.67  13.67  13.67  13.67  13.67  13.67     1
# fifa_lowp_500  43.74  43.74  43.74  43.74  43.74  43.74     1
# fifa_lowp_1000 141.17 141.17 141.17 141.17 141.17 141.17     1

### Over p, negligable -----

dat_fifa <- DALEX::fifa %>%
  dplyr::select(-c(`nationality`, `potential`, `overall`, `wage_eur`, `value_eur`, `movement_reactions`))
y_fifa <- DALEX::fifa$overall
rf_fifa <- randomForest::randomForest(y_fifa~., data = data.frame(y_fifa, dat_fifa))

dat_fifa.100  <- dat_fifa[1:100,]
dat_fifa.500  <- dat_fifa[1:500,]
dat_fifa.1000 <- dat_fifa[1:1000,]
dat_fifa.3000 <- dat_fifa[1:3000,]
dat_fifa.5000 <- dat_fifa[1:5000,]


(mbm4 <- microbenchmark::microbenchmark(
  fifa_lowp_100 = treeshap::treeshap(treeshap::randomForest.unify(.rf, dat.100), x = dat.100),
  fifa_lowp_500 = treeshap::treeshap(treeshap::randomForest.unify(.rf, dat.500), x = dat.500),
  fifa_lowp_1000 = treeshap::treeshap(treeshap::randomForest.unify(.rf, dat.1000), x = dat.1000),
  # fifa_lowp_3000 = treeshap::treeshap(treeshap::randomForest.unify(.rf, dat.3000), x = dat.1000),
  # fifa_lowp_5000 = treeshap::treeshap(treeshap::randomForest.unify(.rf, dat.5000), x = dat.1000),
  fifa_highp_100 = treeshap::treeshap(treeshap::randomForest.unify(rf_fifa, dat_fifa.100), x = dat_fifa.100),
  fifa_highp_500 = treeshap::treeshap(treeshap::randomForest.unify(rf_fifa, dat_fifa.500), x = dat_fifa.500),
  fifa_highp_1000 = treeshap::treeshap(treeshap::randomForest.unify(rf_fifa, dat_fifa.1000), x = dat_fifa.1000),
  # fifa_highp_3000 = treeshap::treeshap(treeshap::randomForest.unify(rf_fifa, dat_fifa.3000), x = dat_fifa.1000),
  # fifa_highp_5000 = treeshap::treeshap(treeshap::randomForest.unify(rf_fifa, dat_fifa.5000), x = dat_fifa.1000),
  
  times = 1L
))
dim(dat.5000); dim(dat_fifa.5000)
# Unit: seconds
# expr    min     lq   mean median     uq    max neval
# fifa_lowp_100  11.00  11.00  11.00  11.00  11.00  11.00     1
# fifa_lowp_500  41.36  41.36  41.36  41.36  41.36  41.36     1
# fifa_lowp_1000 133.34 133.34 133.34 133.34 133.34 133.34     1
# fifa_highp_100  11.30  11.30  11.30  11.30  11.30  11.30     1
# fifa_highp_500  46.53  46.53  46.53  46.53  46.53  46.53     1
# fifa_highp_1000 156.68 156.68 156.68 156.68 156.68 156.68     1
# R> dim(dat.1000); dim(dat_fifa.1000)
# [1] 1000    8
# [1] 1000   36

### piecemeal 1000s -----
print("Seems to hang at n=3000, 5000, so try piecemeal 1000's")

str(dat) ## same str, class as test data
str(tgt_var) ## same numeric vector
.rf ## RF fit.

dat_1 <- dat[0001:1000, ]
dat_2 <- dat[1001:2000, ]
dat_3 <- dat[2001:3000, ]
dat_4 <- dat[3001:4000, ]
dat_5 <- dat[4001:5000, ]

gc()
Sys.time()
(mbm5 <- microbenchmark::microbenchmark(
  fifa_fifth1 = treeshap::treeshap(treeshap::randomForest.unify(.rf, dat_1), x = dat_1),
  fifa_fifth1_ns = treeshap_df(.rf, dat_1),
  # fifa_fifth2 = shap_df2 <- treeshap::treeshap(treeshap::randomForest.unify(.rf, dat_2), x = dat_2),
  # fifa_fifth3 = shap_df3 <- treeshap::treeshap(treeshap::randomForest.unify(.rf, dat_3), x = dat_3),
  # fifa_fifth4 = shap_df4 <- treeshap::treeshap(treeshap::randomForest.unify(.rf, dat_4), x = dat_4),
  # fifa_fifth5 = shap_df5 <- treeshap::treeshap(treeshap::randomForest.unify(.rf, dat_5), x = dat_5),
  times = 1L
))
Sys.time()
## run together:
# Unit: seconds
# expr   min    lq  mean median    uq   max neval
# fifa_fifth1 117.1 117.1 117.1  117.1 117.1 117.1     1
# fifa_fifth2 171.7 171.7 171.7  171.7 171.7 171.7     1
# fifa_fifth3 201.4 201.4 201.4  201.4 201.4 201.4     1