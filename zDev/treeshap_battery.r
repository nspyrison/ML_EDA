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

## changing size of data, linear with N... ----

## Complexity of treeshap looks to be all a func of the tree and not the data. 
## (maybe indirectly?) any rf2 is fast, lets try crossing working data
gc
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

## fifa case ----

## RUNNING 1preprcoess here.
system.time(
  source("./apps/cheem/1preprocess.r")
)[3]
str(dat) ## same str, class as test data
str(tgt_var) ## same numeric vector
.rf ## RF fit.

dat.1 <- dat[1:100, ]
dat.2 <- dat[1:500, ]
dat.3 <- dat[1:1000, ]

dat_fifa <- DALEX::fifa
(mbm3 <- microbenchmark::microbenchmark(
  fifa_smallp_100 = treeshap::treeshap(treeshap::randomForest.unify(.rf, dat.1), x = dat.1),
  fifa_smallp_500 = treeshap::treeshap(treeshap::randomForest.unify(.rf, dat.2), x = dat.2),
  fifa_smallp_1000 = treeshap::treeshap(treeshap::randomForest.unify(.rf, dat.3), x = dat.3),
  times = 1L  
))
# Unit: seconds
# expr    min     lq   mean median     uq    max neval
# fifa_100  13.67  13.67  13.67  13.67  13.67  13.67     1
# fifa_500  43.74  43.74  43.74  43.74  43.74  43.74     1
# fifa_1000 141.17 141.17 141.17 141.17 141.17 141.17     1

