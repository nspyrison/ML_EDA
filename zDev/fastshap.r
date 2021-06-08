## requires -----
#install.packages("fastshap")
require("fastshap")
require("tictoc")
require("treeshap")
require("randomForest")
require("ranger")    # for fast random forest algorithm
require("microbenchmark")
?fastshap::explain
set.seed(101) ## Reproducibility, fastshap is spproximate

## Data -----
dat <- DALEX::apartments[, 1:5]
xdat <- dat[, 2:5] ## -c(m2.price, district))
y <- dat$m2.price

.rf <- randomForest::randomForest(y ~ ., data = data.frame(y, xdat))
.ranger <- ranger::ranger(y ~ ., data = data.frame(y, xdat))

## Summary ----
print("both treeshap and fastshap only work for continuous predictions.")
microbenchmark(
  fit_rf = {randomForest::randomForest(y ~ ., data = data.frame(y, xdat))},
  fit_ranger = {ranger::ranger(y ~ ., data = data.frame(y, xdat))},
  fastshap_10_rf = {fastshap::explain(.rf, X = xdat, nsim = 10,
                                      pred_wrapper = predict)},
  # fastshap_10_ranger = {fastshap::explain(.ranger, X = xdat, nsim = 10,
  #                                     pred_wrapper = predict)},
  treeshap_rf = {treeshap::treeshap(treeshap::randomForest.unify(.rf, xdat), x = xdat)},
  treeshap_ranger = {treeshap::treeshap(treeshap::ranger.unify(.ranger, xdat), x = xdat)},
  dalex_rf_1obs = 
    DALEX::predict_parts_shap(
      explainer = DALEX::explain(model = .rf, data = xdat, y = y, label = ".rf"),
      new_observation = xdat[1,, drop =FALSE]),
  dalex_ranger_1obs =
    DALEX::predict_parts_shap(
      explainer = DALEX::explain(model = .ranger, data = xdat, y = y, label = "ranger"),
      new_observation = xdat[1,, drop =FALSE]),
  times = 3
)



## RF model fit -----
tic("RF fit")
fit <- randomForest::randomForest(y ~ ., data = data.frame(y, xdat)) ## Factor y, 2 levels
toc()
fit2 <- randomForest::randomForest(mpg ~ ., data = mtcars) ## Continuous y
f_dat <- tourr::flea[, 1:6]
f_y <- tourr::flea$species == tourr::flea$species[10]
fit3 <- randomForest::randomForest(f_y ~ ., data = data.frame(f_y, f_dat)) ## Boolean y

## pkg: fastshap -----
tic("Approximate: 10 MC sim")
(shap <- explain(rfo, X = xdat, nsim = 10,
                 pred_wrapper = ranger::predictions)) ## But all NA; because of factor y?
explain(rfo, X = xdat, pred_wrapper = pfun, nsim = 10)
toc() ## Approximate: 10 MC sim: 2.8 sec elapsed
(shap2 <- explain(fit2, X = mtcars[, -1], nsim = 10,
                 pred_wrapper = predict)) ## Works for continuous, how about boolean tests?
(shap3 <- explain(fit3, X = f_dat, nsim = 10)) ## fine for continuous, how about binary

rfo <- ranger::ranger(y ~ ., data =  data.frame(y, xdat))
## have tried using ranger models, and cannot get pred_wrapper to work.
print("fastshap is DoA, go to treeshap")



## pkg: treeshap ----
browseURL("https://github.com/ModelOriented/treeshap")
?treeshap::treeshap(unified_model, x)

ufit <- randomForest.unify(fit, xdat)
# Error in randomForest.unify(fit, xdat) : 
#   Models built on data with categorical features are not supported - please encode them before training.
urfo <- ranger.unify(rfo, xdat)
# Error in as.character.factor(x) : malformed factor
# In addition: Warning message:
#   In Ops.factor(Prediction, n) : '/' not meaningful for factors
ufit2 <- randomForest.unify(fit2, mtcars[, -1]) ## Got one!
ufit3 <- randomForest.unify(fit3, f_dat) ## Got one!


?classCenter
