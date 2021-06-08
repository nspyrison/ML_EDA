#install.packages("fastshap")
require("fastshap")
require("tictoc")
?fastshap::explain

# Load the sample data; see ?datasets::mtcars for details
data(mtcars)

dat <- DALEX::dragons
str(dat)
xdat <- subset(dat, select = -colour)
test <- dat$colour == dat$colour[5] 

#y <- dat$overall
# Fit a projection pursuit regression model
tic("RF Fit")
fit <- randomForest::randomForest(test ~ ., data = data.frame(test, xdat))
toc()

# Compute approximate Shapley values using 10 Monte Carlo simulations
set.seed(101)  # for reproducibility

## fastshap:: -----
tic("Approximate: 10 MC sim")
(shap <- explain(fit, X = xdat, nsim = 10,
                 pred_wrapper = predict))
toc()


tic("exact shap: LinearSHAP")
# Compute exact Shapley (i.e., LinearSHAP) values, not working for RF.
(shap <- explain(fit, X = subset(dat, select = -colour), exact = TRUE,
                 pred_wrapper = predict))
toc()

# Shapley-based plots
library(ggplot2)
autoplot(shap)  # Shapley-based importance plot
autoplot(shap, type = "dependence", feature = "wt", X = mtcars)
autoplot(shap, type = "contribution", row_num = 1)  # explain first row of X


### treeshap ----
browseURL("https://github.com/ModelOriented/treeshap")