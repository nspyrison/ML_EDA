install.packages("fastshap")
require("fastshap")
require("tictoc")
?fastshap::explain

# Load the sample data; see ?datasets::mtcars for details
data(mtcars)

dat <- DALEX::fifa
str(dat)
sub <- subset(dat, select = -c(nationality, potential, wage_eur, value_eur))
#y <- dat$overall
# Fit a projection pursuit regression model
fit <- lm(overall ~ ., data = sub)
?randomForest::randomForest()

# Compute approximate Shapley values using 10 Monte Carlo simulations
set.seed(101)  # for reproducibility
tic("Approximate: 10 MC sim")
shap <- explain(fit, X = subset(sub, select = -overall), nsim = 10, 
                pred_wrapper = predict)
shap
toc()

tic("exact shap: LinearSHAP")
# Compute exact Shapley (i.e., LinearSHAP) values
shap <- explain(fit, exact = TRUE)
shap
toc()

# Shapley-based plots
library(ggplot2)
autoplot(shap)  # Shapley-based importance plot
autoplot(shap, type = "dependence", feature = "wt", X = mtcars)
autoplot(shap, type = "contribution", row_num = 1)  # explain first row of X
