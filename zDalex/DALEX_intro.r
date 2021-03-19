# if(F) ## Working from: 
#   browseURL("https://medium.com/responsibleml/treeshap-explain-tree-based-models-with-shap-values-2900f95f426")
# 
# #devtools::install_github('ModelOriented/treeshap')
# ## uses an algorithm to compute SHAP values for tree ensemble models, in polynomial-time, Lundberg et.al (2018)
# # require("DALEX")
# # require("treeshap")


if(F) ## Working from: 
  browseURL("http://ema.drwhy.ai/shapley.html#SHAPRcode")

titanic_imputed <- archivist::aread("pbiecek/models/27e5c")
titanic_rf <- archivist::aread("pbiecek/models/4e0fc")
henry <- archivist::aread("pbiecek/models/a6538")

library("randomForest")
library("DALEX")

## Make a DALEX "explainer" of in smaple data
explain_rf <- DALEX::explain(model = titanic_rf,  
                             data = titanic_imputed[, -9],
                             y = titanic_imputed$survived == "yes", 
                             label = "Random Forest")
## Predict a single out of sample observation, "Henry"?
predict(explain_rf, henry)

tictoc::tic("shap_henry")
shap_henry <- predict_parts(explainer = explain_rf,  ## ~ 10 s @ B=25
                            new_observation = henry, 
                            type = "shap",
                            B = 10)
tictoc::toc()
plot(shap_henry, show_boxplots = FALSE)

print("note that iBreakDown:::print.break_down prints an agg tbl, not the 11 perms tested and desplayed when coerced to tibble.")
tib_shap_henry <- tibble::as.tibble(shap_henry) ## Note that SHAP is already showing only 7 of 77 branches.
hist(tib_shap_henry$contribution)


print("why isn't it showing the 7 largest contributions though??")
library("dplyr")
tib_shap_henry <- tib_shap_henry %>% arrange(desc(abs(contribution)))
tib_shap_henry
unique(tib_shap_henry$variable)

## Remade from: iBreakDown:::print.break_down_uncertainty
## Create the scree df for the local attribution from a DALEX::predict_parts return.
## !!may have overlap with iBreakDown:::plot.break_down_uncertainty.
df_scree_local_attr <- function(x, ...){
  result <- data.frame(
    label = tapply(x$label, paste(x$label, x$variable, sep = ": "), unique, na.rm = TRUE),
    variable_name = tapply(x$variable_name, paste(x$label, x$variable, sep = ": "), unique, na.rm = TRUE),
    variable_value = tapply(x$variable_value, paste(x$label, x$variable, sep = ": "), unique, na.rm = TRUE),
    median_local_attr = tapply(x$contribution, paste(x$label, x$variable, sep = ": "), median, na.rm = TRUE)
  )
  ## Reorder
  result <- result[order(abs(result$median_local_attr), decreasing = TRUE), ]
  ## Add cumsum_rate
  result$cumsum_rate_abs_median_local_attr <- cumsum(abs(result$median_local_attr)) / sum(abs(result$median_local_attr))
  return(result)
}
df_local_attr <- df_scree_local_attr(shap_henry)


iBreakDown:::plot.break_down_uncertainty
