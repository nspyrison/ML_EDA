if(F) ## Working from: 
  browseURL("https://medium.com/responsibleml/tagged/xai")

#library
library(DALEX)

#apartments dataset from DALEX
#data have 5 numerical variables and 1 factor
head(apartments)

#we use one-hot encoding for district variable - one_hot() function from mltools
data <- mltools::one_hot(data.table::data.table(apartments))

#we created a random forest model using ranger library
library(ranger)
model <- ranger(m2.price~., data = data)

#we created an explainer with DALEX package
explainer <- explain(model, data = data, y = data$m2.price)


# Example for R

# we created a model_parts object
mp <- model_parts(explainer, loss_function = loss_root_mean_square)

# we can see a data.frame with results
mp

# we can plot the results
plot(mp)
