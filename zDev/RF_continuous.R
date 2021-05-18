require("randomForest")
# require("DALEX")
# require("treeshap")
# require("spinifex")
# require("tourr")
# require("ggplot2")

if(F){##examples
  ?randomForest
  ## Classification: (discrete), wants mtry = sqrt(p)
  ##data(iris)
  set.seed(71)
  iris.rf <- randomForest(Species ~ ., data=iris, importance=TRUE,
                          proximity=TRUE)
  print(iris.rf)

  ## Regression: (continuous), wants mtry = p/3
  ## data(airquality)
  set.seed(131)
  ozone.rf <- randomForest(Ozone ~ ., data=airquality, mtry=3,
                           importance=TRUE, na.action=na.omit)
  print(ozone.rf)
}

is.discrete <- function(x) ## see plyr::is.discrete(). !! not on levels, class only
  is.factor(x) || is.character(x) || is.logical(x)

