## Working from:
if(F)
  browseURL("https://pedroconcejero.wordpress.com/2015/12/04/yet-another-very-short-tutorial-on-doparallel-for-r/")

## Init ----
set.seed(10)
n <- 10^4
y  <- round(runif(n, min = 0, max = 1))
x1 <- c(1:n)*runif(n, min = 0,max = 2)
x2 <- c(1:n)*runif(n, min = 0,max = 2)
x3 <- c(1:n)*runif(n, min = 0,max = 2)

all_data <- data.frame(y, x1, x2, x3)
head(all_data)



## Not parallel ------
positions <- sample(nrow(all_data),
                    size = floor((nrow(all_data)/4)*3))
training <- all_data[positions, ]
testing <- all_data[-positions, ]

library("randomForest")
system.time(
rf_mod <- randomForest(y ~ x1 + x2 + x3,
                       data = training)
)
# user  system elapsed 
# 15.73    0.04   15.83 

system.time(
  predicted_test <- randomForest:::predict.randomForest(rf_mod,
                                                        newdata = testing)
)
# user  system elapsed 
# 0.21    0.00    0.21 
summary(predicted_test)




## Parallel ------
library("doParallel")

.all_cores <- detectCores()
registerDoParallel(cores = detectCores() - 1L) #in windows by default 2, in general n-1
getDoParWorkers()


# useful function using modulo operation -use same as cores
.idx_par <- sort(rank(1:nrow(training)) %% getDoParWorkers()) 

rf_mod_p <- vector()
ptime <- system.time({
  rf_mod_p <- foreach(
    i = unique(split_testing),
    .combine = c, ## A function to bind pieces together
    .packages = c("randomForest")) %dopar% { ## 
      rf_mod <- randomForest(y ~ x1 + x2 + x3,
                             data = training[.idx_par,])
    }
})
ptime
summary(rf_mod_p)
summary(rf_mod)

str(rf_mod_p)
str(rf_mod)

summary(rf_mod_p) == summary(rf_mod)
