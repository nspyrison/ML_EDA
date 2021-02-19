require("Rtnse")
require("randomForest")
## Scaling performance eval of variable vs PC spaces
#data(package = "mlbench")
if(F){
  require("tidyverse")
  z <- readr::read_tsv("./data/big_five/data.tsv")
  attr(z, "problems") <- NULL ## Null data, try to 
  skim_z <- skimr::skim(z)
  z <- z %>% dplyr::filter(IPC == 1) %>% 
    dplyr::select(-lat_appx_lots_of_err, -long_appx_lots_of_err, -IPC)
  z <- type.convert(z)
  data_cleaned <- z
  readr::write_rds(data_cleaned, "./data/big_five/data_cleaned.rds")
}
data_cleaned <- readr::read_rds("./data/big_five/data_cleaned.rds")
str(data_cleaned)
Rdimtools::est.boxcount(m)$estdim ## Bar, mean of 68.99 ms
## est.clustering(m)$estdim ## ~ 24.8x slower
Rdimtools::est.correlation(m)$estdim
## est.danco(m)$estdim ## ~ 26x slower
## est.gdistnn(m)$estdim ## ~ 11x slower
## est.incisingball(m)$estdim ## ~ 22x slower
## est.mindkl(m)$estdim ## ~ 23x slower
Rdimtools::est.made(m)$estdim
##est.mle1(m)$estdim ## mle2 uses a correctio term and both are about same speed
Rdimtools::est.mle2(m)$estdim
Rdimtools::est.twonn(m)$estdim
##Rdimtools::est.Ustat(m)$estdim ## ~ 5x slower

## List of data sets
dat <- list(
  spinifex::BreastCancer[, 2:10],
)
clas <- list(
  spinifex::BreastCancer$Class
)

## Initialize lists
idd_var <- idd_std <-
  pca <- pca_runtime <-
  rf_var <- rf_var_runtime <- rf_var_acc <-
  tsne_var <- tsne_var_runtime <-
  rf_pc <- rf_pc_runtime <- rf_pc_acc <-
  tsne_pc <- tsne_pc_runtime <-
  rf_std <- rf_std_runtime <- rf_std_acc <-
  tsne_std <- tsne_std_runtime <-
  rf_stdpc <- rf_stdpc_runtime <- rf_stdpc_acc <-
  tsne_stdpc <- tsne_stdpc_runtime <-
  list()

## Loop over data ----
for(i in 1:length(dat)){
  ## _Initialize data views ---- 
  ## d_var, d_std, d_pc, d_stdpc
  c <- clas[[i]]
  d_var <- dat[[i]]
  d_std <- spinifex::scale_sd(d_var)
  idd_var[[i]] <- ceiling(max(na.rm = T,
                              #Rdimtools::est.Ustat(d_var)$estdim, # convergence rate of U-statistic on manifold
                              Rdimtools::est.correlation(d_var)$estdim, # correlation dimension
                              Rdimtools::est.made(d_var)$estdim, # manifold-adaptive dimension estimation
                              Rdimtools::est.mle2(d_var)$estdim, # MLE with Poisson process
                              Rdimtools::est.twonn(d_var)$estdim # minimal neigh
  ))
  idd_std[[i]] <- ceiling(max(na.rm = T, ## estimates do changes based on scale_sd
                          Rdimtools::est.Ustat(d_std)$estdim, # convergence rate of U-statistic on manifold
                          Rdimtools::est.correlation(d_std)$estdim, # correlation dimension
                          Rdimtools::est.made(d_std)$estdim, # manifold-adaptive dimension estimation
                          Rdimtools::est.mle2(d_std)$estdim, # MLE with Poisson process
                          Rdimtools::est.twonn(d_std)$estdim # minimal neigh
  ))
  d_pc <- prcomp(d_var)$x[, 1:idd_var[[i]]]
  d_stdpc <- prcomp(d_std)$x[, 1:idd_std[[i]]]
  
  ## _Random forest ----
  ## *_var
  start_time <- Sys.time()
  rf_var[[i]] <- randomForest(x = d_var, y = c, ntree = 1000, do.trace = TRUE,
                          keep.forest = FALSE, importance = TRUE)
  rf_var_runtime[[i]] <- Sys.time() - start_time
  rf_var_acc[[i]] <- 1 - median(rf_var[[i]]$err.rate[, 1]) ## 1 - median(OOB error rate)
  ## *_std
  start_time <- Sys.time()
  rf_std[[i]] <- randomForest(x = d_std, y = c, ntree = 1000, do.trace = TRUE,
                              keep.forest = FALSE, importance = TRUE)
  rf_std_runtime[[i]] <- Sys.time() - start_time
  rf_std_acc[[i]] <- 1 - median(rf_std[[i]]$err.rate[, 1]) ## 1 - median(OOB error rate)
  ## *_pc
  start_time <- Sys.time()
  rf_pc[[i]] <- randomForest(x = d_pc, y = c, ntree = 1000, do.trace = TRUE,
                              keep.forest = FALSE, importance = TRUE)
  rf_pc_runtime[[i]] <- Sys.time() - start_time
  rf_pc_acc[[i]] <- 1 - median(rf_pc[[i]]$err.rate[, 1]) ## 1 - median(OOB error rate)
  ## *_stdpc
  start_time <- Sys.time()
  rf_stdpc[[i]] <- randomForest(x = d_stdpc, y = c, ntree = 1000, do.trace = TRUE,
                              keep.forest = FALSE, importance = TRUE)
  rf_stdpc_runtime[[i]] <- Sys.time() - start_time
  rf_stdpc_acc[[i]] <- 1 - median(rf_stdpc[[i]]$err.rate[, 1]) ## 1 - median(OOB error rate)
  
  
  ##
  beepr::beep(4)
  ##
  #### tsne_space ----
  
 
}
