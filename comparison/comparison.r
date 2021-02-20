require("Rtnse")
require("randomForest")
require("tictoc")
require("magrittr")
source("./apps/poc/ggproto_screeplot_pca.r")
## Scaling performance eval of variable vs PC spaces
#data(package = "mlbench")

## Cleaning Big five ----
if(F){
  require("tidyverse")
  z <- readr::read_tsv("./data/big_five/data.tsv")
  attr(z, "problems") <- NULL ## Null data, try to 
  #skim_z <- skimr::skim(z)
  z <- z %>% 
    dplyr::filter(IPC == 1) %>% ## suggested filter from data providers.
    tidyr::drop_na() %>%
    dplyr::select(-lat_appx_lots_of_err, -long_appx_lots_of_err, -IPC, -screenw, -screenh)
  z <- type.convert(z)
  z$dateload <- lubridate::as_datetime(z$dateload)
  data_cleaned <- z
  readr::write_rds(data_cleaned, "./data/big_five/data_cleaned.rds")
}
if(F){ ## filter rows to top 8 responding countries
  require("tidyverse")
  data_cleaned <- readr::read_rds("./data/big_five/data_cleaned.rds")
  t_cntry <- table(data_cleaned$country)
  t_cntry <- t_cntry[order(t_cntry, decreasing = TRUE)] ## Order desc
  tgt_cntry <- names(t_cntry[1:8])
  data_cleaned_top8 <- data_cleaned %>% dplyr::filter(country %in% tgt_cntry)
  readr::write_rds(data_cleaned_top8, "./data/big_five/data_cleaned_top8.rds")
}
data_cleaned_top8 <- readr::read_rds("./data/big_five/data_cleaned_top8.rds")
dat <- dplyr::select(data_cleaned_top8, -country)
dat$dateload <- as.numeric(dat$dateload) ## POSIXct numeric, seconds past 1 January 1970
#dat$dateload <- dat$dateload - min(dat$dateload)
clas <- factor(data_cleaned_top8$country, levels = unique(data_cleaned_top8$country))
summary(dat)

{tic("prcomp")
pca_obj <- prcomp(dat)
toc()}
df_scree <- df_scree_pca(pca_obj)

# {tic("idd_vec")
# est_idd_vec(dat) ## Error: Cannot allocate vector of size 1002.0 Gb
# toc()}

# {tic("RF")
# randomForest(x = dat, y = clas, ntree = 1000, do.trace = TRUE,
#              keep.forest = FALSE, importance = TRUE) ## get like 
# toc()}

pca_dat <- pca_obj$x[, 1:4]
{tic("RF on PCA[1:4]") ~ 1 tree/sec
rf <- randomForest(x = pca_dat, y = clas, ntree = 1000, do.trace = TRUE,
             keep.forest = FALSE, importance = TRUE) ## get like
toc()}
rf <- .Last.value

## Scaling acrross data -----
mlb_dat_nms <- data(package = "mlbench")$results[, 3L]
mlb_dat_ls <- list()
mute <- sapply(seq_along(mlb_dat_nms), function(i){
  data(list = mlb_dat_nms[i], package = "mlbench")
  mlb_dat_ls[[i]] <<- get(mlb_dat_nms[i])
})
names(mlb_dat_ls) <- mlb_dat_nms
# not servo, or soybean, PimaIndiansDiabetes , PimaIndiansDiabetes2, 


raw_dat <- list(
  spinifex::BreastCancer[, 2:10]
)


bh2 <- mlb_dat_ls$BostonHousing2
bh2$chas <- ifelse(bh2$chas == 1, 1, 0)
iono <- mlb_dat_ls$Ionosphere
iono$V1 <- ifelse(iono$V1 == 1, 1, 0)

## List of data sets
dat <- list(
  spinifex::wine[, 2:12],
  spinifex::BreastCancer[, 2:10],
  spinifex::PimaIndiansDiabetes_long[, 1:6],
  spinifex::PimaIndiansDiabetes_wide[, 1:8],
  bh2[, c(2:4, 5:19)],
  mlb_dat_ls$Glass[, 1:9],
  iono[, c(1, 3:34)],
  mlb_dat_ls$LetterRecognition[, 2:17],
  mlb_dat_ls$Ozone[, c(1:3, 5:13)],
  mlb_dat_ls$Satellite[, 1:36],
  mlb_dat_ls$Shuttle[, 1:9],
  mlb_dat_ls$Sonar[, 1:60],
  mlb_dat_ls$Vehicle[, 1:18],
  mlb_dat_ls$Vowel[, 1:10],
  mlb_dat_ls$Zoo[, 1:16]
)
clas <- list(
  spinifex::wine$Type,
  spinifex::BreastCancer$Class,
  spinifex::PimaIndiansDiabetes_long$diabetes,
  spinifex::PimaIndiansDiabetes_wide$diabetes,
  bh2$medv,
  mlb_dat_ls$Glass$Type,
  iono$Class,
  mlb_dat_ls$LetterRecognition$lettr,
  mlb_dat_ls$Ozone$V4,
  mlb_dat_ls$Satellite$classes,
  mlb_dat_ls$Shuttle$Class,
  mlb_dat_ls$Sonar$Class,
  mlb_dat_ls$Vehicle$Class,
  mlb_dat_ls$Vowel$Class,
  mlb_dat_ls$Zoo$type
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
for(i in 7:length(dat)){
  ## _Initialize data views ---- 
  ## d_var, d_std, d_pc, d_stdpc
  
  #if(i == 7) browser()
  
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
  
  
  ###
  cat(paste0(i, " is done"))
  #beepr::beep(4)
  ###
  
  
  #### tsne_space ----
}
