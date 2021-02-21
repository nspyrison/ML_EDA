# 1) Initialize -----
{
  require("Rtsne")
  require("randomForest")
  require("tictoc")
  require("magrittr")
  source("./apps/poc/ggproto_screeplot_pca.r")
  
  ## _Set up data-----
  mlb_dat_nms <- data(package = "mlbench")$results[, 3L]
  mlb_dat_ls <- list()
  mute <- sapply(seq_along(mlb_dat_nms), function(i){
    data(list = mlb_dat_nms[i], package = "mlbench")
    mlb_dat_ls[[i]] <<- get(mlb_dat_nms[i])
  })
  names(mlb_dat_ls) <- mlb_dat_nms
  
  bh2 <- mlb_dat_ls$BostonHousing2
  bh2$chas <- ifelse(bh2$chas == 1, 1, 0)
  iono <- mlb_dat_ls$Ionosphere
  iono$V1 <- ifelse(iono$V1 == 1, 1, 0)
  
  ## List of data sets
  dat_nms <- c("wine", "BreastCancer", "PimaIndiansDiabetes_long", 
               "PimaIndiansDiabetes_wide", "BostonHousing2", "Glass", "Ionosphere",
               "LetterRecognition", "Ozone", "Satellite", "Shuttle", "Sonar",
               "Vehicle", "Vowel", "Zoo")
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
  
  ## Initialize loop lists
  idd_var <- idd_std <- idd_pc <- idd_stdpc <-
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
  
  idd_agg <- function(vec){round(max(mean(vec, na.rm = TRUE), median(vec, na.rm = TRUE)), 0)}
}

# 2) Loop over data ----
for(i in 1:7){
  #if(i == 4)browser()
  # 7:length(dat)){
  ## _Initialize data views ----
  ## d_var, d_std, d_pc, d_stdpc
  c <- clas[[i]]
  d_var <- dat[[i]]
  d_std <- spinifex::scale_sd(d_var)
  idd_var[[i]] <- est_idd_vec(data = d_var, inc_slow = FALSE)
  idd_std[[i]] <- est_idd_vec(data = d_std, inc_slow = FALSE)
  
  d_pc <- prcomp(d_var)$x[, 1:cm(idd_var[[i]])]
  d_stdpc <- prcomp(d_std)$x[, 1:cm(idd_std[[i]])]
  idd_pc[[i]] <- est_idd_vec(data = d_pc, inc_slow = FALSE)
  idd_stdpc[[i]] <- est_idd_vec(data = d_stdpc, inc_slow = FALSE)
  
  # _Random forest ----
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
  
  
  # _tsne_space ----
  
}


# 3) Analyze -----
{
  ## _IDD ----
  setup_idd_tib <- function(suffix = "var"){
    idd_obj <- get(paste0("idd_", suffix))
    m <- matrix(nrow = 1:7, ncol = length(idd_obj[[1]]) + 2)
    df <- as.data.frame(m)
    mute <- sapply(1:length(idd_std), function(i){
      vec <- idd_obj[[i]]
      df[i,] <<- c(dat_nms[i], vec, idd_agg(vec))
    })
    colnames(df) <- paste0(c("dataset", names(idd_obj[[1]]), "idd_used - rmmm"), "_", suffix)
    mute <- sapply(c(2:8), function(i){
      df[, i] <<- round(as.numeric(df[,i]), 3)
    })
    tibble::as_tibble(df)
  }
  tib_var   <- setup_idd_tib("var")
  tib_std   <- setup_idd_tib("std")
  tib_pc    <- setup_idd_tib("pc")
  tib_stdpc <- setup_idd_tib("stdpc")
  tib_idd_out <- cbind(tib_var, 
                       tib_std[, 2:8],
                       tib_pc[, 2:8], 
                       tib_stdpc[, 2:8])
  if(F)
    readr::write_excel_csv(tib_idd_out, "./comparison/output/tib_idd_out.csv") ## to go to latex table.
  
  
}
