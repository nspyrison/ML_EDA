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
## Run Big Five -----
{
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
} ## END OF BIG_FIVE