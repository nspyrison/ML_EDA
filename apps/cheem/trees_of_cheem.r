require("DALEX")
require("treeshap")
require("spinifex")
require("tourr")
require("ggplot2")

is.discrete <- function(x) ## see plyr::is.discrete(). !! not on levels, class only
  is.factor(x) || is.character(x) || is.logical(x)

## Remade from: iBreakDown:::print.break_down_uncertainty
## Create the scree df for the local attribution from a DALEX::predict_parts return.
## !!may have overlap with iBreakDown:::plot.break_down_uncertainty.
df_scree_local_attr <- function(x, ...){ ## x should be a predict_parts() return
  ret <- data.frame(
    label = tapply(x$label, paste(x$label, x$variable, sep = ": "), unique, na.rm = TRUE),
    variable_name = tapply(x$variable_name, paste(x$label, x$variable, sep = ": "), unique, na.rm = TRUE),
    variable_value = tapply(x$variable_value, paste(x$label, x$variable, sep = ": "), unique, na.rm = TRUE), ## oos variable value
    median_local_attr = tapply(x$contribution, paste(x$label, x$variable, sep = ": "), median, na.rm = TRUE)
  )
  
  ## Reorder
  ret <- ret[order(abs(ret$median_local_attr), decreasing = TRUE), ]
  ## Add cumsum_rate
  ret$cumsum_rate_abs_median_local_attr <- cumsum(abs(ret$median_local_attr)) / sum(abs(ret$median_local_attr))
  return(ret) ## NOTE: return is in scree order not colname(data) order.
}

#' @example
#' ## Discrete supervised classification
#' dat <- spinifex::scale_sd(tourr::flea[, 1:6])
#' clas <- flea$species
#' tgt_obs <- 10 ## row number in 1:nrow(dat)
#' tgt_var <- clas == clas[tgt_obs] ## Or regression on a continuous var not in data.
#' 
#' basis_cheem(dat, tgt_obs, tgt_var, clas, basis_type = NULL)
#' 
#' ## Continuous regression, no class aesthetics
#' dat <- dplyr::select(DALEX::fifa, -c("nationality", "potential", "value_eur", "wage_eur"))
#' set.seed(123)
#' .r_idx <- sample(1:nrow(dat), 1000)
#' dat <- dat[.r_idx, ]
#' dat <- scale_sd(dat)
#' y_var <- DALEX::fifa$overall[.r_idx] ## An aggregate skill measure between 1 and 100.
#' 
#' #### Find a outlier to look at
#' .maha <- mahalanobis(dat, colMeans(dat), cov(dat))
#' .maha <- sort(.maha, decreasing = T)
#' head(.maha, n = 6L) ## "I. Pettersson", 4th largest mahalonobis dist, google: a Sweddish goalkepper
#' .tgt_name <- "I. Pettersson"
#' tgt_row <- which(row.names(dat) == .tgt_name)
#' 
#' basis_cheem(data = dat, holdout_rownum = tgt_row, target_var = y_var,
#'             parts_type = "shap", basis_type = "pca")
## Previously hard coded classification target var was: class == new_observation_class
basis_cheem <- function(
  data, holdout_rownum, target_var, class = NULL,
  parts_type = c("shap", "break_down", "oscillations", "oscillations_uni", "oscillations_emp"),
  parts_B = 10,
  parts_N = if(substr(parts_type, 1, 4) == "osci") 500 else NULL, ## see DALEX::predict_parts
  basis_type = c("none", NULL, "RF_importance", "pca", "olda", "odp", "onpp"),
  keep_large_intermediates = FALSE,
  ...
){
  ## Assumptions
  requireNamespace("randomForest")
  requireNamespace("DALEX")
  #requireNamespace("treeshap") ## Not explicitly used, maybe implicitly called in DALEX?
  data <- as.data.frame(data)
  ## Initialize held out observation for data, target var, optional class var
  data_oos  <- data[holdout_rownum,, drop = FALSE] ## drop = FALSE retains data.frame rather than coerce to vector.
  data_else <- data[-holdout_rownum, ]
  target_var_oos  <- target_var[holdout_rownum]
  target_var_else <- target_var[-holdout_rownum]

  ## If class is used
  class_oos <- class_else <- NULL ## Initialize
  if(is.null(class) == FALSE){
    class <- as.factor(class)
    class_oos  <- class[holdout_rownum]
    class_else <- class[-holdout_rownum]
  }else{
    ## If class is null, enforce correct basis functions
    if(is.null(basis_type) == FALSE){
      basis_type <- match.arg(basis_type)
      if(basis_type %in% c("olda", "odp"))
        stop(paste0("basis_type ", basis_type, " requires the 'class' argument."))
    }
  }
  
  #### Random forest, with holdout_rownum removed
  .p <- ncol(data)
  ## Discrete wants mtry = sqrt(p), continuous wants mtry = p/3
  .RF_mtry <- if(is.discrete(target_var_else)) sqrt(.p) else .p / 3L
  .do_clac_imp <- FALSE ## Init
  if(!is.null(basis_type)) if(basis_type == "RF_importance") .do_clac_imp <- TRUE
  .rf <- randomForest::randomForest(target_var_else~.,
                                    data = data.frame(target_var_else, data_else),
                                    mtry = .RF_mtry,
                                    importance = .do_clac_imp)
  
  #### DALEX::predict_parts, (of DALEX::explain()) of that Random forest
  parts_type <- match.arg(parts_type)
  .ex_rf <- DALEX::explain(
    model = .rf,
    data = data_else,
    y = target_var_else,
    label = paste0(parts_type, " local attribution of random forest model"))
  .parts <- DALEX::predict_parts(explainer = .ex_rf, ## Takes some time.
                                 new_observation = data_oos,
                                 type = parts_type,
                                 N = parts_N,
                                 B = parts_B,
                                 ...)
  if(keep_large_intermediates == FALSE)
    attr(.parts, "yhats_distribution") <- NULL ## remove ~90% the size, without hurting plot.predict_parts
  attr(.cheem_bas, "predict_parts") <- .parts
  
  #### The local attribution of those parts [1, p] vector in SHAP order.
  ## Remade from: iBreakDown:::print.break_down_uncertainty
  .df_la <- data.frame(
    label = tapply(.parts$label, paste(.parts$label, .parts$variable, sep = ": "), unique, na.rm = TRUE),
    variable_name = tapply(.parts$variable_name, paste(.parts$label, .parts$variable, sep = ": "), unique, na.rm = TRUE),
    variable_value = tapply(.parts$variable_value, paste(.parts$label, .parts$variable, sep = ": "), unique, na.rm = TRUE), ## oos variable value
    median_local_attr = tapply(.parts$contribution, paste(.parts$label, .parts$variable, sep = ": "), median, na.rm = TRUE)
  )
  ## Reorder .df_la back to original data colname order
  .row_idx <- order(match(.df_la$variable_name, colnames(data)))
  .df_la   <- .df_la[.row_idx, ]
  
  # .la_r_fct_lvl <- gsub(".*\\.","", .parts$label)
  # .fct_lvl <- if(length(unique(.la_r_fct_lvl)) == 1L){""}else{.la_r_fct_lvl}
  ## What if factor was tested instead of boolean?
  .guess_bas.dat_ratio <- nrow(.df_la) / ncol(data)
  if(.guess_bas.dat_ratio != 1L)
    stop(paste0(
      "Local attribution has ", .guess_bas.dat_ratio,
      " times as many rows as the columns of the data. Make sure you didn't want to test a boolean such as tgt_var <- clas == clas[new_obs]. If you need this loop over a test for each factor level."))
   # paste0(substitute(class), "=", gsub(".*\\.","", .parts$label))
  
  #### Basis of a global feature (holdout_rownum removed), to use as second dim of projection
  if(is.null(basis_type) == FALSE){
    if(basis_type != "none"){
      basis_type <- match.arg(basis_type)
      .bas <- switch(basis_type,
                     RF_importance = randomForest::importance(.rf, type = 1L),
                     pca  = basis_pca(data_else),
                     olda = basis_olda(data_else, class_else),
                     odp  = basis_odp(data_else, class_else),
                     onpp = basis_onpp(data_else), ## Using default hyperparameters
                     stop("basis_type expects 'RF_importance', 'pca', 'olda', 'odp', 'onpp' or NULL.")
      )
      
      #### Bring them together and orthonormalize
      .cheem_bas <- cbind(.df_la$median_local_attr, .bas)[, 1L:2L]
      .cheem_bas <- as.matrix(tourr::orthonormalise(.cheem_bas))
      colnames(.cheem_bas) <- c(parts_type, paste0(basis_type, "1"))
    }else{ ## basis_type is NULL; format parts local attributes
      .cheem_bas <- as.matrix(tourr::normalise(.df_la$median_local_attr))
      colnames(.cheem_bas) <- parts_type
      rownames(.cheem_bas) <- .df_la$variable_name
    }
  }
    
  ## Keep attributes
  attr(.cheem_bas, "class") <- c("cheem_basis", "matrix")
  attr(.cheem_bas, "data_else") <- as.matrix(data_else)
  attr(.cheem_bas, "data_oos")  <- as.matrix(data_oos)
  attr(.cheem_bas, "class_else") <- class_else ## Can't call it "class" b/c matrix/df.
  attr(.cheem_bas, "class_oos")  <- class_oos
  if(keep_large_intermediates == TRUE){
    attr(.cheem_bas, "randomForest") <- .rf
    attr(.cheem_bas, "explain")      <- .ex_rf
  }
  
  ## Return
  return(.cheem_bas)
}

## Print cheem_bases as a numeric matrix without showing all the attributes.
print.cheem_basis <- function (x, ...){
  attr(x, "data_else") <- NULL
  attr(x, "data_oos")  <- NULL
  attr(x, "class_else") <- NULL
  attr(x, "class_oos")  <- NULL
  attr(x, "randomForest")  <- NULL
  attr(x, "explain")       <- NULL
  attr(x, "predict_parts") <- NULL
  NextMethod()
}


#' @example
#' dat <- spinifex::scale_sd(flea[, 1:6])
#' clas <- flea$species
#' tgt_obs <- 10 ## row number in 1:nrow(dat)
#' tgt_var <- clas == clas[tgt_obs] ## Or regression on a continuous var not in data.
#' 
#' bas_cheem <- basis_cheem(dat, tgt_obs, tgt_var, clas, basis_type = "olda")
#' 
#' (ggcheem_proj <-
#'   view_cheem(bas_cheem))
#' 
#' view_cheem(bas_cheem, show_boxplots = FALSE, max_features = 4)
#' 
#' if(F)
#'   ggsave("PoC_view_cheem.pdf", ggcheem_proj, device ="pdf", width = 6, height = 3, units="in")
view_cheem <- autoplot.cheem_basis <- plot.cheem_basis <- function(
  cheem_basis, show_parts = TRUE,
  oos_identity_args =
    if(ncol(cheem_basis) >= 2){list(color = "red", size = 5L, shape = 8L)}else
      list(color = "red", size = 1.5, linetype = 2L, length = unit(1, "npc"), alpha = .5),
  ...){ ## Passed to plot.predict_parts()
  .data_else  <- attributes(cheem_basis)$data_else
  .data_oos   <- attributes(cheem_basis)$data_oos
  .class_else <- attributes(cheem_basis)$class_else
  .class_oos  <- attributes(cheem_basis)$class_oos
  .cn <- colnames(cheem_basis)
  
  ## Initialize ggplot_tour()
  gg <- ggplot_tour(basis_array = cheem_basis,data = .data_else)
  
  ## oos projection not done in view_frames
  .proj_new_obs <- data.frame(.data_oos %*% cheem_basis)
  
  ## 2D geom_point and call over oos_args:
  if(ncol(cheem_basis) == 2L){
    .oos_pt_func <- function(...)
      suppressWarnings(geom_point(
        aes_string(x = .cn[1L], y = .cn[2L]), 
        .proj_new_obs, ...))
    .oos_pt_call <- .oos_pt_func(oos_identity_args)
    .oos_pt_call <- do.call(.oos_pt_func, oos_identity_args)
    
    ## 2D data ggproto
    .ggp_data <- list(ggproto_data_points(
      aes_args = list(color = .class_else, shape = .class_else)),
      ggproto_basis_axes(),
      labs(x = .cn[1L], y = .cn[2L]),
      .oos_pt_call
    )
  }
  
  ## 1D geom_hist over oos args:
  if(ncol(cheem_basis) == 1L){
    .oos_rug_func <- function(...)
      suppressWarnings(geom_rug(
        aes_string(x = .cn[1L]), .proj_new_obs, ...))
    .oos_rug_call <- do.call(.oos_rug_func, oos_identity_args)
    
    ## 1D data ggproto
    .ggp_data <- list(ggproto_data_density1d_rug(
      aes_args = list(color = .class_else, fill = .class_else)),
      ggproto_basis_axes1d(),
      labs(x = .cn[1L]),
      .oos_rug_call
    )
  }
  
  ## spinifex::view_frame(data_else)
  gg <- gg +
    ggproto_data_background(gridlines = FALSE) +
    .ggp_data +
    theme(axis.title = element_text())
  
  ## Plot(predict_parts(), by patchwork?
  if(show_parts == TRUE){
    require("patchwork")
    .parts <- attributes(cheem_basis)$predict_parts
    gg <- gg + plot(.parts, ...)
  }
  
  return(gg)
}

#' @examples
#' ## Discrete supervised classification RF
#' dat <- spinifex::scale_sd(flea[, 1:6])
#' tgt_var <- flea$species
#' 
#' la_mat <- local_attribution_matrix(dat, tgt_var)
#' GGally::ggpairs(as.data.frame(la_mat), mapping = aes(color = tgt_var))
local_attribution_list <- function(
  data, target_var,
  parts_type = c("shap", "break_down", "oscillations", "oscillations_uni", "oscillations_emp"),
  parts_B = 10,
  parts_N = if(substr(parts_type, 1, 4) == "osci") 500 else NULL, ## see DALEX::predict_parts
  do_normalize_rows = TRUE,
  n_cores = parallel::detectCores() - 1,
  ...){
  ## Assumptions
  require("doParallel")
  is.discrete <- function(x) ## see plyr::is.discrete(). !! not on levels, class only
    is.factor(x) || is.character(x) || is.logical(x)
  data <- as.data.frame(data)
  
  ## Initialize
  parts_type <- match.arg(parts_type)
  .p <- ncol(data)
  .n <- nrow(data)
  
  if(is.discrete(target_var)){
    .target_var_else <- target_var[-i] == target_var[i]
    .RF_mtry <- sqrt(.p)
    .rf_ls <- .ex_rf_ls <- list()
    .u_tgt <<- unique(target_var)
    .mute <- sapply(1:length(.u_tgt), function(i){
      .rf_ls[[i]] <- randomForest::randomForest(target_var~.,
                                                data = data.frame(target_var == .u_tgt[i], data),
                                                mtry = .RF_mtry)
      .ex_rf_ls[[i]] <- DALEX::explain(model = .rf_ls[[i]],
                                       data = data,
                                       y = target_var == .u_tgt[i],
                                       label = paste0(parts_type, " local attribution of random forest model"))
    })
    names(.rf_ls) <- .u_tgt
  }else{ ## target_var_else is continuous
    .RF_mtry <- .p / 3L
    #### Random forest, of hold one out data
    .rf_ls <- list(randomForest::randomForest(target_var~.,
                                      data = data.frame(target_var, data),
                                      mtry = .RF_mtry))
    #### DALEX::predict_parts, (of DALEX::explain()) of that Random forest
    .ex_rf_ls <- list(DALEX::explain(model = .rf_ls[[1L]],
                                     data = data,
                                     y = target_var,
                                     label = paste0(parts_type, " local attribution of random forest model"))
    )
  }
  
  #### Iterating over each each observation:
  ret <- list(NULL)
  doParallel::registerDoParallel(cores = n_cores) #in windows by default 2, in general n-1
  .r_idx <- 1:.n

  ## Parallelized for each row num -----
  ret <- foreach::foreach(i = .r_idx,
                 .packages = c("DALEX", "treeshap", "spinifex", "tourr")
  ) %dopar% {
    .this_ex_idx <- 1L ## init
    if(is.discrete(target_var))
      .this_ex_idx <- which(.u_tgt == target_var[i])
    .parts <- DALEX::predict_parts(explainer = .ex_rf_ls[[.this_ex_idx]],
                                   new_observation =  data[i,, drop = FALSE],
                                   type = parts_type,
                                   N = parts_N,
                                   B = parts_B,
                                   ...)
    attr(.parts, "yhats_distribution") <- NULL ## Reduce ~90% the size, without hurting plot.predict_parts
    
    
    #### The local attribution of those parts [1, p] vector in SHAP order.
    ## Remade from: iBreakDown:::print.break_down_uncertainty
    .df_la <- data.frame(
      label = tapply(.parts$label, paste(.parts$label, .parts$variable, sep = ": "), unique, na.rm = TRUE),
      variable_name = tapply(.parts$variable_name, paste(.parts$label, .parts$variable, sep = ": "), unique, na.rm = TRUE),
      variable_value = tapply(.parts$variable_value, paste(.parts$label, .parts$variable, sep = ": "), unique, na.rm = TRUE), ## oos variable value
      median_local_attr = tapply(.parts$contribution, paste(.parts$label, .parts$variable, sep = ": "), median, na.rm = TRUE)
    )
    ## Reorder .df_la back to original data colname order
    .row_idx <- order(match(.df_la$variable_name, colnames(data)))
    .df_la   <- .df_la[.row_idx, ]
    
    ## Single obs of local attribution matrix
    .obs_local_attr <- .df_la$median_local_attr
    if(do_normalize_rows == TRUE)
      .obs_local_attr <- tourr::normalise(.obs_local_attr)
    colnames(.obs_local_attr) <- parts_type
    rownames(.obs_local_attr) <- .df_la$variable_name
    
    if(i %% 10 == 0) print(paste0("Done with i = ", i))
    ## Keep attributes and assign
    attr(.obs_local_attr, "predict_parts") <- .parts
    .obs_local_attr
  }
  
  ## Format
  doParallel::stopImplicitCluster()
  .rn <- rownames(data)
  if(is.null(.rn) == TRUE) .rn <- 1L:.n
  rownames(ret) <- paste0(parts_type, " of ", .rn)
  colnames(ret) <- colnames(data)
  attr(ret, "class") <- c("local_attribution_list", "list")
  
  ## Return
  return(ret)
}

### EMA paper examples, recreating source ----
#' @examples 
#' ## Working form source examples:
#' if(F) ## Working from:
#'   browseURL("http://ema.drwhy.ai/shapley.html#SHAPRcode")
#' 
#' titanic_imputed <- archivist::aread("pbiecek/models/27e5c")
#' titanic_rf <- archivist::aread("pbiecek/models/4e0fc")
#' henry <- archivist::aread("pbiecek/models/a6538")
#' 
#' library("randomForest")
#' library("DALEX")
#' 
#' ## Make a DALEX "explainer" of in smaple data
#' explain_rf <- DALEX::explain(model = titanic_rf,
#'                              data = titanic_imputed[, -9],
#'                              y = titanic_imputed$survived == "yes", 
#'                              label = "Random Forest")
#' ## Predict a single out of sample observation, "Henry"?
#' predict(explain_rf, henry)
#' 
#' tictoc::tic("shap_henry")
#' shap_henry <- predict_parts(explainer = explain_rf,  ## ~ 10 s @ B=25
#'                             new_observation = henry, 
#'                             type = "shap",
#'                             B = 10)
#' tictoc::toc()
#' plot(shap_henry, show_boxplots = FALSE)
#' 
#' print("note that iBreakDown:::print.break_down prints an agg tbl, not the 11 perms tested and desplayed when coerced to tibble.")
#' tib_shap_henry <- tibble::as.tibble(shap_henry) ## Note that SHAP is already showing only 7 of 77 branches.
#' hist(tib_shap_henry$contribution)
#' 
#' 
#' print("why isn't it showing the 7 largest contributions though??")
#' library("dplyr")
#' tib_shap_henry <- tib_shap_henry %>% arrange(desc(abs(contribution)))
#' tib_shap_henry
#' unique(tib_shap_henry$variable)
#' 
#' 
#' df_local_attr <- df_scree_local_attr(shap_henry)
#' v <- tourr::normalise(df_local_attr$median_local_attr)
