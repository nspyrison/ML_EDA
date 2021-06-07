require("DALEX")
require("treeshap")
require("spinifex")
require("tourr")
require("ggplot2")

is.discrete <- function(x) ## see plyr::is.discrete(). !! not on levels, class only
  {is.factor(x) || is.character(x) || is.logical(x)}


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
    }
  }else{ ## basis_type is NULL; format parts local attributes
    .cheem_bas <- as.matrix(tourr::normalise(.df_la$median_local_attr))
    colnames(.cheem_bas) <- parts_type
    rownames(.cheem_bas) <- .df_la$variable_name
  }
    
  ## Keep attributes
  attr(.cheem_bas, "class") <- c("cheem_basis", "matrix")
  attr(.cheem_bas, "data_else") <- as.matrix(data_else)
  attr(.cheem_bas, "data_oos")  <- as.matrix(data_oos)
  attr(.cheem_bas, "class_else") <- class_else ## Can't call it "class" b/c matrix/df.
  attr(.cheem_bas, "class_oos")  <- class_oos
  attr(.cheem_bas, "predict_parts") <- .parts
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
  .oos_proj <- data.frame(.data_oos %*% cheem_basis)
  .a_args <- list() ## Init
  
  ## 2D geom_point and call over oos_args:
  if(ncol(cheem_basis) == 2L){
    .oos_pt_func <- function(...)
      suppressWarnings(geom_point(
        aes_string(x = .cn[1L], y = .cn[2L]), 
        .oos_proj, ...))
    .oos_pt_call <- .oos_pt_func(oos_identity_args)
    .oos_pt_call <- do.call(.oos_pt_func, oos_identity_args)
    
    ## 2D data ggproto
    if(is.null(.class_else) == FALSE) 
      .a_args <- list(color = .obs_class, shape = .obs_class)
    .ggp_data <- list(ggproto_data_points(aes_args = .a_args),
                      ggproto_basis_axes(),
                      labs(x = .cn[1L], y = .cn[2L]),
                      .oos_pt_call)
  }
  
  ## 1D geom_hist over oos args:
  if(ncol(cheem_basis) == 1L){
    .oos_rug_func <- function(...)
      suppressWarnings(geom_rug(
        aes_string(x = .cn[1L]), .oos_proj, ...))
    .oos_rug_call <- do.call(.oos_rug_func, oos_identity_args)
    
    ## 1D data ggproto
    if(is.null(.class_else) == FALSE) 
      .a_args <- list(color = .obs_class, fill = .obs_class)
    .ggp_data <- list(ggproto_data_density1d_rug(aes_args = .a_args),
                      ggproto_basis_axes1d(),
                      labs(x = .cn[1L]),
                      .oos_rug_call)
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
#' dat <- spinifex::scale_sd(tourr::flea[, 1:6])
#' tgt_var <- flea$species
#' 
#' la_mat <- local_attribution_matrix_INSAMP_DOPAR(data = dat, target_var = tgt_var)
#' GGally::ggpairs(as.data.frame(la_mat), mapping = aes(color = tgt_var))
local_attribution_matrix_INSAMP_DOPAR <- function(
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
  
  if(is.discrete(target_var) & length(unique(target_var)) > 2L)
    stop("Not expecting multiclass discrete target variable, try looping over target variable testing against each level.")
  .RF_mtry <- if(is.discrete(tgt_var)) sqrt(.p) else .p / 3L
  #### Random forest, of hold one out data
  .rf <- randomForest::randomForest(target_var~.,
                                    data = data.frame(target_var, data),
                                    mtry = .RF_mtry)
  #### DALEX::predict_parts, (of DALEX::explain()) of that Random forest
  .ex_rf <- DALEX::explain(model = .rf,
                           data = data,
                           y = target_var,
                           label = paste0(parts_type, " local attribution of random forest model"))
  
  #### Iterating over each each observation:
  ret <- list(NULL)
  doParallel::registerDoParallel(cores = n_cores) #in windows by default 2, in general n-1
  .r_idx <- 1:.n

  ## Parallelized for each row num -----
  ret <- foreach::foreach(i = .r_idx,
                 .packages = c("DALEX", "treeshap", "spinifex", "tourr"),
                 .combine = rbind,
  ) %dopar% {
    .parts <- DALEX::predict_parts(explainer = .ex_rf,
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
    
    if(i %% 10 == 0) print(paste0("Done with i = ", i, " of ", nrow(data)))
    ## Keep attributes and assign
    attr(.obs_local_attr, "predict_parts") <- .parts
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



#' @example
#' ## Discrete supervised classification
#' dat <- spinifex::scale_sd(tourr::flea[, 1:6])
#' ## classification on discrete, or regression on continuous var not in data.
#' tgt_var <- flea$species
#' ## Must be same named data.frame for DALEX::predict_parts
#' tgt_obs <- dat[10,, drop = FALSE] 
#' 
#' 
#' ## Create a tree-based model
#' .p <- ncol(dat)
#' .rf <- randomForest::randomForest(tgt_var~.,
#'                                   data = data.frame(tgt_var, dat),
#'                                   mtry = if(is.discrete(tgt_var)) sqrt(.p) else .p / 3L)
#' ## Explainer of the random forest model
#' expl <- DALEX::explain(model = .rf,
#'                        data = dat,
#'                        y = tgt_var,
#'                        label = "local attribution of random forest model")
#' 
#' system.time(
#'   cheem_bas <- basis_cheem_INSAMP(expl, tgt_obs)
#' )[3] ## .14 sec
#' 
#' print(cheem_bas) ## method, print.basis_cheem_INSAMP
#' (gg <-  plot(cheem_bas)) ## method, plot.basis_cheem_INSAMP
#' 
#' if(F)
#'   ggsave("cheem_plot.pdf", gg, device ="pdf", width = 8, height = 4, units="in")
basis_cheem_INSAMP <- function(
  explainer, 
  new_observation,
  parts_type = c("shap", "break_down", "oscillations", "oscillations_uni", "oscillations_emp"),
  parts_B = 10,
  parts_N = if(substr(parts_type, 1, 4) == "osci") 500 else NULL, ## See DALEX::predict_parts
  ...
){
  ## Assumptions
  data <- explainer$data
  y    <- explainer$y
  
  #### DALEX::predict_parts, (of DALEX::explain()) of that Random forest
  parts_type <- match.arg(parts_type)
  .parts <- DALEX::predict_parts(explainer = explainer, ## Takes some time.
                                 new_observation = as.data.frame(new_observation),
                                 type = parts_type,
                                 N = parts_N,
                                 B = parts_B,
                                 ...)
  ## Reduces size by ~90%, without hurting plot.predict_parts
  attr(.parts, "yhats_distribution") <- NULL
  
  ## basis_type is NULL; format parts local attributes
  .cheem_bas <- as.matrix(tourr::normalise(.df_la$median_local_attr))
  colnames(.cheem_bas) <- parts_type
  rownames(.cheem_bas) <- .df_la$variable_name
  
  ## Keep attributes
  attr(.cheem_bas, "class")           <- c("basis_cheem_INSAMP", "matrix")
  attr(.cheem_bas, "data")            <- data ## Expected as matrix 
  attr(.cheem_bas, "y")               <- y
  attr(.cheem_bas, "new_observation") <- new_observation
  attr(.cheem_bas, "predict_parts")   <- .parts
  ## Return
  return(.cheem_bas)
}

## Print basis_cheem_INSAMP as a numeric matrix without showing all the attributes.
print.basis_cheem_INSAMP <- function (x, ...){
  attr(x, "data") <- NULL
  attr(x, "y") <- NULL
  attr(x, "new_observation")<- NULL
  attr(x, "predict_parts") <- NULL
  NextMethod()
}

## plot basis_cheem_INSAMP as 1d density, next to plot(.parts)
autoplot.basis_cheem_INSAMP <- plot.basis_cheem_INSAMP <- function(
  cheem_basis,
  show_parts = TRUE,
  new_obs_identity_args =
    if(ncol(cheem_basis) >= 2){list(color = "red", size = 5L, shape = 8L)}else
      list(color = "red", size = 1.5, linetype = 2L, length = unit(1, "npc"), alpha = .5),
  aes_class = NULL,
  ... ## Passed to plot.predict_parts()
){
  data <- attributes(cheem_basis)$data
  new_observation <- attributes(cheem_basis)$new_observation
  if(is.null(aes_class) & is.discrete(attributes(cheem_basis)$y) == TRUE)
    aes_class <- as.factor(attributes(cheem_basis)$y)
  .cn <- colnames(cheem_basis)
  
  ## Initialize ggplot_tour()
  gg <- ggplot_tour(basis_array = cheem_basis, data = .data)
  ## new obs projection
  .ooo_proj <- data.frame(as.matrix(new_observation) %*% cheem_basis)
  .a_args <- list() ## Init
  if(is.null(aes_class) == FALSE) .a_args <- list(color = aes_class, shape = aes_class)
  
  ## 2D geom_point:
  if(ncol(cheem_basis) == 2L){
    ## 2D new obs point
    .new_obs_pt_func <- function(...)
      suppressWarnings(geom_point(
        aes_string(cheem_basis = .cn[1L], y = .cn[2L]), .new_obs_proj, ...))
    .new_obs_pt_call <- .new_obs_pt_func(new_obs_identity_args)
    .new_obs_pt_call <- do.call(.new_obs_pt_func, new_obs_identity_args)
    
    ## 2D combined data layer
    if(is.null(.obs_class) == FALSE)
      .a_args <- list(color = .obs_class, shape = .obs_class)
    .ggp_data <- list(ggproto_data_points(aes_args = .a_args),
                      ggproto_basis_axes(),
                      labs(x = .cn[1L], y = .cn[2L]),
                      .new_obs_pt_call)
  }
  
  ## 1D geom_hist:
  if(ncol(cheem_basis) == 1L){
    ## 1D new obs rug
    .new_obs_rug_func <- function(...)
      suppressWarnings(geom_rug(
        aes_string(x = .cn[1L]), .new_obs_proj, ...))
    .new_obs_rug_call <- do.call(.new_obs_rug_func, new_obs_identity_args)
    
    ## 1D combined data layer
    if(is.null(.obs_class) == FALSE)
      .a_args <- list(color = .obs_class, fill = .obs_class)
    .ggp_data <- list(ggproto_data_density1d_rug(aes_args = .a_args),
                      ggproto_basis_axes1d(),
                      labs(x = .cn[1L]),
                      .new_obs_rug_call)
  }
  
  ## Bring ggprotos together
  gg <- gg +
    ggproto_data_background(gridlines = FALSE) +
    .ggp_data +
    theme(axis.title = element_text())
  
  ## Append plot(predict_parts()), if needed
  if(show_parts == TRUE){
    require("patchwork")
    .parts <- attributes(cheem_basis)$predict_parts
    gg <- gg + plot(.parts, ...)
  }
  
  return(gg)
}
