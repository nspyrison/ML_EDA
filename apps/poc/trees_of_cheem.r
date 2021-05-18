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
#' dat <- spinifex::scale_sd(flea[, 1:6])
#' clas <- flea$species
#' tgt_obs <- 10 ## row number in 1:nrow(dat)
#' tgt_var <- clas == clas[tgt_obs] ## Or regression on a continuous var not in data.
#' 
#' basis_cheem(dat, tgt_obs, tgt_var, clas, basis_type = "olda")
#' 
#' ## Continuous regression, no class aesthetics
#' dat <- dplyr::select(DALEX::fifa, -c("nationality", "potential", "value_eur", "wage_eur"))
#' dat <- scale_sd(dat)
#' y_var <- DALEX::fifa$overall ## An aggregate skill measure between 1 and 100.
#' 
#' #### Find a outlier to look at
#' .maha <- mahalanobis(dat, colMeans(dat), cov(dat))
#' .maha <- sort(.maha, decreasing = T)
#' head(.maha, n = 6L) ## "I. Pettersson", 4th largest mahalonobis dist, google: a Sweddish goalkepper
#' .tgt_name <- "I. Pettersson"
#' tgt_row <- which(row.names(dat) == .tgt_name)
#' 
#' basis_cheem(data = dat, holdout_rownum = tgt_row,target_var = y_var,
#'             parts_type = "shap", basis_type = "pca")
## Previously hard coded classification target var was: class == new_observation_class
basis_cheem <- function(data, holdout_rownum, target_var, class = NULL,
                        parts_type = c("shap", "break_down", "oscillations", "oscillations_uni", "oscillations_emp"),
                        parts_B = 10,
                        parts_N = if(substr(parts_type, 1, 4) == "osci") 500 else NULL, ## see DALEX::predict_parts
                        basis_type = c("pca", "olda", "odp", "onpp"), ...){
  ## Assumptions
  requireNamespace("randomForest")
  requireNamespace("DALEX")
  #requireNamespace("treeshap") ## Not explicitly used, maybe implicitly called in DALEX?
  data <- as.data.frame(data)
  ## Initialize held out observation for data, target var, optional class var
  data_oos  <- data[holdout_rownum,, drop = FALSE] ## drop = FALSE retains data.frame rather than coerce to vector.
  data_else <- data[-holdout_rownum, ]
  target_var_oos   <- target_var[holdout_rownum]
  target_var_else <- target_var[-holdout_rownum]
  ## If class is used
  class_oos <- class_else <- NULL ## Initialize
  if(is.null(class) == FALSE){
    class <- as.factor(class)
    class_oos   <- class[holdout_rownum]
    class_else <- class[-holdout_rownum]
  }else{
    ## If class is null, enforce correct basis functions
    basis_type <- match.arg(basis_type)
    if(basis_type %in% c("olda", "odp"))
      stop(paste0("basis_type ", basis_type, " requires the 'class' argument."))
  }
  
  #### Random forest, with holdout_rownum removed
  .p <- ncol(dat)
  ## Discrete wants mtry = sqrt(p), continuous wants mtry = p/3
  .RF_mtry <- ifelse(is.discrete(target_var_else), sqrt(.p), .p / 3L)
  .rf <- randomForest::randomForest(target_var_else~.,
                                    data = data.frame(target_var_else, data_else),
                                    mtry = .RF_mtry)
  
  #### DALEX::predict_parts, (of DALEX::explain()) of that Random forest
  parts_type <- match.arg(parts_type)
  .ex_rf <- DALEX::explain(model = .rf,
                           data = data_else,
                           y = target_var_else,
                           label = paste0(parts_type, " local attribution of random forest model"))
  .parts <- DALEX::predict_parts(explainer = .ex_rf, ## ~ 10 s @ B=25
                                 new_observation = data_oos,
                                 type = parts_type,
                                 N = parts_N,
                                 B = parts_B,
                                 ...)
  
  #### The local attribution of those parts, to be use as first dim of projection
  .scree_la <- df_scree_local_attr(.parts)
  ## Keep in mind that there are class # of level shap values if you don't test against specific class level
  ## Reorder scree table back to original data colname order
  .row_idx <- order(match(.scree_la$variable_name, colnames(data)))
  .scree_la <- .scree_la[.row_idx, ]
  
  #### Basis of a global feature (holdout_rownum removed), to use as second dim of projection
  basis_type <- match.arg(basis_type)
  .bas <- switch(basis_type,
                 pca  = basis_pca(data_else),
                 olda = basis_olda(data_else, class_else),
                 odp  = basis_odp(data_else, class_else),
                 onpp = basis_onpp(data_else), ## Using default hyperparameters
                 stop("basis_type expects 'pca', 'olda', 'odp' or 'onpp'.")
  )
  
  #### Bring them together and orthonormalize
  .cheem_bas <- cbind(.scree_la$median_local_attr, .bas)[, 1L:2L]
  .cheem_bas <- as.matrix(tourr::orthonormalise(.cheem_bas))
  colnames(.cheem_bas) <- c(parts_type, paste0(basis_type, "1"))
  
  attr(.cheem_bas, "class") <- c("cheem_basis", "matrix")
  attr(.cheem_bas, "data_else") <- as.matrix(data_else)
  attr(.cheem_bas, "data_oos")  <- as.matrix(data_oos)
  attr(.cheem_bas, "class_else") <- class_else ## Can't call it "class" b/c matrix/df.
  attr(.cheem_bas, "class_oos")  <- class_oos
  attr(.cheem_bas, "randomForest")  <- .rf
  attr(.cheem_bas, "explain")       <- .ex_rf
  attr(.cheem_bas, "predict_parts") <- .parts
  
  return(.cheem_bas)
}

## Print cheem_bases as a numeric matrix without showing all the attributes.
print.cheem_basis <- function (x, ...)
{
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
view_cheem <- function(cheem_basis, show_parts = TRUE,
                       oos_identity_args = list(color = "red", size = 5, shape = 8),
                       ...){ ## Passed to plot.pridict_parts()
  .data_else <- attributes(cheem_basis)$data_else
  .data_oos  <- attributes(cheem_basis)$data_oos
  .class_else <- attributes(cheem_basis)$class_else
  .class_oos  <- attributes(cheem_basis)$class_oos
  .cn <- colnames(cheem_basis)
  
  ## oos projection not done in view_frames
  .proj_new_obs <- data.frame(.data_oos %*% cheem_basis)
  
  .oos_pt_func <- function(...){
    geom_point(aes_string(x = .cn[1L], y = .cn[2L]),
             .proj_new_obs, ...)
  }
  .oos_pt_call <- do.call(.oos_pt_func, oos_identity_args)
  
  gg <- view_frame(cheem_basis, data = .data_else,
                   aes_args = list(color = .class_else, shape = .class_else),
                   axes = "left") +
    theme(legend.position = "off",
          axis.title = element_text()) +
    labs(x = .cn[1L],
         y = .cn[2L]) +
    .oos_pt_call
  
  if(show_parts == TRUE){
    require("patchwork")
    .parts <- attributes(cheem_basis)$predict_parts
    gg <- gg + plot(.parts, ...)
  }
  return(gg)
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
