require("ranger")
require("treeshap")
require("DALEX")
require("spinifex")
require("tourr")
require("ggplot2")

is.discrete <- function(x) ## see plyr::is.discrete(). !! not on levels, class only
{is.factor(x) || is.character(x) || is.logical(x)}

rnorm_observation <- function(data){
  c_mns <- apply(data, 2L, mean)
  c_var <- apply(data, 2L, var)
  obs_vec <- rnorm(ncol(data), c_mns, sqrt(c_var))
  obs <- as.data.frame(matrix(obs_vec, nrow = 1L, dimnames = list(NULL, colnames(data))))
  return(obs)
}

#' @examples
#' dat <- DALEX::apartments[, 1:5]
#' xdat <- dat[, 2:5] ## -c(m2.price, district))
#' y <- dat$m2.price
#' 
#' ## Fit a {randomForest} model, slower fit, but faster shap than ranger
#' .rf <- randomForest::randomForest(y ~ ., data = data.frame(y, xdat))
#' system.time(
#'  df_shap <- treeshap_df(.rf, data = xdat)
#' )[3]
treeshap_df <- function(randomForest_model, data){
  .rfu <- treeshap::randomForest.unify(.rf, data)
  .tshap_ls <- treeshap::treeshap(.rfu, x = data) 
  .tshap_ls <- .tshap_ls[c(1,4)]
  ## Keeping only c(1,4); reduces ~98.5% of the obj size, keep shap values make data attr.
  ## But, we lose the iBreakdown-like plot of treeshap::plot_contribution when we take this apart.
  ret <- .tshap_ls[[1]]
  
  attr(ret, "class") <- c("treeshap_df", "data.frame")
  attr(ret, "data")  <- .tshap_ls[[2]] ## Also a data.frame
  return(ret)
}
## Print basis_cheem_INSAMP as a numeric matrix without showing all the attributes.
print.treeshap_df <- function (x, ...){
  attr(x, "data") <- NULL
  NextMethod()
}


# dist_mat <- as.matrix(dist(df_shap))



#' @examples
#' ## Discrete supervised classification RF
#' dat <- spinifex::scale_sd(mtcars)
#' tgt_var <- dat[, 1]
#' xdat <- dat[, -1]
#' 
#' la_df <- local_attribution_df(data = xdat, target_var = tgt_var)
#' GGally::ggpairs(as.data.frame(la_mat), mapping = aes(color = tgt_var))
## Local attribution df from DALEX
local_attribution_df <- function(
  data, target_var,
  model = randomForest::randomForest(
    x    = target_var~.,
    data = data.frame(target_var, data),
    mtry = if(plyr::is.discrete(tgt_var)) sqrt(ncol(data)) else ncol(data) / 3L),
  parts_type = c("shap", "break_down", "oscillations", "oscillations_uni", "oscillations_emp"),
  parts_B = 10,
  parts_N = if(substr(parts_type, 1, 4) == "osci") 500 else NULL, ## see DALEX::predict_parts
  ...){
  ## Assumptions
  data <- as.data.frame(data)
  parts_type <- match.arg(parts_type)
  ## Initialize
  .p <- ncol(data)
  .n <- nrow(data)
  
  if(plyr::is.discrete(target_var) & length(unique(target_var)) > 2L)
    stop("Not expecting multiclass discrete target variable, try looping over target variable testing against each level.")
  #### DALEX::predict_parts, (of DALEX::explain()) of that Random forest
  .ex <- DALEX::explain(model = model,
                           data = data,
                           y = target_var,
                           label = paste0(parts_type, " local attribution of random forest model"))
  
  #### Iterating over each each observation:
  ret <- matrix(NA, nrow = nrow(data), ncol = ncol(data))
  ##for each row num -----
  sapply(1L:nrow(data), function(i){
    .parts <- DALEX::predict_parts(explainer = .ex,
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
    ret[i, ] <<- .df_la$median_local_attr
    if(i %% 10 == 0) print(paste0("Done with ", i, " of ", nrow(data)))
  })
  
  ## Format
  .rn <- rownames(data)
  if(is.null(.rn) == TRUE) .rn <- 1L:.n
  rownames(ret) <- paste0(parts_type, " of ", .rn)
  colnames(ret) <- colnames(data)
  ret <- as.data.frame(ret)
  attr(ret, "class") <- c("local_attribution_df", "data.frame")
  
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
basis_cheem <- function(
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
  attr(.cheem_bas, "class")           <- c("basis_cheem", "matrix")
  attr(.cheem_bas, "data")            <- data ## Expected as matrix 
  attr(.cheem_bas, "y")               <- y
  attr(.cheem_bas, "new_observation") <- new_observation
  attr(.cheem_bas, "predict_parts")   <- .parts
  ## Return
  return(.cheem_bas)
}

## Print basis_cheem_INSAMP as a numeric matrix without showing all the attributes.
print.basis_cheem <- function (x, ...){
  attr(x, "data") <- NULL
  attr(x, "y") <- NULL
  attr(x, "new_observation")<- NULL
  attr(x, "predict_parts") <- NULL
  NextMethod()
}

## plot basis_cheem as 1d density, next to plot(.parts)
autoplot.basis_cheem <- plot.basis_cheem <- function(
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
  gg <- ggplot_tour(basis_array = cheem_basis, data = data)
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


