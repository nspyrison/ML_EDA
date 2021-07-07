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

