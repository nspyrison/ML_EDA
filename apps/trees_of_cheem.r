## Leg work
require("treeshap")
require("DALEX")
require("spinifex")
## Util
require("magrittr")
require("tictoc")
require("beepr")
require("ggplot2")

is.discrete <- function(x){ ## see plyr::is.discrete(). !! not on levels, class only
  is.factor(x) || is.character(x) || is.logical(x)}

rnorm_observation_of <- function(data){
  c_mns <- apply(data, 2L, mean)
  c_var <- apply(data, 2L, var)
  return(as.data.frame(
    matrix(rnorm(ncol(data), c_mns, sqrt(c_var)),
           nrow = 1L, dimnames = list(NULL, colnames(data)))
  ))
}

## SHAP layers --------

### Util functions, SHAP layers

#### Normalized mahalonobis distances | given median, covar

#### projection space df of
plot_df_of <- function(x, y, basis_type = c("pca", "olda"), class = NULL, ## class req for olda
                       d = 2, layer_name, residuals){
  ## maha_vect_of
  #maha_vect_of <- function(x, do_normalize = TRUE){ ## distance from median(x), cov(x)
  maha <- mahalanobis(x, apply(x, 2, median), cov(x)) %>%
    matrix(ncol = 1)
  maha <- spinifex::scale_01(maha)
  
  ## proj_df_of
  # proj_df_of <- function(x, basis_type = c("pca", "olda"), class, ## class req for olda
  #                        d = 2, do_normalize = TRUE){
  basis_type <- match.arg(basis_type)
  basis <- switch(basis_type,
                  pca = prcomp(x)$rotation[, 1:d], #spinifex::basis_pca(x, d),
                  olda = spinifex::basis_olda(x, class, d))
  proj <- as.matrix(x) %*% basis
  proj <- spinifex::scale_01(proj) %>%  as.data.frame()
  
  ## bind
  .qq_color <- colorRampPalette(c("grey", "red"))(100)[
    as.numeric(cut(maha, breaks = 100))]
  .plot_df <- cbind(proj, maha, 1:nrow(x), y, residuals, layer_name, basis_type, .qq_color)
  ## add quantiles and order
  .q_idx <- order(maha)
  .probs <- seq(.01, .99, length.out = nrow(x))
  .qs <- quantile(maha, probs = .probs)
  .qt <- qnorm(.probs, mean(maha), sd(maha))
  .plot_df <- cbind(.plot_df[.q_idx, ], .qs, .qt)
  names(.plot_df) <-
    c(paste0("V", 1:d), "maha_dist", "rownum", "y", "residual", "var_layer", "view",
      "qq_color", "quantile_theoretical", "quantile_maha_dist")
  return(.plot_df)
}

### One shap layer
shap_layer_of <- function(x, y, xtest = NULL, ytest = NULL,
                          layer_name = "UNAMED",
                          basis_type = c("pca", "olda"), class = NULL, d = 2, ## class req for olda
                          verbose = TRUE, noisy = TRUE){
  require("treeshap")
  if(noisy   == TRUE) require("beepr")
  if(verbose == TRUE){
    require("tictoc")
    tictoc::tic(paste0("shap_layer_of ", layer_name))
  }
  
  ## RF model
  .is_y_disc <- is.factor(y) || is.character(y) || is.logical(y)
  sec_rf <- system.time({
    .m <- capture.output(gc())
    .hp_mtry <- if(.is_y_disc == TRUE) sqrt(ncol(x)) else ncol(x) / 3
    .hp_node <- if(.is_y_disc == TRUE) 1 else 5
    .hp_node <- max(.hp_node, nrow(x) / 500)
    .rf <- randomForest::randomForest(y~., data = data.frame(y, x),
                                      mtry = .hp_mtry, nodesize = .hp_node)
  })[3]
  
  ## RF performance,
  ### MANUALLY created, and different than the performance created by the rf fit...
  .resid <- y - predict(.rf, x)
  .rss <- sum(.resid^2)
  .tss <- sum((y - mean(y))^2)
  .mse <- 1 / nrow(x) * .rss %>% round(2)
  .rmse <- sqrt(.mse) %>% round(2)
  .rsq <- 1 - (.rss/.tss) %>% round(4)
  .performance_df <- data.frame(mse = .mse, rmse = .rmse, rsq = .rsq)
  if(is.null(xtest) == FALSE & is.null(ytest) == FALSE){
    .rss_t <- sum((ytest - predict(.rf, xtest))^2)
    .tss_t <- sum((ytest - mean(ytest))^2)
    .mse_t <- 1 / nrow(x) * .rss_t %>% round(2)
    .rmse_t <- sqrt(.mse_t) %>% round(2)
    .rsq_t <- 1 - (.rss_t/.tss_t) %>% round(4)
    .performance_df$test_mse  <- .mse_t
    .performance_df$test_rmse <- .rmse_t
    .performance_df$test_rsq  <- .rsq_t
  }
  
  ## plot_df_of x, .rf, NOT SHAP
  sec_maha_pca <- system.time({
    .m <- capture.output(gc())
    .plot_df <- plot_df_of(
      x, y, basis_type, class, d,
      layer_name = paste0(layer_name, " \n rmse = ", .rmse),
      residual = .resid)
  })[3]
  
  ## treeshap
  sec_shap <- system.time({
    .m <- capture.output(gc())
    .shap <- treeshap_df(.rf, x)
    .shap_xtest <- NULL ## init
    if(is.null(xtest) == FALSE) .shap_xtest <- treeshap_df(.rf, xtest)
  })[3]
  
  ## Execution time
  time_df <- data.frame(
    runtime_seconds = c(sec_rf, sec_shap, sec_maha_pca),
    chunk = c("rf model", "(rf) treeshap shap", "maha/pca"),
    layer = layer_name)
  
  if(verbose == TRUE) tictoc::toc()
  if(noisy == TRUE & sum(time_df$runtime_seconds) > 10) beepr::beep(1)
  return(list(plot_df = .plot_df,
              rf_model = .rf,
              performance_df = .performance_df,
              shap_df = .shap,
              shap_xtest_df = .shap_xtest,
              time_df = time_df))
}


#' @examples
#' X <- tourr::flea[, 2:6]
#' Y <- tourr::flea[, 1]
#' set.seed(303)
#' .idx_test <- sample(1:nrow(X), size = round(.5 * nrow(X))) ### HOLD OUT TEST DATA.
#' X_test  <- X[.idx_test,  ]
#' X_train <- X[-.idx_test, ]
#' Y_test  <- Y[ .idx_test]
#' Y_train <- Y[-.idx_test]
#' x <- X_train; y = Y_train; x_test = X_test; y_test = Y_test;
#' formated_ls <- nested_shap_layers(X_train, Y_train,
#'                                   X_test, Y_test)
#' formated_ls$performance_df

### Format many shap layers
format_nested_layers <- function(shap_layer_ls, x, y,
                                 basis_type = c("pca", "olda"), class = NULL, d = 2,
                                 verbose = TRUE){
  ### rbind plot_df
  b_plot_df <- data.frame()
  .mute <- sapply(1:length(shap_layer_ls), function(i){
    this_plot_df <- shap_layer_ls[[i]]$plot_df
    b_plot_df <<- rbind(b_plot_df, this_plot_df)
  })
  
  ### performance2 of the layers, manual performance
  .nms <- names(shap_layer_ls)
  performance_df2 <- data.frame(NULL)
  .mute <- sapply(1:length(shap_layer_ls), function(i){
    .row <- shap_layer_ls[[i]]$performance_df
    .row$runtime_seconds <- sum(shap_layer_ls[[i]]$time_df$runtime_seconds)
    performance_df2 <<- rbind(performance_df2, .row)
  })
  row.names(performance_df2) <- .nms
  
  # ### performance of the layers
  # .nms <- names(shap_layer_ls)
  # performance_df <- data.frame(NULL)
  # model_ls <- list()
  # .mute <- sapply(1:length(shap_layer_ls), function(i){
  #   model_ls[[i]] <<- shap_layer_ls[[i]]$rf_model
  #   this_row <- data.frame(.nms[i],
  #                          sum(shap_layer_ls[[i]]$time_df$runtime_seconds),
  #                          median(model_ls[[i]]$mse),
  #                          median(sqrt(model_ls[[i]]$mse)),
  #                          median(model_ls[[i]]$rsq))
  #   performance_df <<- rbind(performance_df, this_row)
  # })
  # performance_df[, 2:ncol(performance_df)] <-
  #   round(performance_df[, 2:ncol(performance_df)], 2)
  # names(model_ls) <- names(shap_layer_ls)
  # colnames(performance_df) <- c("layer", "runtime_seconds", "median_mse",
  #                               "median_rmse", "median_rsq")
  
  ### Cbind decode table
  decode_df <- data.frame(rownum = 1:nrow(x), y)
  .mute <- sapply(1:length(shap_layer_ls), function(i){
    this_rf_model <- shap_layer_ls[[i]]$rf_model
    decode_df <<- cbind(decode_df, y - predict(this_rf_model))
  })
  decode_df <- cbind(decode_df, x)
  names(decode_df) <-
    c("rownum", "y", paste0("residual_", names(shap_layer_ls)), names(x))
  if(is.na(as.numeric(rownames(x)[1])) == FALSE) ## does first rowname contain letters
    decode_df$tt <- paste0("rn: ", decode_df$rownum, rownames(decode_df))
  
  ### Rbind shap_df
  b_shap_df <- data.frame()
  .mute <- sapply(1:length(shap_layer_ls), function(i){
    .shap_df <- cbind(shap_layer_ls[[i]]$shap_df, .nms[i])
    b_shap_df <<- rbind(b_shap_df, .shap_df)
  })
  colnames(b_shap_df) <- c(colnames(shap_layer_ls[[1]]$shap_df), "layer_name")
  
  ### Rbind time_df
  b_time_df <- data.frame()
  .mute <- sapply(1:length(shap_layer_ls), function(i){
    b_time_df <<- rbind(b_time_df, shap_layer_ls[[i]]$time_df)
  })
  
  if(verbose == TRUE) tictoc::toc()
  return(list(plot_df = b_plot_df,
              decode_df = decode_df,
              #performance_df = performance_df,
              performance_df = performance_df2,
              shap_df = b_shap_df, 
              time_df = b_time_df
              #, model_ls = model_ls ## almost all of the object size.
  ))
}


## Final SHAP layer function
nested_shap_layers <- function(x, y, xtest = NULL, ytest = NULL,
                               n_shap_layers = 3,
                               basis_type = c("pca", "olda"), class = NULL, d = 2,
                               verbose = TRUE, noisy = TRUE){
  loc_attr_nm <- "shap"
  require("treeshap")
  if(noisy == TRUE) require("beepr")
  if(verbose == TRUE) {
    require("tictoc")
    print(paste0("nested_shap_layers() started at ", Sys.time()))
    tictoc::tic("nested_shap_layers()")
  }
  
  ### Create shap layers in a list
  .next_layers_x <- x ## Init
  .next_layers_xtest <- xtest ## Init
  shap_layer_ls <- list()
  layer_nms <- c("data", paste0(loc_attr_nm, "^", 1:(n_shap_layers - 1)))
  layer_runtimes <- c(NULL)
  .mute <- sapply(1:n_shap_layers, function(i){
    shap_layer_ls[[i]] <<- shap_layer_of(.next_layers_x, y,
                                         .next_layers_xtest, ytest,
                                         layer_nms[i],
                                         basis_type, class, d,
                                         verbose, noisy)
    .next_layers_x <<- shap_layer_ls[[i]]$shap_df
    .next_layers_xtest <<- shap_layer_ls[[i]]$shap_xtest_df ## Could be NULL
    # if(verbose == TRUE & i != n_shap_layers){
    #   layer_runtimes[i] <<- sum(shap_layer_ls[[i]]$time_df$runtime_seconds)
    #   est_min_remaining <- round(sum(layer_runtimes) / i * (n_shap_layers - i) / 60, 1)
    #   #print(paste0("Estimated min of runtime remaining: ", est_min_remaining,
    #                ". Estimated completion time: ", round(Sys.time() + est_min_remaining * 60)
    #   ))
    # }
  })
  names(shap_layer_ls) <- layer_nms
  
  ### Format into more usable dfs rather than layer lists
  formated <- format_nested_layers(shap_layer_ls, x, y,
                                   basis_type, class, d, verbose)
  
  if(noisy == TRUE) beepr::beep(2)
  return(formated)
}



## SHAP data frames -----

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
  .rfu <- treeshap::randomForest.unify(randomForest_model, data)
  .tshap_ls <- treeshap::treeshap(.rfu, x = data) 
  .tshap_ls <- .tshap_ls[c(1, 4)]
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



#' @examples
#' ## Discrete supervised classification RF
#' dat <- spinifex::scale_sd(mtcars)
#' y <- dat[, 1]
#' x <- dat[, -1]
#' mod <- randomForest::randomForest(mpg~., data = dat)
#' 
#' la_df <- local_attribution_df(data = x, target_var = y, model = mod)
#' GGally::ggpairs(as.data.frame(la_mat), mapping = aes(color = tgt_var))
## Local attribution df from DALEX
local_attribution_df <- function(
  data, target_var,
  model = randomForest::randomForest(
    x    = target_var~.,
    data = data.frame(target_var, data),
    mtry = if(plyr::is.discrete(tgt_var)) sqrt(ncol(data)) else ncol(data) / 3),
  parts_type = c("shap", "break_down", "oscillations", "oscillations_uni", "oscillations_emp"),
  parts_B = 10,
  parts_N = if(substr(parts_type, 1, 4) == "osci") 500 else NULL, ## see DALEX::predict_parts
  verbose = TRUE,
  ...){
  ## Assumptions
  data <- as.data.frame(data)
  parts_type <- match.arg(parts_type)
  ## Initialize
  .p <- ncol(data)
  .n <- nrow(data)
  
  if(plyr::is.discrete(target_var) & length(unique(target_var)) > 2)
    stop("Not expecting multiclass discrete target variable, try looping over target variable testing against each level.")
  ## Explanation of the model
  .ex <- DALEX::explain(model = model, ## Has heavy model, needed for predict_parts.
                        data = data,
                        y = target_var,
                        label = paste0(parts_type, " local attribution of random forest model"))
  
  ## predict the parts, iterating over each each observation
  ret <- matrix(NA, nrow = nrow(data), ncol = ncol(data))
  r_seq <- round(seq(0, nrow(data), length.out = 11)[-1])
  sapply(1:nrow(data), function(i){
    .parts <- DALEX::predict_parts(explainer = .ex,
                                   new_observation =  data[i,, drop = FALSE],
                                   type = parts_type,
                                   N = parts_N,
                                   B = parts_B,
                                   ...)
    attr(.parts, "yhats_distribution") <- NULL ## Reduces ~90% of the size, without hurting plot.predict_parts
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
    if(verbose == TRUE) ## if verbose, print message about every 10% of obs
      if(i %in% r_seq) print(paste0("Completed ", i, " of ", nrow(data), " observations. (~ every 10%)."))
  })
  
  ## Format
  .rn <- rownames(data)
  if(is.null(.rn) == TRUE) .rn <- 1L:.n
  rownames(ret) <- paste0(parts_type, " of ", .rn)
  colnames(ret) <- colnames(data)
  ret <- as.data.frame(ret)
  attr(ret, "data") <- data
  attr(ret, "class") <- c("local_attribution_df", "data.frame")
  
  ## Return full local attribution df.
  return(ret)
}




## SHAP as basis component: ----

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


