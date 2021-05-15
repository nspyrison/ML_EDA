require(ggplot2)
require(spinifex)
require(dplyr)
dat <- scale_sd(mtcars)
bas <- basis_pca(dat)
gt_array <- save_history(dat, grand_tour(), max = 3, start = bas)

ggplot_tour <- function(basis_array, data = NULL,
                        angle = .05
                        # bind_col_basis = list(label = NULL),
                        # mapping_data = aes(x, y, color = color) ## can these be simplifid? really we want to know what else to use.
){
  if(is.null(data) == TRUE)
    data <- attr(basis_array, "data") ## Also could be NULL.
  
  if(F){
    .aes_nms <- as.character(mapping_basis) ## With ~, character of a formula
    .aes_nms <- substring(.aes_nms, 2L) ## Drop ~ character, the string names that need to be included and replicated.
    
    
    cbind(as.matrix(mtcars), letters[1:3], flea$species) ## Wants matrix over df, recycles to the longest obj.
    ## do this here or in the ggprotos_ ???? Can I extract from aes() calls? not easy.
  }
  
  ## array2df_version2::
  manip_var <- attr(basis_array, "manip_var") ## Only for manual_tour(), else null
  if(is.null(manip_var)) ## If not a manual tour interpolate
    basis_array <- tourr::interpolate(basis_array, angle = angle)
  df_ls <- array2df(basis_array, data)
  df_basis <- df_ls$basis_frames
  df_data  <- df_ls$data_frames
  attr(df_basis, "manip_var") <- manip_var ## NULL if not a manual tour
  
  ## TODO best way to implement?
  aes_basis <- "NS TODO"
  aes_data  <- "NS TODO"

  ## Assign hidden preped dataframes and mapping aes
  assign(x = ".spinifex_df_basis",  value = df_basis,  envir = globalenv())
  assign(x = ".spinifex_df_data",   value = df_data,   envir = globalenv())
  assign(x = ".spinifex_aes_basis", value = aes_basis, envir = globalenv())
  assign(x = ".spinifex_aes_data",  value = aes_data,  envir = globalenv())
  ## Also behave as ggpot() with overwritable spinifex theme
  return(ggplot() + theme_spinifex())
}

ggproto_basis_axes <- function(position = "left", manip_col = "blue",
                               line_size = 1L, text_size = 5L){
  ## Assumptions
  if(position == "off") return()
  ## Initialization
  df_data <- .spinifex_df_data
  df_basis <- .spinifex_df_basis
  n_frames <- length(unique(df_basis$frame))
  p <- nrow(df_basis)/n_frames
  manip_var <- attr(.spinifex_df_basis, "manip_var")
  ## Setup and transform
  .angles <- seq(0L, 2L * pi, length = 360L)
  .circle <- data.frame(x = cos(.angles), y = sin(.angles))
  if(is.null(df_data)){
    .to <- data.frame(x = c(-1L, 1L), y = c(-1L, 1L))
  }else{
    .to <- df_data
  }
  .center <- scale_axes(data.frame(x = 0L, y = 0L), position, .to)
  .circle <- scale_axes(.circle, position, .to)
  df_basis <- scale_axes(df_basis, position, .to)
  ## Aestheics for the axes segments.
  .axes_col <- "grey50"
  .axes_siz <- line_size
  if (is.null(manip_var) == FALSE) {
    .axes_col <- rep("grey50", p)
    .axes_col[manip_var] <- manip_col
    .axes_col <- rep(axes_col, n_frames)
    .axes_siz <- rep(line_size, p)
    .axes_siz[manip_var] <- 1.5 * line_size
    .axes_siz <- rep(axes_siz, n_frames)
  }
  ## Return ggproto of basis axes in unit circle
  return(
    list(
      ggplot2::geom_path(data = .circle, color = "grey80",
                         size = line_size, inherit.aes = FALSE,
                         mapping = ggplot2::aes(x = x, y = y)),
        suppressWarnings(ggplot2::geom_segment( ## Suppress unused arg: frames
          data = df_basis,
          size = .axes_siz, colour = .axes_col,
          mapping = ggplot2::aes(x = x, y = y, frame = frame,
                                 xend = .center[, 1L], yend = .center[, 2L])
        )),
      suppressWarnings(ggplot2::geom_text(
        data = df_basis,
        colour = .axes_col, size = text_size,
        vjust = "outward", hjust = "outward",
        mapping = ggplot2::aes(x = x, y = y, frame = frame, label = label)
      ))
    )
  )
}

ggproto_data_points <- function(aes_args = list(),
                                identity_args = list(),
                                zero_mark = TRUE,
                                gridline_probs = seq(.25, .75, .25),
){
  prep_ggproto_data <- function(zero_mark = TRUE,
                                gridline_probs = seq(.25, .75, .25)){
  aes_args <- as.list(aes_args)
  identity_args <- as.list(identity_args)
  ## Initialization
  df_data <- .spinifex_df_data
  df_basis <- .spinifex_df_basis
  n_frames <- length(unique(df_basis$frame))
  p <- nrow(df_basis)/n_frames
  manip_var <- attr(.spinifex_df_basis, "manip_var")
  
  ret <- list()
  ## Setup gridlines
  if(is.numeric(gridline_probs)){
    .x_min <- min(.to[, 1L])
    .y_min <- min(.to[, 2L])
    .x_max <- max(.to[, 1L])
    .y_max <- max(.to[, 2L])
    .x_q <- quantile(.to[, 1L], gridline_probs)
    .y_q <- quantile(.to[, 2L], gridline_probs)
    .len <- length(gridline_probs)
    .df_gridlines <- data.frame(
      x     = c(rep(.x_min, .len), .x_q),
      x_end = c(rep(.x_max, .len), .x_q),
      y     = c(.y_q, rep(.y_min, .len)),
      y_end = c(.y_q, rep(.y_max, .len))
    )
    
    gridlines <- ggplot2::geom_segment(
      data = .df_gridlines,
      color = "grey80", size = .8,
      mapping = ggplot2::aes(x = x, y = y, xend = x_end, yend = y_end)
    )
    ret <- c(ret, gridlines)
  }

  ## Setup sero mark 
  if(zero_mark == TRUE){
    .center <- scale_axes(data.frame(x = 0L, y = 0L), position, .to)
    .x_tail <- .05 * diff(range(.x_min, .x_max))
    .y_tail <- .05 * diff(range(.y_min, .y_max))
    .df_zero_mark <-
      data.frame(x     = c(.center[, 1L] - .center, .center[, 1L]),
                 x_end = c(.center[, 1L] + .center, .center[, 1L]),
                 y     = c(.center[, 2L], .center[, 2L] - .center),
                 y_end = c(.center[, 2L], .center[, 2L] + .center)
      )
    
    zero_mark <- ggplot2::geom_segment(
      data = .df_zero_mark,
      color = "grey60", size = 1.2,
      mapping = ggplot2::aes(x = x, y = y, xend = x_end, yend = y_end)
    )
    ret <- c(ret, zero_mark)
  }
  return()
  }
  ## Return ggproto of projection points, zero mark, and gridlines 
  return(ret)
}


z <- ggplot_tour(gt_array, dat)
zz <- ggproto_basis_axes()

z + zz
gg <- ggplot_tour(gt_array, dat) + ggproto_basis_axes()

