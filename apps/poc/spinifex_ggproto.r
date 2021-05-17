require(ggplot2)
require(spinifex)
require(dplyr)

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

  ## Assign hidden prepared dataframes
  assign(x = ".spinifex_df_basis",  value = df_basis,  envir = globalenv())
  assign(x = ".spinifex_df_data",   value = df_data,   envir = globalenv())
  ## Also behave as ggpot() with overwritable theme settings
  ret <- ggplot2::ggplot() +
    ggplot2::theme_void() +
    ggplot2::scale_color_brewer(palette = "Dark2") +
    ggplot2::coord_fixed() +
    ggplot2::theme(legend.position = "bottom",      ## Of plot
                   legend.direction = "horizontal", ## With-in aesthetic
                   legend.box = "vertical",         ## Between aesthetic
                   legend.margin = margin())        ## Try to minimize margin.
  return(ret)
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
    .axes_col <- rep(.axes_col, n_frames)
    .axes_siz <- rep(line_size, p)
    .axes_siz[manip_var] <- 1.5 * line_size
    .axes_siz <- rep(.axes_siz, n_frames)
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

ggproto_data_background <- function(zero_mark = TRUE,
                                    gridline_probs = seq(0, 1, .25)){
  ## Initialization
  position <- "center" ## Data assumed center.
  df_data  <- .spinifex_df_data
  df_basis <- .spinifex_df_basis
  n_frames <- length(unique(df_basis$frame))
  p <- nrow(df_basis) / n_frames
  manip_var <- attr(.spinifex_df_basis, "manip_var") ## NULL if not manip var
  
  ret <- list() ## Init
  ## Setup gridlines
  if(is.numeric(gridline_probs) &
     all(gridline_probs >= 0L) &
     all(gridline_probs <= 1L)
  ){
    .to <- df_data
    .x_min <- min(.to[, 1L])
    .y_min <- min(.to[, 2L])
    .x_max <- max(.to[, 1L])
    .y_max <- max(.to[, 2L])
    .x_q <- .x_min + gridline_probs * diff(range(.x_min, .x_max))
    .y_q <- .y_min + gridline_probs * diff(range(.y_min, .y_max))
    .len <- length(gridline_probs)
    .df_gridlines <- data.frame(
      x     = c(rep(.x_min, .len), .x_q),
      x_end = c(rep(.x_max, .len), .x_q),
      y     = c(.y_q, rep(.y_min, .len)),
      y_end = c(.y_q, rep(.y_max, .len))
    )
    
    gridlines <- ggplot2::geom_segment(
      data = .df_gridlines,
      color = "grey80", size = .5,
      mapping = ggplot2::aes(x = x, y = y, xend = x_end, yend = y_end)
    )
    ret <- c(ret, gridlines)
  }
  
  ## Setup zero mark, 5% on each side.
  if(zero_mark == TRUE){
    .center <- scale_axes(data.frame(x = 0L, y = 0L), position, .to)
    .x_tail <- .05 * diff(range(.x_min, .x_max))
    .y_tail <- .05 * diff(range(.y_min, .y_max))
    .df_zero_mark <-
      data.frame(x     = c(.center[, 1L] - .x_tail, .center[, 1L]),
                 x_end = c(.center[, 1L] + .x_tail, .center[, 1L]),
                 y     = c(.center[, 2L], .center[, 2L] - .y_tail),
                 y_end = c(.center[, 2L], .center[, 2L] + .y_tail)
      )
    
    zero_mark <- ggplot2::geom_segment(
      data = .df_zero_mark,
      color = "grey60", size = 1L,
      mapping = ggplot2::aes(x = x, y = y, xend = x_end, yend = y_end)
    )
    ret <- c(ret, zero_mark)
  }
  return(ret)
}

ggproto_data_points <- function(aes_args = list(),
                                identity_args = list()){
  ## Assumptions
  if(is.null(.spinifex_df_data) == TRUE) return()
  aes_args <- as.list(aes_args)
  identity_args <- as.list(identity_args)
  
  ## Initialization
  df_data  <- .spinifex_df_data
  df_basis <- .spinifex_df_basis
  n_frames <- length(unique(df_basis$frame))
  n <- nrow(df_data) / n_frames

  ## Add aes_args to df_data, replicating across frame
  .tgt_len  <- nrow(df_data)
  .orig_nms <- names(df_data)
  .aes_arg_nms <- names(aes_args)
  .mute <- lapply(seq_along(aes_args), function(i){
    .this_arg <- aes_args[[i]]
    if(length(.this_arg) %in% c(1L, n) == FALSE)
      warning(paste0("aes_arg '", .aes_arg_nms[i], "' not of length 1 or data."))
    .this_col <- rep_len(.this_arg, .tgt_len)
    df_data[, ncol(df_data) + 1L] <<- .this_col ## Bind column to df_data
    aes_args[[i]] <<- .this_col ## Replace the value with the string of the orig
    ## TODO Warning this may cause issues down the line!?
    #### as ggplot will want to treat this as a vector rather than column of the df?
    #### Causes the split in the legend... i don't see a way around it, without having end
  })
  ## repair names
  names(df_data) <- c(.orig_nms, .aes_arg_nms)
  
  ## do.call aes() over the aes_args 
  .aes_func <- function(...)
    ggplot2::aes(x = x, y = y, frame = frame, ...)
  .aes_call <- do.call(.aes_func, aes_args)
  ## do.call geom_point() over the identity_args 
  .geom_func <- function(...){
    suppressWarnings(
      ggplot2::geom_point(mapping = .aes_call, data = df_data, ...)
    )
  }
  .geom_call <- do.call(.geom_func, identity_args)
  ## Return ggproto of projection points
  return(.geom_call)
}

## Printing as points 
ggproto_data_text <- function(aes_args = list(label = as.character(1:nrow(dat))),
                              identity_args = list()){
  ## Assumptions
  if(is.null(.spinifex_df_data) == TRUE) return()
  aes_args <- as.list(aes_args)
  identity_args <- as.list(identity_args)
  ## Initialization
  df_data  <- .spinifex_df_data
  df_basis <- .spinifex_df_basis
  n_frames <- length(unique(df_basis$frame))
  n <- nrow(df_data) / n_frames
  
  ## Add aes_args to df_data, replicating across frame
  .tgt_len   <- nrow(df_data)
  .orig_nms <- names(df_data)
  .aes_arg_nms <- names(aes_args)
  .mute <- lapply(seq_along(aes_args), function(i){
    .this_arg <- aes_args[[i]]
    if(length(.this_arg) %in% c(1L, n) == FALSE)
      warning(paste0("aes_arg '", .aes_arg_nms[i], "' not of length 1 or data."))
    .this_col <- rep_len(.this_arg, .tgt_len)
    df_data[, ncol(df_data) + 1L] <<- .this_col ## Bind column to df_data
    aes_args[[i]] <<- .this_col ## Replace the value with the string of the orig
    ## TODO Warning this may cause issues down the line!?
    #### as ggplot will want to treat this as a vector rather than column of the df?
    #### Causes the split in the legend... i don't see a way around it, without having end
  })
  ## repair names
  names(df_data) <- c(.orig_nms, .aes_arg_nms)
  
  ## do.call aes() over the aes_args 
  .aes_func <- function(...)
    ggplot2::aes(x = x, y = y, frame = frame, ...)
  .aes_call <- do.call(.aes_func, aes_args)
  ## do.call geom_point() over the identity_args 
  .geom_func <- function(...){
    suppressWarnings(
      ggplot2::geom_text(mapping = .aes_call, data = df_data, ...)
    )
  }
  .geom_call <- do.call(.geom_func, identity_args)
  ## Return ggproto
  return(.geom_call)
}


if(F){ ## TESTING
#' @examples 
#' dat <- scale_sd(mtcars)
#' bas <- basis_pca(dat)
#' gt_array <- save_history(dat, grand_tour(), max = 3, start = bas)
#' 
#' ggplot_tour(gt_array, dat) +
#'   ggproto_basis_axes() +
#'   ggproto_data_background() +
#'   ggproto_data_points() + theme(legend.position = "right")
#'   
#' dat <- scale_sd(tourr::flea[, 1:6])
#' clas <- flea[, 7]
#' bas <- basis_pca(dat)
#' mv  <- manip_var_of(bas)
#' mt_array <- manual_tour(bas, manip_var = mv, angle = .1)
#' lab <- as.character(1:nrow(dat))
#' ggplot_tour(mt_array, dat) +
#'   ggproto_basis_axes() +
#'   ggproto_data_background() +
#'   ggproto_data_points(aes_args = list(color = clas, shape = clas),
#'                       identity_args = list(size= 1.5, alpha = .7)) +
#'   ggproto_data_text()
  debugonce(ggproto_data_points)
}

