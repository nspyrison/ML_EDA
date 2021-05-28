### UTIL -----
#' @examples
#' dat <- scale_sd(::flea[, 1:6])
#' bas <- basis_pca(dat)
#' mv  <- manip_var_of(bas)
#' mt_array <- manual_tour(bas, manip_var = mv, angle = .1)
#' ggplot_tour(mt_array, dat) ## litters .spinifex_df_basis, .spinifex_df_data, .spinifex_scale_to
#' 
ggplot_tour <- function(basis_array, data = NULL,
                        angle = .05, ## Not applicable for manual tour.
                        scale_to = c("data", NULL, "density")
                        ## Class/classification for method default values?
){
  if(is.null(data) == TRUE)
    data <- attr(basis_array, "data") ## Can be NULL
  if(any(class(basis_array) %in% c("matrix", "data.frame"))) ## Format for array2df (_list)
    basis_array <- array(as.matrix(basis_array), dim = c(dim(basis_array), 1L))
  
  ## array2df_version2(), approximately
  manip_var <- attr(basis_array, "manip_var") ## NULL if not a manual tour
  if(is.null(manip_var)) ## If not a manual tour interpolate
    basis_array <- tourr::interpolate(basis_array, angle = angle)
  df_ls <- array2df(basis_array, data)
  df_basis <- df_ls$basis_frames
  df_data  <- df_ls$data_frames
  attr(df_basis, "manip_var") <- manip_var ## NULL if not a manual tour
  
  ## Set scale_to to df_data else unit box, for spinifex::scale_axes(to = .scale_to)
  scale_to <- match.arg(scale_to)
  if(is.null(scale_to) == FALSE){
    scale_to <- switch(scale_to,
                       data = df_data,
                       density = {
                         .den <- density(df_data[, 1L])
                         density = data.frame(x = quantile(df_data[, 1L], 
                                                           probs = c(.05, .95)),
                                              y = 3 * range(.den[[2L]]))
                       })
  } else ## is .scale to is NULL
    scale_to <- data.frame(x = c(-1L, 1L), y = c(-1L, 1L))
  
  ## Assign hidden prepared dataframes
  assign(x = ".spinifex_df_basis", value = df_basis, envir = globalenv())
  assign(x = ".spinifex_df_data",  value = df_data,  envir = globalenv())
  assign(x = ".spinifex_scale_to", value = scale_to,  envir = globalenv())
  
  ## Also behave as ggpot() with overwritable theme settings
  ret <- ggplot2::ggplot() + spinifex::theme_spinifex()
  
  return(ret)
}
# ## Print method
# #### Was a good idea, but ggplot stops working when you change the first class, 
# #### and doesn't effect if you append.
# print.ggtour <- function(x, ...){
#   class(x) <- c("gg", "ggplot")
#   x +
#     ggproto_basis_axes() +
#     ggproto_data_background(gridline_probs = FALSE) +
#     ggproto_data_points()
# }

lapply_rep_len <- function(list, nrow_array, nrow_data){
  .nms <- names(list)
  .mute <- lapply(seq_along(list), function(i){
    .this_vector <- list[[i]]
    if(length(.this_vector) %in% c(1L, nrow_data) == FALSE)
      warning(paste0("aes_arg '", .nms[i], "' not of length 1 or data."))
    .replicated_vector <- rep_len(.this_vector, nrow_array)
    list[[i]] <<- .replicated_vector ## Replace the value with the string of the orig
  })
  return(list)
}

## Initialize common obj from hidden tour objects, test it's existance
.init_ggproto <- function(){
  ## Assumption
  if(exists(".spinifex_df_basis") == FALSE) 
    stop(paste0("`.spinifex_df_basis` does not exsist, have you run `ggplot_tour` yet?"))
  
  ## Initialization, littering hidden objects 1 level up, not in global.
  .df_basis <<- .spinifex_df_basis ## Give alterable local copies of _basis and _data
  .df_data  <<- .spinifex_df_data
  .scale_to <<- .spinifex_scale_to
  .n_frames <<- length(unique(.df_basis$frame))
  .p <<- nrow(.df_basis) / .n_frames
  .n <<- nrow(.df_data)  / .n_frames
  .manip_var <<- attr(.df_basis, "manip_var") ## NULL if not a manual tour
  
  ## For some reason  cannot replicate ls args here; not working, too many levels/envirnments?
  return()
}

### ANIMATE ------


#' @examples
#' dat <- scale_sd(::flea[, 1:6])
#' clas <- flea[, 7]
#' bas <- basis_pca(dat)
#' mv  <- manip_var_of(bas)
#' mt_array <- manual_tour(bas, manip_var = mv, angle = .1)
#' lab <- as.character(1:nrow(dat))
#' ggtour <- ggplot_tour(mt_array, dat) +
#'   ggproto_basis_axes() +
#'   ggproto_data_background() +
#'   ggproto_data_points(aes_args = list(color = clas, shape = clas),
#'                       identity_args = list(size= 1.5, alpha = .7))
#' 
#' \dontrun{
#' animate_gganimate(ggtour)
#' 
#' (anim <-
#'   animate_gganimate(ggtour, fps = 10, rewind = TRUE,
#'                     start_pause = 1, end_pause = 2))
#' 
#' gganimate::anim_save("my_tour.gif",
#'                      animation = anim,
#'                      path = "./figures")
#' }
animate_gganimate <- function(
  ggtour, fps = 8, rewind = FALSE, start_pause = 0.5, end_pause = 1,
  knit_pdf_anim = FALSE, ## Do ignore fps:end_pause, and route to gganimate::knit_print.gganim()?
  ... ## Passed to gganimate::animate or gganimate::knit_print.gganim
){
  requireNamespace("gganimate")
  gga <- ggtour +
    gganimate::transition_states(frame, transition_length = 0L)
  
  ## Pdf animation, early return
  if(knit_pdf_anim == TRUE){
    pdf_anim <- knit_print.gganim(gga, ...)
    return(pdf_anim)
  }
  
  ## Normal animation, with applied options, knit_pdf_anim == FALSE
  anim <- gganimate::animate(
    gga, fps = fps, rewind = rewind,
    start_pause = fps * start_pause,
    end_pause = fps * end_pause, 
    ...)
  return(anim)
}

#' @examples
#' dat <- scale_sd(::flea[, 1:6])
#' clas <- flea[, 7]
#' bas <- basis_pca(dat)
#' mv  <- manip_var_of(bas)
#' mt_array <- manual_tour(bas, manip_var = mv, angle = .1)
#' lab <- as.character(1:nrow(dat))
#' ggtour <- ggplot_tour(mt_array, dat) +
#'   ggproto_basis_axes() +
#'   ggproto_data_background() +
#'   ggproto_data_points(aes_args = list(color = clas, shape = clas),
#'                       identity_args = list(size= 1.5, alpha = .7))
#' 
#' \dontrun{
#' animate_gganimate(ggtour)
#' 
#' (anim <-
#'    animate_plotly(ggtour, fps = 10, rewind = TRUE,
#'                   start_pause = 1, end_pause = 2))
#'                            
#' htmlwidgets::saveWidget(widget = anim, file = "./figures/my_tour.html",
#'                         selfcontained = TRUE)
#' }
animate_plotly <- function(
  ggtour, fps = 8, #use_rowname_tooltip = TRUE,
  ... ## Passed to plotly::layout.
){
  requireNamespace("plotly")
  gg <- ggtour + 
    ## to block plotly.js warning: supoort of horizontal legend;
    #### https://github.com/plotly/plotly.js/issues/53 
    ggplot2::theme(legend.position = "right",
                   legend.direction = "vertical",
                   legend.box = "horizontal")
  ggp <- plotly::ggplotly(p = gg)
  ggp <- plotly::animation_opts(p = ggp, frame = 1L / fps * 1000L,
                                transition = 0L, redraw = FALSE)
  ggp <- plotly::layout(
    ggp,
    showlegend = FALSE,
    yaxis = list(showgrid = FALSE, showline = FALSE), ##  fixedrange = TRUE is a curse.
    xaxis = list(showgrid = FALSE, showline = FALSE,
                 scaleanchor = "y", scalaratio = 1L),
    ...
  )
  ggp <- plotly::config(ggp, displayModeBar = FALSE)
  return(ggp)
}


### GGRPROTO_BASIS_* ------
ggproto_basis_axes <- function(position = "left", manip_col = "blue",
                               line_size = 1, text_size = 5){
  ## Assumptions
  if(position == "off") return()
  
  ## Initialize
  .init_ggproto()
  
  ## Setup and transform
  .angles <- seq(0L, 2L * pi, length = 360L)
  .circle <- data.frame(x = cos(.angles), y = sin(.angles))
  .center <- scale_axes(data.frame(x = 0L, y = 0L), position, .scale_to)
  .circle <- scale_axes(.circle, position, .scale_to)
  .df_basis <- scale_axes(.df_basis, position, .scale_to)
  ## Aesthetics for the axes segments.
  .axes_col <- "grey50"
  .axes_siz <- line_size
  if (is.null(.manip_var) == FALSE) {
    .axes_col <- rep("grey50", .p)
    .axes_col[.manip_var] <- manip_col
    .axes_col <- rep(.axes_col, .n_frames)
    .axes_siz <- rep(line_size, .p)
    .axes_siz[.manip_var] <- 1.5 * line_size
    .axes_siz <- rep(.axes_siz, .n_frames)
  }
  ## Return ggproto of basis axes in unit circle
  return(
    list(
      ggplot2::geom_path(data = .circle, color = "grey80",
                         size = line_size, inherit.aes = FALSE,
                         mapping = ggplot2::aes(x = x, y = y)),
        suppressWarnings(ggplot2::geom_segment( ## Suppress unused arg: frames
          data = .df_basis,
          size = .axes_siz, colour = .axes_col,
          mapping = ggplot2::aes(x = x, y = y, frame = frame,
                                 xend = .center[, 1L], yend = .center[, 2L])
        )),
      suppressWarnings(ggplot2::geom_text(
        data = .df_basis,
        colour = .axes_col, size = text_size,
        vjust = "outward", hjust = "outward",
        mapping = ggplot2::aes(x = x, y = y, frame = frame, label = label)
      ))
    )
  )
}


#' @examples
#' dat <- scale_sd(tourr::flea[, 1:6])
#' clas <- tourr::flea[, 7]
#' bas <- basis_pca(dat)
#' mv  <- manip_var_of(bas)
#' mt_array <- manual_tour(bas, manip_var = mv, angle = .1)
#'
#' ggt <- ggplot_tour(mt_array, dat) +
#'   ggproto_basis_axes1d() +
#'   ggproto_data_background() +
#'   ggproto_data_points(aes_args = list(color = clas, shape = clas),
#'                       identity_args = list(size= 1.5, alpha = .7))
#' #animate_plotly(ggt) ## plotly::ggplotly() throwing error: 
#' ## Error in -data$group : invalid argument to unary operator
#' animate_gganimate(ggt)
ggproto_basis_axes1d <- function(position = "left", manip_col = "blue",
                                 segment_size = 2, text_size = 5){
  ## Assumptions
  if(position == "off") return()
  
  ## Initialize
  .init_ggproto()
  
  ## Aesthetics for the axes segments
  .axes_col <- "grey50"
  .axes_siz <- segment_size
  if(is.null(.manip_var) == FALSE) {
    .axes_col <- rep("grey50", .p)
    .axes_col[.manip_var] <- manip_col
    .axes_siz <- rep(segment_size, .p)
    .axes_siz[.manip_var] <- 1.5 * segment_size
  }
  ## Initialize data.frames, before scaling
  .frame1  <- .df_basis[.df_basis$frame == 1L, ]
  .df_zero <- data.frame(x = 0L, y = 0L)
  .df_seg  <- data.frame(x = .df_basis$x,
                         y = rep_len(.p:1L, length.out = nrow(.df_basis)),
                         frame = .df_basis$frame,
                         label = .df_basis$label)
  .df_txt  <- data.frame(x = -1.33, y = .p:1L, label = .frame1$label)
  .df_rect <- data.frame(x = c(-1L, 1L), y = c(.5, .p + .5))
  .df_seg0 <- data.frame(x = 0L, y = c(.5, .p + .5))
  ## Scale them
  .df_zero <- scale_axes(.df_zero, position, .scale_to)
  .df_seg  <- scale_axes(.df_seg, position, .scale_to)
  .df_txt  <- scale_axes(.df_txt, position, .scale_to)
  .df_rect <- scale_axes(.df_rect, position, .scale_to)
  .df_seg0 <- scale_axes(.df_seg0, position, .scale_to)
  
  ## Return ggproto of basis axes in barplot table
  return(list(
    geom_segment(aes(x = min(x), y = min(y), xend = max(x), yend = max(y)),
                 .df_seg0, color = "grey80", linetype = 2L),
    geom_rect(aes(xmin = min(x), xmax = max(x), ymin = min(y), ymax = max(y)),
              .df_rect, fill = NA, color = "grey60"),
    geom_text(aes(x, y, label = label), .df_txt, size = text_size, color = .axes_col),
    suppressWarnings(geom_segment(
      aes(x, y, xend = .df_zero[,1L], yend = y, frame = frame),
      .df_seg, color = rep(.axes_col, .n_frames), size = rep(.axes_siz, .n_frames)))
  ))
}


### GGPROTO_DATA_* ----

##TODO: gridlines still not lined up wit zero mark as it's powered from the extrema, not 0.
ggproto_data_background <- function(zero_mark = TRUE,
                                    gridlines = 0){
  ## Assumptions
  if(is.null(.spinifex_df_data) == TRUE) return()
  position <- "center" ## Data assumed center.
  
  ## Initialize
  .init_ggproto()
  
  ## Setup and transform
  ret <- list() ## Init
  #### gridlines
  if(is.numeric(gridlines) & gridlines > 1L){
    .rates <- seq(0L, 1L, 1L / round(gridlines - 1L, 0L))
    .min <- min(min(.scale_to[, 1L]), min(.scale_to[, 2L]))
    .max <- max(max(.scale_to[, 1L]), max(.scale_to[, 2L]))
    .x_min <- min(.scale_to[, 1L])
    .y_min <- min(.scale_to[, 12])
    .grid1d <- .min + .rates * (.max - .min)
    #.grid1d <- .grid1d - median(.grid1d) ## center
    .len <- length(.rates)
    .df_gridlines <- data.frame(
      x     = c(rep(.x_min, .len), .grid1d),
      x_end = c(rep(.x_min, .len), .grid1d),
      y     = c(.grid1d, rep(.y_min, .len)),
      y_end = c(.grid1d, rep(.y_min, .len))
    )
    
    gridlines <- ggplot2::geom_segment(
      data = .df_gridlines,
      color = "grey80", size = .5, alpha = .5,
      mapping = ggplot2::aes(x = x, y = y, xend = x_end, yend = y_end)
    )
    ret <- c(ret, gridlines)
  }
  
  #### Setup zero mark, 5% on each side.
  if(zero_mark == TRUE){
    .center <- scale_axes(data.frame(x = 0L, y = 0L), position, .scale_to)
    .min <- min(min(.scale_to[, 1L]), min(.scale_to[, 2L]))
    .max <- max(max(.scale_to[, 1L]), max(.scale_to[, 2L]))
    .tail <- .05 * (.max - .min)
    
    .df_zero_mark <-
      data.frame(x     = c(.center[, 1L] - .tail, .center[, 1L]),
                 x_end = c(.center[, 1L] + .tail, .center[, 1L]),
                 y     = c(.center[, 2L], .center[, 2L] - .tail),
                 y_end = c(.center[, 2L], .center[, 2L] + .tail)
      )
    
    zero_mark <- ggplot2::geom_segment(
      data = .df_zero_mark,
      color = "grey60", size = 1L, alpha = .7,
      mapping = ggplot2::aes(x = x, y = y, xend = x_end, yend = y_end)
    )
    ret <- c(ret, zero_mark)
  }
  
  ## Return
  return(ret)
}



#' @examples
#' dat <- scale_sd(mtcars)
#' bas <- basis_pca(dat)
#' gt_array <- save_history(dat, grand_tour(), max = 3, start = bas)
#' 
#' gg <- ggplot_tour(gt_array, dat) +
#'   ggproto_basis_axes() +
#'   ggproto_data_points() +
#'   ggproto_data_hex()
#' 
#' animate_gganimate(gg)
ggproto_data_points <- function(aes_args = list(),
                                identity_args = list()){
  ## Assumptions
  if(is.null(.spinifex_df_data) == TRUE) return()
  position <- "center" ## Data assumed center.
  aes_args <- as.list(aes_args)
  identity_args <- as.list(identity_args)
  
  ## Initialize, inc replicating arg lists.
  .init_ggproto()
  .tgt_len <- nrow(.df_data)
  aes_args <- lapply_rep_len(aes_args, nrow_array = .tgt_len, nrow_data = .n)
  identity_args <- lapply_rep_len(identity_args, nrow_array = .tgt_len, nrow_data = .n)
  
  ## do.call aes() over the aes_args
  .aes_func <- function(...)
    ggplot2::aes(x = x, y = y, frame = frame, ...)
  .aes_call <- do.call(.aes_func, aes_args)
  ## do.call geom_point() over the identity_args
  .geom_func <- function(...) suppressWarnings(
    ggplot2::geom_point(mapping = .aes_call, data = .df_data, ...))
  .geom_call <- do.call(.geom_func, identity_args)
  
  ## Return ggproto of projection points
  return(.geom_call)
}

#' @examples
#' dat <- scale_sd(tourr::flea[, 1:6])
#' clas <- tourr::flea$species
#' bas <- basis_pca(dat)
#' gt_array <- save_history(dat, grand_tour(), max = 3, start = bas)
#' 
#' tictoc::tic("gg assign")
#' gg <- ggplot_tour(gt_array, dat, scale_to = "density") +
#'   ggproto_basis_axes() +
#'   ggproto_data_density1d_rug(aes_args = list(color = clas, fill = clas))
#' tictoc::toc()
#' 
#' tictoc::tic("animate")
#' animate_gganimate(gg) ## ~48 seconds 
#' tictoc::toc()
ggproto_data_density1d_rug <- function(aes_args = list(),
                                       identity_args = list()){
  ## Assumptions
  if(is.null(.spinifex_df_data) == TRUE) return()
  position <- "center" ## Data assumed center.
  aes_args <- as.list(aes_args)
  identity_args <- as.list(identity_args)
  
  ## Initialize, inc replicating arg lists.
  .init_ggproto()
  ## Calculate a new scale value of the density 
  
  ## Rep ls of args
  .tgt_len <- nrow(.df_data)
  aes_args <- lapply_rep_len(aes_args, nrow_array = .tgt_len, nrow_data = .n)
  identity_args <- lapply_rep_len(identity_args, nrow_array = .tgt_len, nrow_data = .n)
  
  ## do.call aes() over the aes_args
  .aes_func <- function(...)
    ggplot2::aes(x = x, frame = frame, ...)
  .aes_call <- do.call(.aes_func, aes_args)
  ## do.call geom over identity_args
  .geom_func1 <- function(...) suppressWarnings(ggplot2::geom_density(
    mapping = .aes_call, data = .df_data, position = "stack", ...))
  .geom_call1 <- do.call(.geom_func1, identity_args)
  ## do.call geom over identity_args #2:
  .geom_func2 <- function(...) suppressWarnings(ggplot2::geom_rug(
    mapping = .aes_call, data = .df_data, ...))
  .geom_call2 <- do.call(.geom_func2, identity_args)
  
  ## Return ggproto of projection points
  return(list(.geom_call1, .geom_call2))
}


## Printing as points
ggproto_data_text <- function(aes_args = list(label = as.character(1:nrow(dat))),
                              identity_args = list()){
  ## Assumptions
  if(is.null(.spinifex_df_data) == TRUE) return()
  position <- "center" ## Data assumed center.
  aes_args <- as.list(aes_args)
  identity_args <- as.list(identity_args)
  
  ## Initialize, inc replicating arg lists.
  .init_ggproto()
  
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

#' @examples
#' dsmall <- diamonds[sample(nrow(diamonds), 1000),]
#' dat <- scale_sd(dsmall[, c(1, 5:6, 8:10)])
#' clas <- dsmall$color
#' bas <- basis_pca(dat)
#' gt_array <- save_history(dat, grand_tour(), max = 3, start = bas)
#' 
#' ggp <- ggplot_tour(gt_array, dat) +
#'   ggproto_basis_axes() +
#'   ggproto_data_hex(50, list(group = clas, fill= clas) ) +
#'   ggproto_data_background()
#' animate_gganimate(ggp)
## Printing as points
ggproto_data_hex <- function(bins = 30,
                             aes_args = list(),
                             identity_args = list()){
  ## Assumptions
  if(is.null(.spinifex_df_data) == TRUE) return()
  position <- "center" ## Data assumed center.
  aes_args <- as.list(aes_args)
  identity_args <- as.list(identity_args)
  
  ## Initialize, inc replicating arg lists.
  .init_ggproto()
  
  ## do.call aes() over the aes_args
  .aes_func <- function(...)
    ggplot2::aes(x = x, y = y, frame = frame, group = frame, ...)
  .aes_call <- do.call(.aes_func, aes_args)
  ## do.call geom_point() over the identity_args
  .geom_func <- function(...){
    suppressWarnings(
      ggplot2::geom_hex(mapping = .aes_call, data = .df_data, bins = bins, ...)
    )
  }
  .geom_call <- do.call(.geom_func, identity_args)
  
  ## Return ggproto
  return(.geom_call)
}



##== TESTING ==##
if(interactive()){ 
  require("ggplot2")
  require("spinifex")
  require("dplyr")
  require("hdrcde")
}
##  GGPLOT RETURN BEFORE ANIM
#' @examples
#' dat <- scale_sd(mtcars)
#' bas <- basis_pca(dat)
#' gt_array <- save_history(dat, grand_tour(), max = 3, start = bas)
#' 
#' ggplot_tour(gt_array, dat) +
#'   ggproto_basis_axes() +
#'   ggproto_data_background() +
#'   ggproto_data_points()
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


