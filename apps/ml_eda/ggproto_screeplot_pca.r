#' Creates a data frame of the variance explained by the Principal Components.
#' 
#' @examples 
#' dat <- as.matrix(tourr::flea[, 1:6])
#' pca_obj <- prcomp(dat)
#' df_scree_pca(pca_obj)
df_scree_pca <- function(pca_obj){
  p <- length(pca_obj[[1L]])
  ## Scree table, PC variance explained
  data.frame(pc_num = 1L:p,
             pc_var = 100L * pca_obj$sdev^2L / sum(pca_obj$sdev^2L),
             cumsum_var = 100L * cumsum(pca_obj$sdev^2L) / sum(pca_obj$sdev^2L)
  )
}

#' Creates a screeplot of the variance explained by the Principal Components.
#' 
#' @examples 
#' dat <- as.matrix(tourr::flea[, 1:6])
#' pca_obj <- prcomp(dat)
#' 
#' palette(RColorBrewer::brewer.pal(8, "Dark2"))
#' ggplot2::ggplot() + ggproto_screeplot_pca(pca_obj)
#' 
#' ggplot2::ggplot() +
#'   ggproto_screeplot_pca(pca_obj = pca_obj) +
#'   ggplot2::theme_bw()

ggproto_screeplot_pca <- function(pca_obj){
  df_screetable_pca <- df_scree_pca(pca_obj)
  
  lab_fill <- "PC variance"
  lab_col  <- "Cummulative variance"
  ## List of ggproto's that is addable to a ggplot object.
  list(
    ## Individual feature bars
    ggplot2::geom_bar(ggplot2::aes(x = pc_num, y = pc_var, fill = lab_fill),
                      df_screetable_pca, stat = "identity"),
    ## Cumulative feature line
    ggplot2::geom_line(ggplot2::aes(x = pc_num, y = cumsum_var,
                                    color = lab_col, group = 1L),
                       df_screetable_pca, lwd = 1.2),
    ggplot2::geom_point(ggplot2::aes(x = pc_num, y = cumsum_var,
                                     color = lab_col),
                        df_screetable_pca, shape = 18L, size = 4L),
    ## Titles and colors
    ggplot2::labs(x = "Principal component", y = "% Variance explained",
                  fill = element_blank(), color = element_blank()),
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 0L),
                   legend.position = "bottom",
                   legend.direction = "horizontal"),
    ggplot2::scale_fill_manual(values = palette()[1L]),
    ggplot2::scale_colour_manual(values = palette()[2L])
  )
}

#' Creates a data frame of the variance explained by the Principal Components.
#' 
#' @examples 
#' dat <- as.matrix(tourr::flea[, 1:6])
#' pca_obj <- prcomp(dat)
#' est.pca(pca_obj)
est.pca <- function(pca_obj, var_cutoff = .9){
  cum_var <- df_scree_pca(pca_obj)$cumsum_var
  ret <- as.integer(
    min(
      which(cum_var > 100L * var_cutoff)
    )
  )
  names(ret) <- paste0("pca_cumvar=", 100L * var_cutoff, "%")
  return(ret)
}

#' @example 
#' dat <- as.matrix(tourr::flea[, 1:6])
#' ide_vect(data = dat, inc_slow = FALSE)
#' ide_vect(data = dat, inc_slow = TRUE)
ide_vect <- function(data, inc_slow = FALSE){
  ide_pca <- est.pca(prcomp(data), .9)
  ls_funcs <- list(Rdimtools::est.boxcount, Rdimtools::est.correlation,
                   Rdimtools::est.made, Rdimtools::est.mle2,
                   Rdimtools::est.twonn)
  nms <- c("est.boxcount", "est.correlation", "est.made", "est.mle2", "est.twonn")
  if(inc_slow == TRUE){
    ls_funcs <- c(
      ls_funcs, list(Rdimtools::est.clustering, Rdimtools::est.danco,
                     Rdimtools::est.gdistnn, Rdimtools::est.incisingball,
                     Rdimtools::est.mindkl, Rdimtools::est.Ustat
      ))
    nms <- c(nms, "est.clustering", "est.danco", "est.gdistnn", "est.incisingball", "est.mindkl", "est.Ustat")
  } ## est.incisingball prints histogram...
  ret <- sapply(1:length(ls_funcs), function(i){
    tryCatch(ls_funcs[[i]](data)$estdim,
             error=function(cond){
               message("Error in est.* function:")
               message(cond)
               return(NA)
             })
  })
  ret <- c(ide_pca, ret)
  names(ret) <- c(names(ide_pca), nms)
  return(ret)
}
