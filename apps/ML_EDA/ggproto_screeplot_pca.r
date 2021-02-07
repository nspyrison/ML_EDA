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

