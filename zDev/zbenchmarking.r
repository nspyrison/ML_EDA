data(LetterRecognition, package = "mlbench")
dat <- LetterRecognition[,2:17]
#install.packages("microbenchmark")
library(microbenchmark)
microbenchmark::microbenchmark(prcomp(dat),
                                princomp(dat))

library(Rdimtools)
Rdimtools::est.made(dat)
ggplot() + ggproto_screeplot_pca(dat)


m <- as.matrix(tourr::flea[,1:6])
m <- as.matrix(spinifex::PimaIndiansDiabetes_long[,1:6])

microbenchmark::microbenchmark(
  est.boxcount(m)$estdim,
  est.clustering(m)$estdim, 
  est.correlation(m)$estdim, 
  est.danco(m)$estdim,
  est.gdistnn(m)$estdim,
  est.incisingball(m)$estdim,
  est.made(m)$estdim,
  est.Ustat(m)$estdim,
  est.mindkl(m)$estdim,
                               est.correlation(m)$estdim,
                               est.made(m)$estdim,
                               est.mle1(m)$estdim, 
                               est.twonn(m)$estdim,
                               times = 25L
)

vecd = rep(0,5)
vecd[1] = est.Ustat(m)$estdim # convergence rate of U-statistic on manifold
vecd[2] = est.correlation(m)$estdim # correlation dimension
vecd[3] = est.made(m)$estdim # manifold-adaptive dimension estimation
vecd[4] = est.mle1(m)$estdim # MLE with Poisson process
vecd[4] = est.mle2(m)$estdim # MLE with Poisson process
vecd[5] = est.twonn(m)$estdim # minimal neighborhood information
ls_funcs <- list(est.boxcount, est.clustering, est.correlation, est.danco, )
lapply(1:length(ls_funcs), function(i){ls_funcs[[i]](m)$estdim})

est.boxcount(m)$estdim
vecd


library(Rtsne)
Rtsne::Rtsne(, dims = 2, pca = FALSE,
             perplexity = (nrow(x) - 1)/3,
             max_iter = 250)


install.packages("randomForest")
library(randomForest)
set.seed(4543)
data(mtcars)
mtcars.rf <- randomForest(mpg ~ ., data=mtcars, ntree=1000,
                          keep.forest=FALSE, importance=TRUE)
importance(mtcars.rf)
importance(mtcars.rf, type=1)

proj <- prcomp(mtcars[, 2:11])$x
proj_tgt <- data.frame(proj, mpg = mtcars$mpg)
proj_fit <- randomForest(mpg ~ ., data = proj_tgt, ntree=1000,
                         keep.forest=FALSE, importance=TRUE)
varImpPlot(proj_fit, type=2)


pca_obj <- prcomp(mtcars)
.cum_var <- df_scree_pca(pca_obj)$cumsum_var
est_idd <- min(which(.cum_var > 90))
p <- length(pca_obj[[1L]])
df_rect <- data.frame(lb = .5,
                      mb = est_idd +.5,
                      ub = p + .)
#browser()
ggplot() + 
  geom_rect(aes(xmin = lb, xmax = mb, ymin = -Inf, ymax = Inf),
            df_rect, fill = "aquamarine", alpha = 0.3) +
  geom_rect(aes(xmin = mb, xmax = ub, ymin = -Inf, ymax = Inf),
            df_rect, fill = "firebrick1", alpha = 0.3) +
  ggproto_screeplot_pca(pca_obj) + 
  theme_minimal()
