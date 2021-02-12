data(LetterRecognition, package = "mlbench")
dat <- LetterRecognition[,2:17]
dim(dat)
#install.packages("microbenchmark")
library(microbenchmark)

message("Both PCA methods ~ same speed.;")
message("prcomp 23.86ms to princomp 25.61ms")## 
# microbenchmark::microbenchmark(prcomp(dat),
#                                 princomp(dat))

library(Rdimtools)
Rdimtools::est.made(dat)
ggplot() + ggproto_screeplot_pca(dat)


m <- as.matrix(tourr::flea[,1:6])
m <- as.matrix(spinifex::PimaIndiansDiabetes_long[,1:6])


message("round one filterng")

## Looking at time
microbenchmark::microbenchmark(
  Rdimtools::est.boxcount(m)$estdim, ## Bar, mean of 68.99 ms
  ## est.clustering(m)$estdim, ## ~ 24.8x slower
  Rdimtools::est.correlation(m)$estdim,
  ## est.danco(m)$estdim, ## ~ 26x slower
  ## est.gdistnn(m)$estdim, ## ~ 11x slower
  ## est.incisingball(m)$estdim, ## ~ 22x slower
  ## est.mindkl(m)$estdim, ## ~ 23x slower
  Rdimtools::est.made(m)$estdim,
  ##est.mle1(m)$estdim, ## mle2 uses a correctio term and both are about same speed
  Rdimtools::est.mle2(m)$estdim,
  Rdimtools::est.twonn(m)$estdim,
  Rdimtools::est.Ustat(m)$estdim, ## ~ 5x slower
  times = 10L
)


data(BostonHousing, package = "mlbench")
m<- BostonHousing
{## Looking at values
funcs <- list(Rdimtools::est.boxcount,
              Rdimtools::est.correlation,
              Rdimtools::est.made,
              Rdimtools::est.mle2,
              Rdimtools::est.twonn,
              Rdimtools::est.Ustat)
ests <- matrix(0, nrow=6, ncol=10)
tictoc::tic()
for (i in 1:6){
  for(j in 1:1){
    ests[i, j] <- funcs[[i]](m)$estdim
  }
}
tictoc::toc()
ests
}
require(tidyverse)
df <- ests[,1] %>% tibble()

ggplot() + geom_density(aes(.), df)


vecd = rep(0,5)
vecd[1] = Rdimtools::est.Ustat(m)$estdim # convergence rate of U-statistic on manifold
vecd[2] = Rdimtools::est.correlation(m)$estdim # correlation dimension
vecd[3] = Rdimtools::est.made(m)$estdim # manifold-adaptive dimension estimation
vecd[4] = Rdimtools::est.mle2(m)$estdim # MLE with Poisson process
vecd[5] = Rdimtools::est.twonn(m)$estdim # minimal neighborhood information
ls_funcs <- list(est.boxcount, est.clustering, est.correlation, est.danco, )
lapply(1:length(ls_funcs), function(i){ls_funcs[[i]](m)$estdim})

est.boxcount(m)$estdim
vecd


library(Rtsne)
Rtsne::Rtsne(, dims = 2, pca = FALSE,
             perplexity = (nrow(x) - 1)/3,
             max_iter = 250)


#install.packages("randomForest")
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
