require(tourr)
require(spinifex)
dat <- scale_sd(flea[, 1:6])
clas <- flea$species
bas <- basis_pca(dat)

hist_gt <- save_history(dat, tour_path = grand_tour(), start = bas)
animate_pcp(dat, planned_tour(hist_gt))
animate(dat, planned_tour(hist_gt))


?freeze
frozen <- matrix(NA, nrow = nrow(bas), ncol = ncol(bas))
frozen[3, ] <- c(-.8, .8)
#bas_froze <- freeze(bas, frozen) ## seemingly not used in a frozen tour
animate_xy(dat, frozen_tour(frozen = frozen))
## Doesn't seem to to be consistent across sum of sq >/< 1, or column number

## zeroed -- works for col 3, but not 1
frozen <- matrix(NA, nrow = nrow(bas), ncol = ncol(bas))
frozen[3, ] <- 0
animate_xy(dat, frozen_tour(frozen = frozen))

## near full
frozen <- matrix(NA, nrow = nrow(bas), ncol = ncol(bas))
frozen[3, ] <- c(.95, 0)
animate_xy(dat, frozen_tour(frozen = frozen))

## single proj var.
frozen <- matrix(NA, nrow = nrow(bas), ncol = ncol(bas))
frozen[3, ] <- c(.5, NA)
animate_xy(dat, frozen_tour(frozen = frozen))

## by column -- limited
frozen <- matrix(NA, nrow = nrow(bas), ncol = ncol(bas))
frozen[, 1] <- c(NA, NA, .5, NA, .5, NA)
animate_xy(dat, frozen_tour(frozen = frozen))

## goal; want to be able to use a whole column but doesn't always seem to want to work. 
frozen <- matrix(NA, nrow = nrow(bas), ncol = ncol(bas))
frozen[c(3,5), 1] <- c(.9 * bas[c(3,5), 1])
animate_xy(dat, frozen_tour(frozen = frozen))


## can lock different variables.
frozen <- matrix(NA, nrow = nrow(bas), ncol = ncol(bas))
frozen[3, 1] <- frozen[5, 2] <- .5
animate_xy(dat, frozen_tour(frozen = frozen))
