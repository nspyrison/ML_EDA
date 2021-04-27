require(tourr)
?frozen_tour ## Also not the Not run: code and comments in the examples section.

#### Works for some variables but not others. ------
## Example case; expected success
frozen <- matrix(NA, nrow = 4, ncol = 2)
frozen[3, ] <- .5
if(interactive())
  animate_xy(flea[, 1:4], frozen_tour(2, frozen)) 

## Trivial variable change; unexpected error
frozen <- matrix(NA, nrow = 4, ncol = 2)
frozen[1, ] <- .5
animate_xy(flea[, 1:4], frozen_tour(2, frozen))

#### Check is column norm >1, also need to check row norm? ------
## Norm > 1; expected error
frozen <- matrix(NA, nrow = 4, ncol = 2)
frozen[3, ] <- 1.1 
animate_xy(flea[, 1:4], frozen_tour(2, frozen))

## Norm > 1, variable contribution larger than 1; unexpected success
frozen <- matrix(NA, nrow = 4, ncol = 2)
frozen[3, ] <- .9
if(interactive())
  animate_xy(flea[, 1:4], frozen_tour(2, frozen))

## Norm > 1, each row < 1, but col norm > 1; expected error
frozen <- matrix(NA, nrow = 4, ncol = 2)
frozen[c(3, 4), ] <- .9
animate_xy(flea[, 1:4], frozen_tour(2, frozen))
