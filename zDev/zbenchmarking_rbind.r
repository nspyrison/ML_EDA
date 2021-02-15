require(profvis)
require(shiny)

profvis({
  runApp("apps/ML_EDA")
})




library(microbenchmark)
dat <- as.matrix(diamonds[1:10000, 5:10])
bas_array <- array(NA, dim = c(6,2,10)) ##init
mute <- sapply(1:10, function(i){
  bas_array[,, i] <<- tourr::basis_random(n = 6)
})


baseline <- quote({
  mat_frame <- NULL
  mute <- sapply(1L:10L, function(i){ ## Vectorization of for, does behave slightly diff than for loop.
    new_frame <- dat %*% bas_array[,, i]
    ## Center the new frame
    new_frame[, 1] <- new_frame[, 1] - mean(new_frame[, 1])
    new_frame[, 2] <- new_frame[, 2] - mean(new_frame[, 2])
    new_frame <- cbind(new_frame, i) ## Append frame number
    mat_frame <<- rbind(mat_frame, new_frame) ## Add rows to df
  })
})

alt1 <- quote({
  frame_ls <- list()
  mute <- sapply(1L:10L, function(i){ ## Vectorization of for, does behave slightly diff than for loop.
    new_frame <- dat %*% bas_array[,, i]
    ## Center the new frame
    new_frame[, 1] <- new_frame[, 1] - mean(new_frame[, 1])
    new_frame[, 2] <- new_frame[, 2] - mean(new_frame[, 2])
    frame_ls[[i]] <<- data.frame(new_frame, i) ## Add rows to df
  })
  mat_frame <- data.table::rbindlist(as.list(frame_ls))
})


microbenchmark(times = 10L,
  eval(baseline),
  eval(alt1)
)

dat <- data.frame(diamonds[1:10000, 5:10])
dat_mat <- as.matrix(dat)
str(dat)
microbenchmark(times = 100L,
               as.matrix(dat),
               as.matrix(dat_mat),
               is.matrix(dat_mat),
               if(is.matrix(dat)) as.matrix(dat)
)

