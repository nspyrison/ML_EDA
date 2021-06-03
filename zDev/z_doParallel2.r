require("doParallel")
.n_cores <- detectCores() - 1L
registerDoParallel(cores = .n_cores) #in windows by default 2, in general n-1
getDoParWorkers()

x <- 1:1E4
.len <- seq_along(x) ## or nrows(df)
#.idx <- rep(1:.n_cores, each = ceiling(.len / .n_cores))[1:.len]
.idx <- seq_along(x)

ret <- list()
(ptime <- system.time({
  ret <- foreach(
    i = .idx,
    #.combine = , ## A function to bind pieces together, if missing; list.
    .packages = c()) %dopar% { ## foreach(i=1:3) %dopar% sqrt(i)
      i + 1
    }
}))
