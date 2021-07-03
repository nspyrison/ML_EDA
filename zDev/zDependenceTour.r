require(spinifex)
?tourr::dependence_tour()

message("pos is the a vect of the columns numbers allowd to vary on that axis, so someing thing like:")
dat <- scale_sd(mtcars)
pos <- c(2, rep(1, ncol(dat)-1))
animate_xy(dat, dependence_tour(pos))

ct_path <- save_history(dat, dependence_tour(pos), max_bases = 5)
ggt <- ggtour(ct_path, data = dat) + proto_default()
animate_plotly(ggt)
