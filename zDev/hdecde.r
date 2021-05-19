require(hdrcde)

#### hdrcde::hdrscatterplot ---- 
?hdrcde::hdrscatterplot

x <- c(rnorm(200, 0, 1), rnorm(200, 4, 1))
y <- c(rnorm(200, 0, 1), rnorm(200, 4, 1))
hdrscatterplot(x, y)
hdrscatterplot(x, y, label = paste0("p", 1:length(x)))
hdrcde::hdrscatterplot(x, y)

print("would like something like this, but I think geom_density_2d will be an easier first step.")


require("ggplot2")
require("tictoc")
?geom_density2d
?geom_hex
if(F)
  browseURL("https://www.r-graph-gallery.com/2d-density-plot-with-ggplot2.html")

# Show the contour only
# Data
a <- data.frame( x=rnorm(20000, 10, 1.9), y=rnorm(20000, 10, 1.2) )
b <- data.frame( x=rnorm(20000, 14.5, 1.9), y=rnorm(20000, 14.5, 1.9) )
c <- data.frame( x=rnorm(20000, 9.5, 1.9), y=rnorm(20000, 15.5, 1.9) )
data <- rbind(a,b,c)

{
tic()
ggplot(data, aes(x=x, y=y) ) +
  geom_density_2d()
toc()
}{
tic()
# Show the area only
ggplot(data, aes(x=x, y=y) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")
toc()
}{
tic()
# Area + contour
ggplot(data, aes(x=x, y=y) ) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon", colour="white")
toc()
}

{ 
  tic()
  # Area + contour
  hdrcde::hdrscatterplot(data$x, data$y)
  toc()
}

{ 
  tic()
  # Area + contour
  ggplot(data, aes(x=x, y=y) ) +
    geom_hex()
  toc()
}


#### hdrcde::cde, not promising ---- 
if(F){
  print("Nahhh, I think I just want HDR.")
  ?cde
  
  # Old faithful data
  faithful.cde <- cde(faithful$waiting, faithful$eruptions,
                      x.name="Waiting time", y.name="Duration time")
  plot(faithful.cde)
  plot(faithful.cde, plot.fn="hdr")
  class(faithful.cde)
  
  ?hdrcde::plot.cde
}