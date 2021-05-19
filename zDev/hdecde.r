require(hdrcde)

?hdrcde::hdrscatterplot

x <- c(rnorm(200, 0, 1), rnorm(200, 4, 1))
y <- c(rnorm(200, 0, 1), rnorm(200, 4, 1))
hdrscatterplot(x, y)
hdrscatterplot(x, y, label = paste0("p", 1:length(x)))
hdrcde::hdrscatterplot(x, y)

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