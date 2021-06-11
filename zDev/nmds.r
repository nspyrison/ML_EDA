
require("spinifex")
require("MASS")
require('ggfortify')

dim(spinifex::PimaIndiansDiabetes_wide)
dat <- spinifex::PimaIndiansDiabetes_wide

nmds <- MASS::isoMDS(dist(dat))
plot(nmds$points, type = "n")
text(nmds$points, labels = as.character(1:nrow(dat)))
