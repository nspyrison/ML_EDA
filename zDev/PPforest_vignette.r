install.packages("PPforest")
require("PPforest")
vignette("PPforest-vignette")

Tree.crab <- PPforest::PPtree_split("Type~.", data = crab, PPmethod = "LDA", size.p = 0.6)
Tree.crab
