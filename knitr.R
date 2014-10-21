rm(list = ls())
library(knitr)
setwd("~/Copy/Berkeley/stat243/ps5")
file <- "ps5.Rtex"
knit2pdf(file)
