rm(list = ls())
library(knitr)
setwd("~/Documents/Berkeley/stat243/ps2")
file <- "ps2.Rtex"
knit2pdf(file)
