rm(list = ls())
library(knitr)
setwd("~/Documents/Berkeley/stat243/ps4")
file <- "ps4.Rtex"
knit2pdf(file)
