#----------------------------------------------------------------------
# ps3.R
# Jin Rou New, 2014
#----------------------------------------------------------------------
rm(list = ls())
setwd("~/Documents/Berkeley/stat243/ps3")
library(XML)
library(stringr)
source("speech-functions.R")

output_dir <- "output"
dir.create(output_dir, showWarnings = FALSE)
urls_speech <- GetSpeechURLs()

speech_list_all <- lapply(urls_speech, GetAndProcessSpeech)
# Run the following if you get an error with lapply command
speech_list_all <- vector("list", length = length(urls_speech))
speech_error <- NULL 
for (i in seq_along(urls_speech)) {
  print(paste(i, urls_speech[i]))
  speech_list_all[[i]] <- try(GetAndProcessSpeech(urls_speech[i]))
  if (class(speech_list_all[[i]]) == "try-error") { 
    speech_error <- c(speech_error, i)
    next
  }
}
for (i in speech_error) {
  speech_list_all[[i]] <- GetAndProcessSpeech(urls_speech[i])
}
# Test for one speech
# url_speech <- "http://www.presidency.ucsb.edu/ws/index.php?pid=102826"
# speech_list <- GetAndProcessSpeech(url_speech)
names(speech_list_all) <- sapply(speech_list_all, function(speech_list) 
  paste(speech_list$speech_data_scalar$president, speech_list$speech_data_scalar$year))
save(speech_list_all, file = file.path(output_dir, "speech_list_all.rda"))