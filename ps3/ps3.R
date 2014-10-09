#----------------------------------------------------------------------
# ps3.R
# Jin Rou New, 2014
#----------------------------------------------------------------------
rm(list = ls())
setwd("~/Documents/Berkeley/stat243/ps3")
library(XML)
library(stringr)
library(koRpus)
library(ggplot2)
library(gridExtra)
source("speech-functions.R")
output_dir <- "output"
#----------------------------------------------------------------------
# Get list of speech objects
dir.create(output_dir, showWarnings = FALSE)
if (!file.exists(file.path(output_dir, "speech_list_all.rda"))) {
  urls_speech <- GetSpeechURLs()
  speech_list_all <- lapply(urls_speech, GetAndProcessSpeech)
  names(speech_list_all) <- sapply(speech_list_all, function(speech_list) 
    paste(speech_list$speech_data_scalar$president, speech_list$speech_data_scalar$year))
  save(speech_list_all, file = file.path(output_dir, "speech_list_all.rda"))
} else {
  load(file.path(output_dir, "speech_list_all.rda"))
}
# Get speech data frame
if (!file.exists(file.path(output_dir, "speech_df.rda"))) {
  speech_df <- GetSpeechDataFrame(speech_list_all)
  save(speech_df, file = file.path(output_dir, "speech_df.rda"))
} else {
  load(file.path(output_dir, "speech_df.rda"))
}

# Make exploratory plots of variables over time
vars_to_plot <- c("num_words", "avg_word_nchars", "avg_sentence_nwords", 
                  "lexical_diversity", "readability_age",
                  "war", "free", "we", "America")
labels_to_plot <- c("Word\ncount", "Average length\nof words (# chars)", 
                    "Average length of\nsentences (# words)", 
                    "Lexical diversity (#\nunique words/# words)", 
                    "Flesch-Kincaid\nreadability score (Age)",
                    "war", "free", "we", "America")

p <- list()
theme_set(theme_bw(base_size = 10) + 
            theme(axis.title.x = element_text(vjust = -0.3),
                  axis.title.y = element_text(vjust = 1.1)))
for (i in seq_along(vars_to_plot)) {
  p[[i]] <- ggplot(speech_df, 
                   aes_string(x = "year", y = vars_to_plot[i], 
                              color = "president")) +
    geom_point() + 
    xlab("Year of address") + ylab(labels_to_plot[i]) + 
    scale_colour_hue(guide = FALSE)
}
# Plots of text variables
do.call(grid.arrange, 
        c(p[1:5], list(nrow = 5, ncol = 1,
                       main = textGrob("Change in speech variables over time",
                                       gp = gpar(font=2)))))
# Plots of word occurences
do.call(grid.arrange, 
        c(p[6:9], list(nrow = 4, ncol = 1, 
                       main = textGrob("Normalised # occurences of certain words",
                                       gp = gpar(font=2)))))

# Make boxplots comparing Republican and Democratic presidents
comp_vars_to_plot <- vars_to_plot[1:5]
comp_labels_to_plot <- labels_to_plot[1:5]
boxplots <- vector("list", length(comp_vars_to_plot))
for (i in seq_along(comp_vars_to_plot)) {
  boxplots[[i]] <- ggplot(speech_df[!is.na(speech_df$party), ], 
                          aes_string(x = "party", y = comp_vars_to_plot[i])) +
    geom_boxplot() + 
    xlab("") + 
    ylab(comp_labels_to_plot[i])
}
do.call(grid.arrange, 
        c(boxplots, list(nrow = 2, ncol = 3, 
                       main = textGrob("Comparison of Democrat vs Republican speeches",
                                       gp = gpar(font=2)))))
#----------------------------------------------------------------------
# F-K readability vs year bubble charts
speech_df$num_words_scaled <- sqrt(speech_df$num_words/pi)
p <- ggplot(speech_df, 
            aes_string(x = "year", y = "readability_age", 
                       color = "president", size = "num_words_scaled")) +
  geom_point(alpha = 0.5) +
  xlab("Year of address") + ylab(labels_to_plot[i]) + 
  scale_size_continuous(name = "# words", 
                        breaks = sqrt(c(2500, 5000, 10000)/pi), 
                        labels = c(2500, 5000, 10000)) + 
  scale_colour_hue(guide = FALSE)
print(p)
#----------------------------------------------------------------------
# Get10Words <- function(speech_list_all) {
#   word_freq_df_list <- lapply(speech_list_all, function(speech_list)
#     data.frame(speech = paste(speech_list$speech_data_scalar$president,
#                               speech_list$speech_data_scalar$year),
#                president = speech_list$speech_data_scalar$president,
#                year = speech_list$speech_data_scalar$year,
#                word = names(speech_list$speech_data_vector$word_frequencies),
#                frequency = speech_list$speech_data_vector$word_frequencies))
#   word_freq_df <- do.call(rbind, word_freq_df_list)
#   for (president in unique(word_freq_df$president)) {
#     select_president <- word_freq_df$president == president
#     select <- !(word_freq_df$word %in% word_freq_df$word[!select_president]) & 
#       select_president & word_freq_df$frequency > 2
#     if (any(select)) {
#       print(president)
#       print(data.frame(word = word_freq_df$word[select],
#             frequency = word_freq_df$frequency[select]))
#     }
#   }
# }
