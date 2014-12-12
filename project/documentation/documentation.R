#----------------------------------------------------------------------
# Produce documentation/help files
#----------------------------------------------------------------------
# Get Rd files in man folder
# devtools::document() 

# Convert Rd files to latex files
library(tools)
dir.create("documentation/texforfunctions", showWarnings = FALSE,
           recursive = TRUE)
functionnames <- list.files("ga-package/man")
functionnames <- functionnames[grepl("Rd", functionnames)]
functionnames <- gsub("\\.Rd", "", functionnames)
for (functionname in functionnames)
  Rd2latex(paste0("ga-package/man/", functionname, ".Rd"), 
           out = paste0("documentation/texforfunctions/", functionname, ".tex"), 
           outputEncoding = "UTF-8")

# Get tex file that inputs all functions
temp <- c("ga-package", "select_model")
cat(paste0("\\input{texforfunctions/", 
           c(temp, functionnames[!(functionnames %in% temp)]), ".tex} \n"), 
    file = paste("documentation/overviewallfunctions.tex"))
