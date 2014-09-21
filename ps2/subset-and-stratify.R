#!/usr/bin/Rscript
#----------------------------------------------------------------------
# subset-and-stratify.R
# Jin Rou New, 2014
#----------------------------------------------------------------------
# To run R script in Terminal, type:
# chmod ugo+x subset-and-stratify.R
# ./subset-and-stratify.R

# Parse arguments
args <- commandArgs(TRUE)
stopifnot(length(args) == 4)
data_filepath <- args[1]
var_stratify <- as.integer(args[2]) # Column index to stratify on
var_subset <- as.integer(args[3]) # Column index to subset on
value_subset <- as.character(args[4]) # Value to subset on

# # For testing
# data_filepath <- "AirlineData2006-2008.csv.bz2"
# var_stratify <- 1 # Year
# var_subset <- 18 # Flight destination
# value_subset <- "SFO"

output_dir <- "output"
dir.create(output_dir, showWarnings = FALSE) # Set up output directory
con <- bzfile(data_filepath, "rt") # Open input file connection (for reading in text mode)
nrows_block <- 50000 # Read from input file in blocks of size nrows_block
header <- read.csv(con, header = FALSE, nrows = 1)
data <- read.csv(con, header = FALSE, nrows = nrows_block) # Read in first block
while (!class(data) == "try-error") { # Read and process blocks while there are still rows to be read and processed
  data_subset <- data[as.character(data[, var_subset]) == value_subset, ] # Subset data
  data_strata_list <- split(data_subset, data_subset[, var_stratify]) # Stratify data
  for (stratum in names(data_strata_list)) { # Output data strata to respective bz2 files
    data_filepath_out <- file.path(output_dir, paste0(stratum, ".csv.bz2"))
    con_out <- bzfile(data_filepath_out, "at") # Open output file connection (for appending in text mode)
    write.table(data_strata_list[[stratum]], row.names = FALSE, col.names = FALSE, sep = ",", 
                file = con_out, append = TRUE) # Write to file connection
    close(con_out) # Close output file connection
  }
  data <- try(read.csv(con, header = FALSE, nrows = nrows_block)) # Read next block
}
closeAllConnections()
