rm(list = ls())
# setwd("~/Copy/Berkeley/stat243/ps6")
library(RSQLite)
# library(RSQLite.extfuns)
library(ff)
library(ffbase)
library(foreach)
library(doParallel)
library(iterators)
library(doBy)
library(data.table)
#----------------------------------------------------------------------
get_notifications <- TRUE
if (get_notifications) {
  library(RPushbullet)
  options(error = function() { # Be notified when there is an error
    pbPost(type = "note", 
           title = "Error!", 
           body = geterrmessage(),
           recipients = c(1, 2))
  })
}
#----------------------------------------------------------------------
data_dir <- "data"
procdata_dir <- "procdata"
dir.create(data_dir, showWarnings = FALSE)
dir.create(procdata_dir, showWarnings = FALSE)
years <- 1987:2008
#======================================================================
# Question 1
# Download all bz2 files
for (year in years) {
  data_filepath <- paste0(year,".csv.bz2")
  if (!file.exists(file.path(data_dir, data_filepath)))
    download.file(paste0("http://www.stat.berkeley.edu/share/paciorek/", data_filepath),
                  file.path(data_dir, data_filepath))
}
if (!file.exists(file.path(procdata_dir, "flights.db"))) {
  db <- dbConnect(SQLite(), dbname = file.path(procdata_dir, "flights.db"))
  col_classes <- c("integer", rep("factor", 3), rep("integer", 4), 
                   "factor", "integer", "factor", rep("integer", 5), rep("factor", 2),
                   rep("integer", 4), "factor", rep("integer", 6))
  for (year in years) {
    is_first_file <- year == years[1]
    data_filepath <- paste0(year,".csv.bz2")
    con <- bzfile(file.path(data_dir, data_filepath), "rt")
    data <- read.csv(con, header = TRUE, colClasses = col_classes)
    data$DepDelay[is.na(data$DepDelay)] <- -9999
    dbWriteTable(conn = db, name = "flights", value = data,
                 row.names = FALSE, append = !is_first_file)
  }
  dbDisconnect(db)
  closeAllConnections()
}

if (get_notifications) {
  pbPost(type = "note", 
         title = paste0("ps6.R"), 
         body = paste0("Question 1 for SQLite done!"),
         recipients = c(1, 2))
}

#======================================================================
# Question 2
# (a) and (b) with SQLite
db <- dbConnect(SQLite(), dbname = file.path(procdata_dir, "flights.db"))

# Subset flights data
if (!("flights_subset" %in% dbListTables(db))) {
  viewquery <- "create view flights_subset as select * from flights where DepDelay >= -30 and DepDelay <= 720"
  dbGetQuery(db, viewquery)
}

if (FALSE) {
# Extract flights departing from SFO or OAK
system.time({
  query1 <- "select * from flights_subset where Origin = 'SFO' or Origin = 'OAK'"
  flights_bayarea <- dbGetQuery(db, query1)
})

# Find mean/median departure delay by airport
system.time({
  query2 <- "select Origin, avg(DepDelay) as meanDepDelay from flights_subset group by Origin"
  depdelay_mean <- dbGetQuery(db, query2)
})

# Notes: Requires RSQLite.extfuns which is not available for R 3.0.2 on AWS
# init_extensions(db)
# system.time({
#   query3 <- "select Origin, median(DepDelay) as medianDepDelay from flights_subset group by Origin"
#   depdelay_median <- dbGetQuery(db, query3)
# })

if (get_notifications) {
  pbPost(type = "note", 
         title = paste0("ps6.R"), 
         body = paste0("Question 2a/b for SQLite done!"),
         recipients = c(1, 2))
}

# Check that flights were correctly extracted and means/medians calculated
unique(flights_bayarea$Origin)
head(depdelay_mean)
# head(depdelay_median)
dbDisconnect(db)
#----------------------------------------------------------------------
# (c)
# Add index to Origin
if (!file.exists(file.path(procdata_dir, "flights-indexed.db")))
  file.copy(file.path(procdata_dir, "flights.db"), file.path(procdata_dir, "flights-indexed.db"))
db_indexed <- dbConnect(SQLite(), dbname = file.path(procdata_dir, "flights-indexed.db"))
indexquery <- "create index OriginID on flights(Origin)"
try(dbGetQuery(db_indexed, indexquery)) # Ad-hoc

if (!("flights_subset" %in% dbListTables(db_indexed))) {
  # Subset flights data
  viewquery <- "create view flights_subset as select * from flights where DepDelay >= -30 and DepDelay <= 720"
  dbGetQuery(db_indexed, viewquery)
}

# Extract flights departing from SFO or OAK
system.time({
  query1 <- "select * from flights_subset where Origin = 'SFO' or Origin = 'OAK'"
  flights_bayarea2 <- dbGetQuery(db_indexed, query1)
})

# Find mean/median departure delay by airport
system.time({
  query2 <- "select Origin, avg(DepDelay) as meanDepDelay from flights_subset group by Origin"
  depdelay_mean2 <- dbGetQuery(db_indexed, query2)
})

# init_extensions(db_indexed)
# system.time({
#   query3 <- "select Origin, median(DepDelay) as medianDepDelay from flights_subset group by Origin"
#   depdelay_median2 <- dbGetQuery(db_indexed, query3)
# })

if (get_notifications) {
  pbPost(type = "note", 
         title = paste0("ps6.R"), 
         body = paste0("Question 2c for SQLite done!"),
         recipients = c(1, 2))
}

# Check that flights were correctly extracted and means/medians calculated
unique(flights_bayarea2$Origin)
head(depdelay_mean2)
# head(depdelay_median)
dbDisconnect(db_indexed)
#----------------------------------------------------------------------
# (d)
# Find mean departure delay by airport (in parallel)
db_indexed <- dbConnect(SQLite(), dbname = file.path(procdata_dir, "flights-indexed.db"))
num_cores <- 4
registerDoParallel(num_cores)
query <- "select Origin from flights_subset"
airports <- unique(dbGetQuery(db_indexed, query)[[1]])
# OR
# query <- "select distinct Origin from flights_subset"
system.time({
  depdelay_mean3 <- foreach(airport = airports, .combine = rbind) %dopar% {
    db_indexed <- dbConnect(SQLite(), dbname = file.path(procdata_dir, "flights-indexed.db"))
    query <- paste0("select Origin, avg(DepDelay) as meanDepDelay ",
                    "from flights_subset where Origin = '", airport ,"'")
    mean_temp <- dbGetQuery(db_indexed, query)
  }
})

if (get_notifications) {
  pbPost(type = "note", 
         title = paste0("ps6.R"), 
         body = paste0("Question 2d for SQLite done!"),
         recipients = c(1, 2))
}

# Check that flights were correctly extracted and means/medians calculated
head(depdelay_mean3)
dbDisconnect(db)
#----------------------------------------------------------------------
# Question 3
# (a) SQLite
db <- dbConnect(SQLite(), dbname = file.path(procdata_dir, "flights.db"))
} # end FALSE

# Extract the 20 observations with the longest departure delays 
# from airports with at least 1 million flights
system.time({
  viewquery <- paste("create view num_departing_flights as select Origin,",
                     "count(Origin) as NumDepartures from flights group by Origin")
  dbGetQuery(db, viewquery)
  joinquery <- paste("create view flights_all as select * from flights_subset",
                     "join num_departing_flights on flights_subset.Origin = num_departing_flights.Origin")
  dbGetQuery(db, joinquery)
  searchquery <- "select * from flights_all where NumDepartures >= 1000000 order by DepDelay desc limit 20 "
  dep_delays_longest <- dbGetQuery(db, searchquery)
})

if (get_notifications) {
  pbPost(type = "note", 
         title = paste0("ps6.R"), 
         body = paste0("Question 3 for SQLite done!"),
         recipients = c(1, 2))
}

# Check that observations were extracted correctly
print(dep_delays_longest)
dbDisconnect(db)
#----------------------------------------------------------------------
#----------------------------------------------------------------------
