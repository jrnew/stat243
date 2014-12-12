#########################
#Test with more data sets and check that ga function is working as expected
##tested using berkhousing data, 4 colomns, choose price as y variables
data <- berkhousing
yvar <- "price"
xvars <- c("tax","sqft","built")
pop_size <- min(nrow(data)*2, 200)
ga(berkhousing,"price",xvars=c("tax","sqft","built"),num_max_iterations = 10L)
ga(data,yvar = yvar, xvars = xvars, num_max_iterations = 10L, method_select = "RWS")
ga(data,yvar = yvar, xvars = xvars, num_max_iterations = 10L, method_select = "SUS")
ga(data,yvar = yvar, xvars = xvars, num_max_iterations = 10L, method_select = "DS")
mod <- lm(price~tax+sqft, data=data)
plot(x=fitted(mod), y=residuals(mod))
qqnorm(residuals(mod))
qqline(residuals(mod))
#the result looks good.

##tested using cricket data, with only two coloumns
data <- read.delim("~/cricket.txt")
yvar <- "Temp"
xvars <- "Chirps.Second"
pop_size <- min(nrow(data)*2, 200)
ga(data,yvar = yvar, xvars = xvars ,num_max_iterations = 10L)

#This is a stupid test, but there is a an error message, keep testing using 
#data with more than two columns.

##tested using video data,with 15 columns. "99" is used instead of NA
data <- read.table("~/video.txt", header=TRUE, quote="\"")
yvar <- "grade"
xvars <- names(data)[1:14]
xvars <- c("freq", "educ", "sex", "age", "work")
pop_size <- min(nrow(data)*2, 200)
ga(data,yvar = yvar, xvars = xvars ,num_max_iterations = 10L)
mod <- lm(grade~time+freq+sex+home+math+email, data=data)
plot(x=fitted(mod), y=residuals(mod))
qqnorm(residuals(mod))
qqline(residuals(mod))
#plot looks better when the AIC is lower


##tested using birth weight data, with 7 colomns and 1236 rows
data <- read.table("~/birth.txt", header=TRUE, quote="\"")
yvar <- "bwt"
xvars <- names(data)[2:7]
pop_size <- min(nrow(data)*2, 200)
ga(data,yvar = yvar, xvars = xvars ,num_max_iterations = 10L)
ga(data,yvar = yvar, xvars = xvars, num_max_iterations = 10L, method_select = "RWS")
ga(data,yvar = yvar, xvars = xvars, num_max_iterations = 10L, method_select = "SRS")
ga(data,yvar = yvar, xvars = xvars, num_max_iterations = 10L, method_select = "SUS")
mod <- lm(bwt~parity+height+weight+smoke,data=data)
plot(x=fitted(mod), y=residuals(mod))
qqnorm(residuals(mod))
qqline(residuals(mod))
#plot looks good.

#Testing for the summary function
data <- as.data.frame(trancSFO)
yvar <- "ArrDelay"
pop_size <- min(nrow(data)*2, 200)
xvars <- c( "Month", "DayofMonth", "DayofWeek", "DepTime", 
            "CRSDepTime", "ArrTime", "CRSArrTime", 
            "FlightNum", "TailNum", "ActualElapsedTime", "CRSElaspsedTime", 
            "AirTime", "DepDelay", "Origin", "Distane","TaxiIn",
            "TaxiOut", "NASDelay")
ga <- select_model(data,yvar = yvar, xvars = xvars ,num_max_iterations = 10L, seed = 100)
summary_ga(ga)
summary_ga(ga,num_view = 12)

data <- berkhousing
yvar <- "price"
xvars <- c("tax","sqft","built")
pop_size <- min(nrow(data)*2, 200)
ga <- select_model(data,yvar = yvar, xvars = xvars, num_max_iterations = 10L)
summary_ga(ga)

