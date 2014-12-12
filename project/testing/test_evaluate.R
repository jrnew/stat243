context("Evaluate")
source("../R/evaluate.R")
source("../R/process_data.R")
source("../R/initialize.R")

#Initialize population
pop_size <- 100
num_vars <- 5
pop<-initialize(pop_size,
                num_vars)

#Initialize test data
n <- 100
x1 <- runif(n, 2, 3)
x2 <- runif(n, -5, -4)
x4 <- runif(n, 0.8, 1)
data <- data.frame(x1 = x1, x2 = x2, x3 = rnorm(n), x4 = x4, x5 = rnorm(n), 
                   y = x1 + 0.5*x2 + 1.7*x4)
yvar <- "y"
model_data <- process_data(data = data, yvar = yvar)

#Single evaulation for each model/criterion combination    
lmaic<-evaluate_once(model_data = model_data, 
                      xvars_select = as.logical(c(1, 0, 1, 0, 1)),
                      model = "lm", criterion = "AIC")

glmaic<-evaluate_once(model_data = model_data, 
                     xvars_select = as.logical(c(1, 0, 1, 0, 1)),
                     model = "glm", glm_family="gaussian",criterion = "AIC")

lmbic<-evaluate_once(model_data = model_data, 
                     xvars_select = as.logical(c(1, 0, 1, 0, 1)),
                     model = "lm", criterion = "BIC")

glmbic<-evaluate_once(model_data = model_data, 
                     xvars_select = as.logical(c(1, 0, 1, 0, 1)),
                     model = "glm", glm_family="gaussian",criterion = "BIC")

test_that("Output type", {
  
  expect_that(lmaic,is_a("numeric"))
  expect_that(glmaic,is_a("numeric"))
  expect_that(lmbic,is_a("numeric"))
  expect_that(glmbic,is_a("numeric"))
  
})

test_that("Wrong inputs", {
  
  expect_that(length(model_data$xvars) != length(xvars_select),throws_error())
  
})

#Test for evaluation on entire population
eval<-evaluate(pop,model_data = model_data, 
               model = "lm", criterion = "AIC")

test_that("Population Output", {
  
  expect_that(eval,is_a("numeric"))
  expect_that(length(eval),equals(nrow(pop)))
  
})
