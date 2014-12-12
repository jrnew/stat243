context("Data processing check")
source("../R/process_data.R")

#Initialize test data
n <- 100
x1 <- runif(n, 2, 3)
x2 <- runif(n, -5, -4)
x4 <- runif(n, 0.8, 1)
data <- data.frame(x1 = x1, x2 = x2, x3 = rnorm(n), x4 = x4, x5 = rnorm(n), 
                   y = x1 + 0.5*x2 + 1.7*x4)
yvar <- "y"
model_data <- process_data(data = data, yvar = yvar)


test_that("Inputs", {
  
  expect_that(data,is_a("data.frame"))
  expect_that(yvar,is_a("character"))
  
})

test_that("Number of outputs", {
  
  expect_that(length(model_data),equals(4))
  
     })

test_that("Outputs correct type",{
  
  expect_that(model_data$data,is_a("data.frame"))
  expect_that(model_data$yvar,is_a("character"))
  expect_that(model_data$xvars,is_a("character"))
  expect_that(model_data$num_vars,is_a("integer"))
  
    })

test_that("Num vars is positive",{
  
  expect_that(model_data$num_vars >= 0,is_true())
  
    })