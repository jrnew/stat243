context("Selection")
source("../R/evaluate.R")
source("../R/process_data.R")
source("../R/initialize.R")
source("../R/select_for_mating.R")

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

#evaluate test data
eval<-evaluate(pop,model_data = model_data, 
                     model = "lm", criterion = "AIC")

#select chromosomes for recombination
pick<- select_for_mating(
  pop,
  evaluation=eval,
  method = "rank",
  do_parallel = FALSE
)

test_that("Inputs", {
  
  expect_that(pop,is_a("matrix"))
  expect_that(eval,is_a("numeric"))
  expect_that(length(eval),equals(nrow(pop)))
  expect_that("rank",is_a("character"))
  
})


test_that("Output type", {
  
  expect_that(pick,is_a("matrix"))
  
})

test_that("Output elements", {
  
  expect_that(max(pick),equals(1))
  expect_that(min(pick),equals(0))
  
})

test_that("Output size", {
  
  expect_that(dim(pick),equals(dim(pop)))
  
})