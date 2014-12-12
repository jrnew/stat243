context("Initialization")
source("../R/initialize.R")

#Initialize population
pop_size <- 100
num_vars <- 5
pop<-initialize(pop_size,
                      num_vars)

test_that("Inputs", {
  
  expect_that(pop_size > 0,is_true())
  expect_that(num_vars >= 0,is_true())
  
})

test_that("Output type", {
  
  expect_that(pop,is_a("matrix"))
  
})

test_that("Output size", {
  
  expect_that(nrow(pop),equals(pop_size))
  expect_that(ncol(pop),equals(num_vars))
  
})

test_that("Elements", {
  
  expect_that(max(pop),equals(1))
  expect_that(min(pop),equals(0))
  
})