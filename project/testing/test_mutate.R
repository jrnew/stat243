context("Mutation")
source("../R/mutate.R")

#Initialize test population
pop_size <- 100
num_vars <- 5
pop<-initialize(pop_size,
                num_vars)

popmut<-mutate(pop)
pop1<-mutate(pop,prob_mutate=1)
pop0<-mutate(pop,prob_mutate=0)

test_that("Inputs", {
  
  expect_that(pop_size > 0,is_true())
  expect_that(num_vars >= 0,is_true())
  
})

test_that("Output format", {
  
  expect_that(popmut,is_a("matrix"))
  expect_that(dim(popmut),equals(dim(pop)))
  
})

test_that("All mutate", {
  
  expect_that(as.logical(1-pop1),equals(as.logical(pop)))
  
})

test_that("No mutate", {
  
  expect_that(pop0,equals(pop))
  
})

test_that("Mutate 0.01", {
  
  expect_that(identical(popmut,pop),is_false())
  expect_that(sum(abs(popmut-pop)) >= 0 ,is_true())
  
})

