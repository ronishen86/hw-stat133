# load the source code of the functions to be tested
source("functions.R")

# stat_range: context with one test that groups expectations
context("Test for range value") 

test_that("range works as expected", {
  x <- c(1, 2, 3, 4, 5)
  
  expect_equal(stat_range(x), 4)
  expect_length(stat_range(x), 1)
  expect_type(stat_range(x), 'double')
})

test_that("range works as expected", {
  y <- c(1, 2, 3, 4, NA)
  
  expect_equal(stat_range(y), NA_real_)
  expect_length(stat_range(y), 1)
})

test_that("range works as expected", {
  z <- c(TRUE, FALSE, TRUE)
  
  expect_equal(stat_range(z), 1L)
  expect_length(stat_range(z), 1)
  expect_type(stat_range(z), 'integer')
})

test_that("range works as expected", {
  w <- letters[1:5]
  
  expect_error(stat_range(w))
})

# testing stat_centers
context("Test for center values") 

test_that("measure for center works as expected", {
  x <- c(1, 2, 3, 4, 5)
  
  expect_equal(stat_centers(x), c(3,3))
  expect_length(stat_centers(x), 2)
  expect_type(stat_centers(x), 'double')
})

test_that("measure for center works as expected", {
  y <- c(1, 2, 3, 4, NA)
  
  expect_equal(stat_centers(y), c(NA_real_, NA_real_))
  expect_length(stat_centers(y), 2)
})

test_that("measure for center works as expected", {
  z <- c(TRUE, FALSE, TRUE)
  
  expect_equal(stat_centers(z), c(1,2/3))
  expect_length(stat_centers(z), 2)
  expect_type(stat_centers(z), 'double')
})

test_that("measure for center works as expected", {
  w <- letters[1:5]
  
  expect_warning(stat_centers(w))
})

# testing stat_spreads
context("Test for spread values") 

test_that("measure for spread works as expected", {
  x <- c(1, 2, 3, 4, 5)
  
  expect_equal(stat_spreads(x), c(4,2,sd(x)))
  expect_length(stat_spreads(x), 3)
  expect_type(stat_spreads(x), 'double')
})

test_that("measure for spread works as expected", {
  y <- c(1, 2, 3, 4, NA)
  
  expect_error(stat_spreads(y))
})

test_that("measure for spread works as expected", {
  z <- c(TRUE, FALSE, TRUE)
  
  expect_equal(stat_spreads(z), c(1, .5, sd(c(TRUE, FALSE, TRUE))))
  expect_length(stat_spreads(z), 3)
  expect_type(stat_spreads(z), 'double')
})

test_that("measure for spread works as expected", {
  w <- letters[1:5]
  
  expect_error(stat_spreads(w))
})


