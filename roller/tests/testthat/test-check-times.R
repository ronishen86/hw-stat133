context("Test check roll arguments")

test_that("check_times works with ok vectors", {
  
  expect_true(check_times(1))
  expect_true(check_times(33))
})

test_that("check_times fails with invalid numbers", {
  
  expect_error(check_times(.5))
  expect_error(check_times("a"))
})