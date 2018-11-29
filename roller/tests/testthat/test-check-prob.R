context("Test check prob arguments")

test_that("check_prob works with ok vectors", {
  
  expect_true(check_prob(c(0.075, 0.1, 0.125, 0.15, 0.20, 0.35)))
  expect_true(check_prob(rep(1/4, 4)))
})

test_that("check_prob fails with invalid lengths", {
  
  expect_error(check_prob(1))
})

test_that("check_prob fails with invalid numbers", {
  
  expect_error(check_prob(0.333, 0.666))
  expect_error(check_prob(-0.5, 0.5))
  expect_error(check_prob(0.5, -0.5))
  expect_error(check_prob(0.5, NA))
})



