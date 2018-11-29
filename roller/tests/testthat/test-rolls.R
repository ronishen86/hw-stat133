context("Test rolls")

test_that("rolls works with ok parameters", {
  
  expect_true(roll(device(), times = 50))
  expect_true(roll(device(sides = 1:6, prob = rep(1/6, 6)), times = 50))
  expect_true(roll(device(
    sides = c('a', 'b', 'c', 'd', 'e', 'f'),
    prob = c(0.075, 0.1, 0.125, 0.15, 0.20, 0.35)), times = 20))
})

test_that("rolls fails with invalid parameters", {
  
  expect_error(roll(device(sides = c('a'))))
  expect_error(roll(device(), times = .5))
})