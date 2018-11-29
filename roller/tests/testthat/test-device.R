context("Test device")

test_that("device works with ok parameters", {
  
  expect_true(device(
    sides = c('i', 'ii', 'iii', 'iv'), 
    prob = rep(1/4, 4)))
  expect_true(device(
    sides = 1:6,
    prob = c(0.075, 0.1, 0.125, 0.15, 0.20, 0.35)))
})

test_that("device fails with invalid parameters", {
  
  expect_error(device(sides = c('a')))
  expect_error(device(sides = c('heads', 'heads')))
  expect_error(device( sides = c('a', 'b'), prob = c(0.2, 0.1)))
  expect_error(device( sides = c('a', 'b', 'c'), prob = c(0.2, 0.8)))
  })