context("Test check side arguments")

test_that("check_sides with ok vectors", {
  
  expect_true(check_sides(c('i', 'ii', 'iii', 'iv')))
  expect_true(check_sides(1:6))
})

test_that("check_sides fails with invalid lengths", {
  
  expect_error(check_sides(c('a')))
  expect_error(check_sides(c(1234)))
})

test_that("check_sides fails with duplications", {
  
  expect_error(check_sides(c('one', 'one')))
  expect_error(check_sides(c(1, 1, 1)))
})

