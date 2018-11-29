context("Test device")

test_that("is device works with ok parameters", {

  expect_true(is.device(device(
      sides = c('i', 'ii', 'iii', 'iv'),
      prob = rep(1/4, 4))
  ))

})

test_that("is device fails with invalid parameters", {

  expect_false(is.device(c(1, 2, 3)))
})
