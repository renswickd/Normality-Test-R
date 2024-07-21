test_that("normality function works", {
  expect_no_error(normality(rnorm(100)))
  expect_no_error(normality(rpois(100,3)))
  expect_error(normality(c("add")))
})
  