test_that("normality function works", {
  expect_no_error(normality(rnorm(100)))
  expect_no_error(normality(rpois(100,3)))
  expect_error(normality(c("add")))
  expect_warning(normality(c(1,2,3,4,NA)))
})
  