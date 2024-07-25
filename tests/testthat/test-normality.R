# library(testthat)
# library(usethis)
# library(devtools)
# library(here)
# #
# use_testthat()
# # load_all()

test_that("normality function works", {
  expect_no_error(normality(rnorm(100)))
  expect_no_error(normality(rpois(100,3)))

  expect_no_error(normality(rpois(100,3), plot = TRUE))
  expect_no_error(normality(rnorm(200), plot = TRUE))

  expect_no_error(normality(rnorm(100), plot = FALSE))
  expect_no_error(normality(rpois(100,3), plot = FALSE))

  expect_error(normality(NULL),"Input need to be a non NULL value")
  expect_error(normality(NA), "Input need to be a non NA value")
  expect_error(normality(Inf), "Input need to be a non Inf value")

  expect_error(normality(array(c(1,2,3,4),dim = c(2,2))), "Input must be an 1-D vector/array")
  expect_error(normality(c(7,7,7,7,7)), "Input must not be identical")
  expect_error(normality(c("incorrect input format")))
  expect_error(normality(c(1,99)),"Input must have atleast three or more non NA values")

  expect_warning(normality(c(1,2,3,4,NA)), "DATA has NA values: W-test statistics value is derived ignoring NAs")

  expect_error(normality(rnorm(100),plot=c("random")), "Paramete plot must have Boolean values")
})
