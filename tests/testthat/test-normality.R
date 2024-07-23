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

  expect_error(normality(NULL))
  expect_error(normality(NA))
  expect_error(normality(Inf))
  expect_error(normality(c("incorrect input format")))
  expect_error(normality(c(1,99)))
  expect_error(normality(array(c(1,2,3,4),dim = c(2,2))))

  expect_warning(normality(c(1,2,3,4,NA)))

  # expect_error(normality(rnorm(100),plot=NULL))
  expect_error(normality(rnorm(100),plot=c("random")))
})
