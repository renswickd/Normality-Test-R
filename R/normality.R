normality <- function(data, plot=FALSE) {

  # detect null values
  if (all(is.null(data))) stop("Input need to be a non NULL value")
  if (all(is.infinite(data))) stop("Input need to be a non Inf value")
  if (all(is.na(data))) stop("Input need to be a non NA value")

  #check argument-data
  ## all NA, other than numeric data

  stopifnot(is.numeric(data))

  if (length(dim(data)) > 1) stop("Input must be an 1-D vector/array")
  if (length(unique(data)) == 1) stop("Input must not be identical")
  if (length(data) <=2 ) stop("Input must have atleast three or more non NA values")

  ## warning for NA values
  if (any(is.na(data))) {
    warning("DATA has NA values: W-test statistics value is derived ignoring NAs")
  }

  #check argument-plot
  if (!(plot %in% c(TRUE, FALSE))) stop("Paramete plot must have Boolean values")

  if (plot == TRUE) {
    qqnorm(data, pch = 1, frame = FALSE)
    qqline(data, col = "blue", lwd = 1)
  }

  result <- shapiro.test(data)
  return(result)
}

# sample_data <- c(1,2,3,4,5,6,2,3,4,3,4)
# is.numeric(sample_data)
# normality(NA)
# normality(matrix(c(1,2,3)))
#
#
# is.infinite(-Inf)
# length(unique(c(1,1,1)))
#
# a = array(c(1,2,3),dim = c(1,3))
# length(dim(a))
