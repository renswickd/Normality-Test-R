normality <- function(data, plot=FALSE) {

  # Exceptions: detect NULL / Inf / NA values
  if (all(is.null(data))) stop("Input need to be a non NULL value")
  if (all(is.infinite(data))) stop("Input need to be a non Inf value")
  if (all(is.na(data))) stop("Input need to be a non NA value")


  #Exceptions: Detect invalid data formats / Invalid Dimensions / Data catch
  stopifnot(is.numeric(data))
  if (length(dim(data)) > 1) stop("Input must be an vector/1-D array")
  if (length(unique(data)) == 1) stop("Input must not be identical")

  n <- length(data)
  if (n <=2 ) stop("Input must have atleast three or more non NA values")

  # Warnings: NA values
  if (any(is.na(data))) {
    warning("DATA has NA values: W-test statistics value is derived ignoring NAs")
    data <- data[complete.cases(data)]
    n <- length(data)
  }

  # Exceptions: Detect invalid value for argument-plot
  if (!(plot %in% c(TRUE, FALSE))) stop("Paramete plot must have Boolean values")

  if (plot == TRUE) {
    qqnorm(data, pch = 1, frame = FALSE)
    qqline(data, col = "blue", lwd = 1)
  }

  # result <- shapiro.test(data)

  sorted_data <- sort(data)
  a_numerator = (1:n - 3/8)
  a_denominator = (n + 1/4)
  a_value <- qnorm(a_numerator / a_denominator)
  W <- sum(a_value * sorted_data)^2 / sum((sorted_data - mean(data))^2)
  return(W)

}


