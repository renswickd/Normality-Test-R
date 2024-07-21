normality <- function(data, plot=FALSE) {
  if (plot == TRUE) { 
    qqnorm(data, pch = 1, frame = FALSE)
    qqline(data, col = "blue", lwd = 1)
  }
  if (any(is.na(data))) {
    warning("DATA has NA values")
  }
  result <- shapiro.test(data)
  return(result)
}

# sample_data <- c(1,2,3,4,5,6,2,3,4,2,3,4,NA)
# normality(sample_data, plot = FALSE)
