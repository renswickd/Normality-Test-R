# print("Hello World!")

# can git bash terminal to be used to commit and push the codes to GitHub
# what should be the output? only W statistic value? or along with p-value
# for Shapiro-Wilk test, do we have to use teh inbuilt function? or has it to be manually written?


check_normality <- function(data) {
  result <- shapiro.test(data)
  return(result)
}

sample_data <- c(1,2,3,4,5,6,2,3,4,2,3,4)
check_normality(sample_data)