#' Calculate the 95% Confidence Interval for the Mean
#'
#' This function takes a numeric vector of sample data and calculates the
#' 95% confidence interval for the population mean based on the sample.
#'
#' @param x A numeric vector containing the sample data.
#'
#' @return A numeric vector of length 2 containing the lower and upper bounds of the
#' 95% confidence interval for the sample mean.
#'
#' @export
#'
#' @examples
#' sample_data <- c(1, 2, 3, 4, 5)
#' myci(sample_data)
myci <- function(x) {
  # Perform a t-test on the sample data to get the confidence interval
  ci <- t.test(x)$conf.int
  return(ci)
}
