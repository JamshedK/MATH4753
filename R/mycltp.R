#' Histogram of Sample Mean from Binomial Distribution
#'
#' This function simulates samples from a binomial distribution, computes their means,
#' and plots a histogram of these means with a theoretical normal curve overlay.
#'
#' @param n Numeric. Sample size for each iteration.
#' @param iter Numeric. Number of iterations or samples to be taken.
#' @param p Numeric. Probability of success for the binomial distribution. Default is 0.5.
#' @param ... Additional parameters to be passed to the `hist` function.
#'
#' @return Invisible. A histogram is plotted as a side effect.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' mycltb(n=50, iter=1000)
#' }

mycltb <- function(n, iter, p=0.5, ...) {

  ## r-random sample from the Binomial
  y <- rbinom(n * iter, size=n, prob=p)

  ## Place these numbers into a matrix
  ## The columns will correspond to the iteration
  ## and the rows will equal the sample size n
  data <- matrix(y, nr=n, nc=iter, byrow=TRUE)

  ## Apply the function mean to the columns (2) of the matrix
  ## These results are placed in a vector w
  w <- apply(data, 2, mean)

  ## Prepare to make a histogram of the values in w
  ## Store histogram parameters without plotting yet
  param <- hist(w, plot=FALSE)

  ## Since the histogram will be a density plot, find the max density
  ymax <- max(param$density)

  ## Add 10% more for a margin of safety
  ymax <- 1.1 * ymax

  ## Now we can make the histogram with specified frequency and y-axis limits
  hist(w, freq=FALSE, ylim=c(0, ymax),
       main=paste("Histogram of sample mean", "\n", "sample size= ", n, sep=""),
       xlab="Sample mean", ...)

  ## Add a theoretical normal curve based on binomial distribution parameters
  curve(dnorm(x, mean=n * p, sd=sqrt(p * (1-p))), add=TRUE, col="Red", lty=2, lwd=3)
}
