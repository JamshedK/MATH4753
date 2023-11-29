#' Determine Number of Tickets to be Sold for a Flight
#'
#' The function `ntickets` calculates the number of tickets to sell for a flight with `N` seats,
#' ensuring that the probability the plane is overbooked does not exceed `gamma`. The calculation
#' is done in two ways:
#' 1) Using the appropriate discrete distribution.
#' 2) Using the normal approximation.
#'
#' @param N Number of seats available in the flight.
#' @param gamma Maximum allowable probability that the plane is overbooked.
#' @param p Probability that a ticket holder shows up for the flight.
#'
#' @return A named list containing:
#' \itemize{
#'   \item \code{nd}: Number of tickets to be sold as calculated using the discrete distribution.
#'   \item \code{nc}: Number of tickets to be sold as calculated using the normal approximation.
#'   \item \code{N}: Number of seats available in the flight.
#'   \item \code{gamma}: Maximum allowable probability that the plane is overbooked.
#'   \item \code{p}: Probability that a ticket holder shows up for the flight.
#' }
#'
#' The function also generates two plots of the Objective function versus n. The Objective function
#' is derived from the defining equation set to zero (e.g., 1-gamma-pnorm(...) = 0). One plot is for
#' the discrete case and the other for the continuous (normal approximation) case.
#'
#' @examples
#' data <- ntickets(N=200, gamma=0.02, p=0.95)
#' print(data)
#'
#' @importFrom stats pbinom pnorm
#' @importFrom graphics abline plot
#' @export

ntickets <- function(N, gamma, p) {
  library(ggplot2)
  n <- seq(N, N+50, by = 1)

  # Calculating n using binomial distribution
  f <- 1 - gamma - pbinom(N, n, p)
  v <- abs(f)
  i <- which.min(v)
  nd <- n[i]

  # Calculating n using Normal distribution
  ncseq <- seq(N, N+50, length=1000)
  mu = ncseq*p
  sd = sqrt(ncseq*p*(1-p))
  fc <- 1-gamma-pnorm(N+0.5, mu, sd)
  v2 <- abs(fc)
  i2 <- which.min(v2)
  nc <- ncseq[i2]


  # Plotting for binomial (f)
  plot(n, f, type="b", col="blue", lwd=0.5, pch=19, ylim=c(min(f), 1),
       xlab="n", ylab="Objective", main=sprintf("Objective Vs n using Binomial\n(%d) gamma=%.2f N=%d", nd, gamma, N))
  abline(h=0, col="red", lwd=2)
  abline(v=nd, col="red", lwd=2, lty=2)

  # Plotting for normal approximation (fc)
  plot(ncseq, fc, type="l", col="black", lwd=2, pch=19, ylim=c(min(fc), 1),
       xlab="n", ylab="Objective", main=sprintf("Objective Vs n using Normal Approximation\n(%f) gamma=%.2f N=%d", nc, gamma, N))
  abline(h=0, col="red", lwd=2)
  abline(v=nc, col="red", lwd=2, lty=2)


  return(list(nd = nd, nc = nc, N = N, gamma = gamma, p = p))
}
