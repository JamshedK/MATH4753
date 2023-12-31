% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/myci.R
\name{myci}
\alias{myci}
\title{Calculate the 95% Confidence Interval for the Mean}
\usage{
myci(x)
}
\arguments{
\item{x}{A numeric vector containing the sample data.}
}
\value{
A numeric vector of length 2 containing the lower and upper bounds of the
95% confidence interval for the sample mean.
}
\description{
This function takes a numeric vector of sample data and calculates the
95% confidence interval for the population mean based on the sample.
}
\examples{
sample_data <- c(1, 2, 3, 4, 5)
myci(sample_data)
}
