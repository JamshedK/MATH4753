#' Plot and Shade a Normal Distribution Curve
#'
#' @param mu Mean of the normal distribution.
#' @param sigma Standard deviation of the normal distribution.
#' @param a Value up to which area under the curve is shaded.
#'
#' @return A list containing the probability \(P(X \leq a)\).
#' @export
#' @examples
#' \dontrun{
#'   result <- myncurve(0, 1, 0.5)
#'   print(result)
#' }

myfun <- function(x){
  x^2 + 20
}
