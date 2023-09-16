
#' This function calculates the height of the tree given BHDiameter
#'
#' @param x a quantitative vector with BHDiameter
#'
#' @return  a quantitative vector of height
#' @export
#' @examples
#' myplot(1:10)
#'
myplot=function(x){
  0.86089580 +1.46959217*x  -0.02745726*x^2
}
