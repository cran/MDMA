#' @title Reliable change index
#' @description \code{rci} computes the reliable change index according to Jacobson and Truax (1992).
#'
#' `r lifecycle::badge("stable")`
#' @param x1 prescore.
#' @param x2 postscore, same length as \code{x1}.
#' @param rxx internal consistency statistic.
#' @return \code{rci} returns a vector of \code{length(x1)} with reliable change index scores.
#' @references
#' \itemize{
#' \item{Jacobson, N.S., & Truax, P. (1992). Clinical significance: a statistical approach to defining meaningful change in psychotherapy research. *Journal of Consulting and Clinical Psychology, 59*, 12-19.}}
#' @examples
#' library(MASS)
#' set.seed(1)
#' q <- mvrnorm(n=120, mu=c(40, 50), Sigma = matrix(c(56.25,45,45,56.25), ncol = 2), empirical = TRUE)
#' cbind(q, rci(q[,1], q[,2], .8), rci(q[,1], q[,2], .8) > 1.96)
#' @author Mathijs Deen
#' @export
rci <- function(x1, x2, rxx){
  return((x2 - x1) / sqrt(2 * sd(x1, na.rm = TRUE)^2 * (1 - rxx)))
}
