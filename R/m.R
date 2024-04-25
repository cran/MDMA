#' @title Mean center
#' @description Mean center a vector or numeric matrix.
#'
#' `r lifecycle::badge("stable")`
#' @param x a numeric matrix or vector.
#' @return \code{m} returns a mean centered version of \code{x}. If \code{x} is
#'     a matrix, the matrix dimensions are preserved.
#' @details This function resembles \code{base::scale.default}, with the \code{scale}
#'     argument set to \code{FALSE}. This, together with the short function name, is
#'     especially useful when you want to mean center variables in an analysis (e.g.,
#'     using \code{(g)lm}), but you dont want the long form \code{scale(x, scale=FALSE)}
#'     to clutter up the rownames of the parameter estimates or the model anova.
#' @examples
#' vals <- matrix(rnorm(24, 15, 10), ncol = 2)
#' m(vals)
#' @author Mathijs Deen
#' @export

m <- function(x){
  x <- as.matrix(x)
  center <- colMeans(x     = x,
                     na.rm = TRUE)
  x <- sweep(x            = x,
             MARGIN       = 2L,
             STATS        = center,
             check.margin = FALSE)
  return(x)
}
