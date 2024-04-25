#' @title List of correlation coefficients
#' @description List all correlations in a correlation matrix without duplicates.
#'
#' `r lifecycle::badge("stable")`
#' @param x a numeric vector, matrix or data frame.
#' @param ... arguments passed to the \code{cor} function.
#' @return \code{corList} returns a list of correlations
#' @examples
#' mtcars[,c("mpg","disp", "hp", "drat", "wt", "qsec")] |>
#'   corList(method="spearman")
#' @author Mathijs Deen
#' @importFrom stats cor
#' @export
corList <- function(x, ...){
  cors <- cor(x, ...)
  up <- upper.tri(cors)
  out <- data.frame(which(up, arr.ind=TRUE), cor=cors[up])
  out$var1 <- rownames(cors)[out$row]
  out$var2 <- rownames(cors)[out$col]
  out <- out[,c("var1","var2","cor")]
  return(out)
}
