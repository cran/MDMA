#' @title Area under the curve
#' @description Calculate the area under the curve.
#'
#' `r lifecycle::badge("stable")`
#' @param x object of class \code{roc}.
#' @param ... other arguments (none are used at the moment).
#'
#' @return returns the area under the curve for a \code{roc} class object.
#' @examples
#' a <- roc(QIDS$QIDS, QIDS$depression, c("Yes","No"), "Yes")
#' auc(a)
#' @author Mathijs Deen
#' @export
auc <- function(x, ...){
  UseMethod("auc")
}

#' @export
auc.roc <- function(x, ...){
  return(x$auc)
}
