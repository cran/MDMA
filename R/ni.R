#' @title Inverse value matching
#' @description Evaluates whether the left hand side argument is not in the right hand side argument.
#'
#' `r lifecycle::badge("stable")`
#' @param lhs left hand side.
#' @param rhs right hand side.
#' @return The function returns a vector of the same length as \code{lhs}.
#' @details The \code{\%ni\%} function negates the \code{\%in\%} operator.
#' @examples
#' c(1,2,3) %ni% c(1,2)
#' @author Mathijs Deen
#' @export
`%ni%` <- function(lhs, rhs){
  `%notin%` <- Negate(`%in%`)
  return(lhs %notin% rhs)
}
