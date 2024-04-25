#' @title Save something to an object
#' @description \code{keep} saves an object to a new object. This is useful
#'     if one wants to save an intermediate result when using pipes.
#' @param object the object that is to be saved into \code{name}.
#' @param name the name of the new object, containing the value of \code{object}.
#' @param pos where to do the assignment. See \code{?assign} for more details.
#' @param envir the environment to use. See \code{?assign} for more details.
#' @param inherits should the enclosing framss of the environment be inspected?
#'     See \code{?assign} for more details.
#'
#' @return Upon saving \code{object} to \code{name}, the value of \code{object} is
#'     returned. This makes it suitable for pipes.
#' @export
#'
#' @examples
#' mtcars |>
#'   lm(mpg ~  disp + hp, data = _) |>
#'   keep(lm.mpg_disp_hp) |>
#'   summary()
#' @author Mathijs Deen
keep <- function(object,
                 name,
                 pos = 1,
                 envir = as.environment(pos),
                 inherits = FALSE){
  assign(x        = as.character(substitute(name)),
         value    = object,
         pos      = pos,
         envir    = envir,
         inherits = inherits)
}
