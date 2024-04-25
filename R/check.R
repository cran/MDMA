#' @title Check model for influential cases
#' @description Perform checks for a linear model regarding influential cases and collinearity
#'     numerically and graphically.
#'
#' `r lifecycle::badge("stable")`
#' @param object object of class \code{lm}.
#' @param ... other parameters (none are used at the moment).
#'
#' @return \code{check} returns a list containing two matrices with statistics regarding
#'     influential cases and a vector of variance inflation factors. Furthermore, it
#'     produces diagnostics plots.\cr
#'     The return list contains three elements:\cr \cr
#'     - \code{influence}, a \code{data.frame}, with observations in the model,
#'     and the following variables:
#'     \item{predicted.value}{The value predicted by the model.}
#'     \item{residual}{The raw residual.}
#'     \item{std.residual}{The standardized residual.}
#'     \item{dfb.<...>}{DFBETAs for the variables in the model.}
#'     \item{dffit}{DFFIT value.}
#'     \item{cov.r}{Covariance ratio, a measure of change in the determinant of
#'     the coefficient covariance matrix.}
#'     \item{cook.d}{Cook's distance.}
#'     \item{hat}{Hat values.}
#'     \item{influential}{Determines whether a case is influential on any of the
#'     measures \code{dfb.<...>}, \code{dffit}, \code{cov.r}, \code{cook.d} or \code{hat}.
#'     See \code{influential cases} for more information.}
#'     \cr\cr
#'     - \code{is.infl} is a \code{data.frame} indicating which influence measure(s)
#'     is/are flagged per observation.
#'     \cr\cr
#'     - \code{vifs}, a vector containing variance inflation factors for the
#'     variables in the model.
#'     \cr\cr
#'     By default, the two \code{data.frame}s regarding influence measures only give the influence
#'     measures for cases that are flagged as being influential. Influence measures for all cases
#'     can be queried using \code{\link{print.check.lm}}.
#'     \cr\cr
#'     The generated plots are the plots produced by \code{\link[stats]{plot.lm}}, numbers 1 through 6.
#' @section influential cases:
#'
#' For the influence indicators, the following rules are applied to check whether a case
#' is influential:
#' \itemize{
#'   \item \eqn{\mathrm{any\enspace}|\mathrm{dfbeta}| > 1}.
#'   \item \eqn{|\mathrm{dffit}| > 3 \sqrt{\frac{k}{n-k}}}.
#'   \item \eqn{|1 - \mathrm{cov.r}| > \frac{3k}{n-k}}.
#'   \item \eqn{F\mathrm{(}n, n-k \mathrm{)} = \mathrm{cooks.d\enspace having\enspace}.
#'   p > .5}
#'   \item \eqn{\mathrm{hat} > \frac{3k}{n}}.
#' }
#' These indicators for being an influential case were derived from
#' \code{\link[stats]{influence.measures}} in the \code{stats} package.
#'
#' @export
#'
#' @examples
#' lm.1 <- lm(mpg ~ disp + wt, data = mtcars)
#' check(lm.1)
#' @importFrom car vif
#' @importFrom stats hatvalues influence.measures predict rstandard
#' @author Mathijs Deen
check <- function(object, ...){
  UseMethod("check")
}

#' @export
check.lm <- function(object, ...){
  pred <- predict(object)
  res <- object$residuals
  lev <- hatvalues(object)
  std.res <- rstandard(object)
  influenceM <- influence.measures(object)
  influential_case <- apply(influenceM$is.inf, 1L, any, na.rm = TRUE)
  influence <- data.frame(predicted.value = pred,
                          residual = res,
                          std.residual = std.res,
                          influenceM$infmat,
                          influential = ifelse(influential_case, 1,0))
  terms <- labels(terms(object))
  n.terms <- length(terms)
  ifelse(n.terms > 1, vifs <- vif(object), vifs <- NA)
  out <- list(influence = influence, is.inf  = data.frame(influenceM$is.inf), vifs = vifs)
  class(out) <- "check.lm"
  oldpar <- par(mfrow = c(3,2))
  on.exit(par(oldpar), add=TRUE)
  plot(object, which = 1)
  plot(object, which = 2)
  plot(object, which = 3)
  plot(object, which = 4)
  plot(object, which = 5)
  plot(object, which = 6)
  return(out)
}

#' @title Print lm check
#' @description Print the check of lm object
#'
#' `r lifecycle::badge("stable")`
#' @param x an object used to select a method.
#' @param which.infl Indicate whether only influential cases (\code{influential}, the default)
#'     or all cases (\code{all}) should be printed.
#' @param ... further arguments passed to or from other methods (none are used).
#' @return prints the \code{check.lm} object.
#' @export
#' @examples
#' lm.1 <- lm(mpg ~ disp + wt, data = mtcars)
#' chk.lm.1 <- check(lm.1)
#' print(chk.lm.1, which.infl="all")
#' @author Mathijs Deen
print.check.lm <- function(x, which.infl = c("influential", "all"), ...){
  which.infl <- match.arg(arg = which.infl, several.ok = FALSE)
  if(which.infl == "influential"){
    cat(sprintf("Influential cases: \n"))
    print(round(x$influence[x$influence$influential == 1, ], 3))
    cat(sprintf("\nIndication per influence measure\n"))
    print(x$is.inf[x$influence$influential == 1,])

  } else{
    cat(sprintf("Case fit values and influence statistics: \n"))
    print(round(x$influence, 3))
    cat(sprintf("\nIndication per influence measure\n"))
    print(x$is.inf)
  }
  cat(sprintf("\n"))
  if(!anyNA(x$vifs)){
    cat(sprintf("Variance inflation factors: \n"))
    print(round(x$vifs, 3))
  }else{
    cat(sprintf("No VIFs calculated since model contains fewer than 2 terms"))
  }
}

