#' @title Plot a probability distribution
#' @description Plot the density function of certain probability distributions.
#'
#' `r lifecycle::badge("stable")`
#' @param distribution the probability distribution for which a plot should be drawn. Currently,
#'     the options are \code{"normal", "t", "chi2"} and \code{"F"}.
#' @param xRange Range of x axis over which the distribution should be drawn.
#' @param xColArea Optional, a matrix with two columns, where each row contains lower and upper bounds
#'    for intervals that should be colored under the pdf curve.
#' @param xAreaCol Optional, should contain (a) color(s) for the interval colors in \code{xColArea}.
#'     Defaults to \code{"red"}. Should either be length 1 or length \code{length(xColArea)}.
#' @param mean mean for the normal distribution.
#' @param sd sd for the normal distribution.
#' @param df df for the t distribution.
#' @param df1 first df for the F distribution.
#' @param df2 second df for the F distribution.
#' @param ncp non-centrality parameter
#' @param ... other arguments to be forwarded to the \code{plot} function.
#'
#' @return \code{plotDistribution} returns a probability density plot.
#' @importFrom graphics polygon
#' @importFrom stats dchisq dnorm dt df
#' @export
#'
#' @examples
#' plotDistribution(distribution = "normal",
#'                  xRange       = c(-5, 5),
#'                  xColArea     = matrix(data  = c(-5, -1.96,
#'                                                  1.96, 5),
#'                                        ncol  = 2,
#'                                        byrow = TRUE),
#'                  xAreaCol     = c("green", "blue"),
#'                  mean         = 0,
#'                  sd           = 1,
#'                  yaxt         = "n",
#'                  ylab         = "")
#' @author Mathijs Deen
plotDistribution <- function(distribution = c("normal", "t", "chi2", "F"),
                             xRange = c(0,5),
                             xColArea = NULL,
                             xAreaCol = NULL,
                             mean = 0,
                             sd = 1,
                             df, df1, df2,
                             ncp, ...){
  if(distribution %ni% c("normal", "t", "chi2", "F"))
    stop('distribution should be one of "normal", "t", "chi2", "F"', call.=FALSE)
  distribution <- match.arg(arg = distribution, several.ok = FALSE)
  x <- seq(from = xRange[1], to = xRange[2], by = 0.01)
  if(distribution == "normal") y <- dnorm(x, mean = mean, sd = sd, log = FALSE)
  if(distribution == "t") y <- dt(x = x, df = df, ncp = ncp, log = FALSE)
  if(distribution == "chi2") y <- dchisq(x = x, df = df, ncp = ncp, log = FALSE)
  if(distribution == "F") y <- stats::df(x = x, df1 = df1, df2 = df2, ncp = ncp, log = FALSE) #df function from stats package, not the df argument
  plot(x, y, type="l", ...)
  if(!is.null(xColArea)){
    if(!is.matrix(xColArea)) stop("nColArea is not a matrix", call.=FALSE)
    if(dim(xColArea)[2] != 2) stop("nColArea should have 2 columns", call.=FALSE)
    if(is.null(xAreaCol)) xAreaCol <- "red"
    if(length(xAreaCol) == 1) xAreaCol <- rep(xAreaCol[1], dim(xColArea)[1])
    if(length(xAreaCol) != dim(xColArea)[1])
      stop("xAreaCol should be length 1 or length dim(xColArea)[1]", call. = FALSE)
    for(i in seq_along(1:nrow(xColArea))){
      r <- xColArea[i,]
      polygon(c(x[x %inRange% r], max(r), min(r)),
              c(y[x %inRange% r], 0, 0),
              col = xAreaCol[i])
    }
  }
}
