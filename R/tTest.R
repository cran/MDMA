#' @title t Test
#' @description perform t tests with the possibility of inputting group statistics.
#'
#' `r lifecycle::badge("stable")`
#' @param x a numeric vector. Can be of length 1 for a group mean.
#' @param y a numeric vector. Should be \code{NULL} for a one-sample t-test.
#' @param sdx standard deviation for \code{x}, when this reflects a group mean.
#' @param sdy standard deviation for \code{y}, when this reflects a group mean.
#' @param nx sample size for \code{x}, when this reflects a group mean.
#' @param ny sample size for \code{y}, when this reflects a group mean.
#' @param alternative a character string specifying the alternative hypothesis,
#'     must be one of "\code{two.sided}" (default), "\code{greater}" or "\code{less}".
#'     You can specify just the initial letter.
#' @param mu a number indicating the true value of the mean (or difference in means)
#'     if you are performing an independent samples t-test).
#' @param paired a logical indicating whether you want a paired t-test.
#' @param rxy correlation between two paired samples.
#' @param var.equal a logical variable indicating whether to treat the two variances as being equal. If
#'     \code{TRUE} then the pooled variance is used to estimate the variance otherwise the Welch (or Satterthwaite) approximation to the degrees of freedom is used.
#' @param conf.level level of the confidence interval.
#' @return \code{tTest} performs a t-test (independent samples, paired samples, one sample) just like base-R t.test, but with the extended possibility to enter group statistics instead of raw data.
#' @importFrom stats pt qt complete.cases setNames var
#' @examples
#' library(MASS)
#' set.seed(1)
#' ds <- mvrnorm(n=50, mu = c(50,55),
#'               Sigma = matrix(c(100,0,0,81),
#'                              ncol = 2),
#'               empirical = TRUE) |>
#'   data.frame() |>
#'   setNames(c("x1","x2"))
#' t.test(ds$x1, ds$x2)
#' tTest(x   = ds$x1,
#'       y   = 55,
#'       sdy = 9,
#'       ny  = 50)
#' @author Mathijs Deen
#' @export
tTest <- function(x,
                  y           = NULL,
                  sdx         = NULL,
                  sdy         = NULL,
                  nx          = length(na.omit(x)),
                  ny          = length(na.omit(y)),
                  alternative = c("two.sided", "greater", "less"),
                  mu          = 0,
                  paired      = FALSE,
                  rxy         = NULL,
                  var.equal   = FALSE,
                  conf.level  = 0.95){

  m <- function(a) mean(a, na.rm=TRUE)
  alternative <- match.arg(arg = alternative, several.ok = FALSE)

  if(length(x) == 1 & is.null(sdx)) stop("x of length 1 is only accepted with specified sdx", call. = FALSE)
  if(length(x) == 1 & nx == 1) stop("x of length 1 is only accepted with specified nx", call. = FALSE)
  if(length(y) == 1 & is.null(sdy)) stop("y of length 1 is only accepted with specified sdy", call. = FALSE)
  if(length(y) == 1 & ny == 1) stop("y of length 1 is only accepted with specified ny", call. = FALSE)
  if(is.null(sdx)) sdx <- sd(x, na.rm = TRUE)
  if(is.null(sdy)) sdy <- sd(y, na.rm = TRUE)
  if(!missing(mu) && (length(mu) != 1 || is.na(mu))) stop("'mu' must be a single number")
  if(!missing(conf.level) && (length(conf.level) != 1 || !is.finite(conf.level) || conf.level < 0 || conf.level > 1)) stop("'conf.level' must be a single number between 0 and 1")

  vx <- sdx^2

  xObserved <- ifelse(length(x) == 1, FALSE, TRUE)
  yObserved <- ifelse(length(y) == 1, FALSE, TRUE)
  if(is.null(y)) yObserved <- FALSE

  if(!is.null(y)){
    type <- ifelse(paired, "paired", "independent")
  }else{
    type <- "one-sample"
  }

  if(type %in% c("paired", "independent")){
    dname <- paste(deparse1(substitute(x)),"and",
                   deparse1(substitute(y)))
    if(paired){
      method <- "Paired t-test"
      if(xObserved & yObserved){
        xOK <- yOK <- complete.cases(x,y)
        x <- x[xOK]
        y <- y[yOK]
        x <- x - y
        mx <- m(x)
        vx <- var(x)
        nx <- length(x)
        df <- nx - 1
        stdError <- sqrt(vx / nx)
        tVal <- (mx - mu) / stdError
      }else{
        if(is.null(rxy)) stop("argument rxy should be provided when at least one of the groups is not observed", call. = FALSE)
        if(!xObserved & !yObserved & nx != ny) stop("paired observations should be of equal length", call. = FALSE)
        x <- na.omit(x)
        y <- na.omit(y)
        mx <- m(x) - m(y)
        ifelse(!xObserved & !yObserved, nx <- nx, nx <- max(length(x), length(y)))
        df <- nx -1
        s <- sqrt(sdx^2/nx + sdy^2/ny)
        stdError <- sqrt((sdx^2/nx + sdy^2/ny) * (1 - rxy))
        tVal <- (mx - mu) / stdError
      }
      estimate <- setNames(mx, "mean difference")
    }else{
      method <- paste(if(!var.equal) "Welch", "Two Sample t-test")
      mx  <- m(x)
      my  <- m(y)
      vy  <- sdy^2
      estimate <- c(mx,my)
      names(estimate) <- c("mean of x","mean of y")
      if(var.equal){
        df <- nx + ny - 2
        v <- (nx - 1) * vx + (ny - 1) * vy
        vPooled <- v / df
        stdError <- sqrt(vPooled * (1 / nx + 1 / ny))
      }
      else{
        stdErrorx <- sqrt(vx / nx)
        stdErrory <- sqrt(vy / ny)
        stdError <- sqrt(stdErrorx^2 + stdErrory^2)
        df <- stdError^4 / (stdErrorx^4/(nx - 1) + stdErrory^4/(ny - 1))
      }
      tVal <- (mx - my - mu) / stdError
    }
  }else{
    method <- "One Sample t-test"
    dname <- deparse1(substitute(x))
    if (paired) stop("'y' is missing for paired test")
    x   <- na.omit(x)
    df  <- nx - 1
    mx  <- m(x)
    vx  <- sdx^2
    stdError <- sqrt(vx / nx)
    tVal <- (mx - mu) / stdError
    estimate <- setNames(mx, "mean of x")
  }
  if(alternative == "less"){
    pVal <- pt(tVal, df, lower.tail = TRUE)
    cint <- c(-Inf, tVal + qt(conf.level, df))
  }
  else if(alternative == "greater"){
    pVal <- pt(tVal, df, lower.tail = FALSE)
    cint <- c(tVal - qt(conf.level, df), Inf)
  }
  else{
    pVal <- 2 * pt(-abs(tVal), df)
    alpha <- 1 - conf.level
    cint <- qt(1 - alpha/2, df)
    cint <- tVal + c(-cint, cint)
  }
  cint <- mu + cint * stdError
  names(tVal) <- "t"
  names(df) <- "df"
  names(mu) <- if(paired) "mean difference"
  else if(!is.null(y)) "difference in means"
  else "mean"
  attr(cint,"conf.level") <- conf.level
  rval <- list(statistic = tVal, parameter = df, p.value = pVal,
               conf.int = cint, estimate = estimate, null.value = mu,
               stderr = stderr,
               alternative = alternative,
               method = method, data.name = dname)
  class(rval) <- c("tTest", "htest")
  return(rval)
}

#' @title Summarize outcome of a t test
#' @description Summarize the outcome of a t test
#'
#' `r lifecycle::badge("stable")`
#' @param object object of class \code{htest} (i.e., the result of \code{mdma::tTest} or \code{stats::t.test}).
#' @param rnd number of decimal places. Should have length 1 or 3. One value specifies the rounding
#'     value for the degrees of freedom, t statistic and p value all at once, while specifying three values
#'     gives the rounding values for the three statistics respectively.
#' @param ... other arguments of the summary generic (none are used).
#' @return \code{summary.htest} returns a typical APA-like output (without italics) for a t-test.
#' @export
#' @examples
#' x1 <- QIDS$QIDS[QIDS$depression == "Yes"]
#' x2 <- QIDS$QIDS[QIDS$depression == "No"]
#' tt <- tTest(x1, x2)
#' summary(tt, rnd = c(1,2,3))
#' @author Mathijs Deen
summary.tTest <- function(object, rnd = 3L, ...){
  if(length(rnd) == 1){
    dfRnd <- tRnd <- pRnd <- rnd
  }else if(length(rnd) == 3){
    dfRnd <- rnd[1]
    tRnd  <- rnd[2]
    pRnd  <- rnd[3]
  }else{
    stop("argument rnd should have lenght of 1 or 3", call. = FALSE)
  }
  if(any(rnd > 9)) stop("Maximum number of decimals is 9", call. = FALSE)
  x <- object
  pOrig <- x$p.value
  p <- format(round(pOrig, pRnd), nsmall = pRnd)
  pUnit <- strsplit(p, "\\.")[[1]][1]
  if(pUnit == "1") {
    pFinal <- substr("p > .999999999", 1, pRnd + 5)
  }else{
    pDecs <- strsplit(p, "\\.")[[1]][2]
    if(as.numeric(pDecs) == 0){
      pFinal <- paste0("p < .", paste(rep("0", pRnd-1), collapse=""), "1")
    }else{
      pFinal <- paste0("p = .", pDecs)
    }
  }
  outf <- paste0("t(%.", dfRnd, "f) = %.", tRnd, "f, %s")
  cat(sprintf(outf, x$parameter, x$statistic, pFinal))
}

#' @title Print t test
#' @description Print the output of a t test.
#'
#' `r lifecycle::badge("stable")`
#' @param x an object used to select a method.
#' @param ... further arguments passed to or from other methods.
#' @return prints the \code{tTest} object as a \code{htest} object.
#' @export
#' @examples
#' x1 <- QIDS$QIDS[QIDS$depression == "Yes"]
#' x2 <- QIDS$QIDS[QIDS$depression == "No"]
#' tt <- tTest(x1, x2)
#' print(tt)
#' @author Mathijs Deen
print.tTest <-function(x, ...){
  NextMethod()
}
