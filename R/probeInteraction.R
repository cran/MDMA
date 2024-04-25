#' @title Probe interaction models
#' @description Probe the effect of a moderator on an X/antecedent variable in a linear model.
#'
#' `r lifecycle::badge("stable")`
#' @param object object of class \code{lm}.
#' @param antecedent antecedent (or x) variable in \code{object}.
#' @param moderator moderator variable in \code{object}.
#' @param alpha desired alpha level for Johnson-Neyman procedure.
#' @param JN indicate whether Johnson-Neyman procedure should be carried out.
#' @param n.interval.moderator number of intervals in the moderator variable to probe.
#' @param quantile.moderator quantile values in the moderator variable to probe.
#' @param values.moderator raw values in the moderator variable to probe.
#' @return \code{probeInteraction} returns a data frame containing values of the moderator
#'     in a linear model, the effect of the antecedent at that value of the moderator,
#'     standard errors, t values, p values and a confidence interval.
#' @details the arguments \code{n.interval.moderator}, \code{quantile.moderator} and \code{values.moderator}
#'     can be combined. All unique values from these methods combined, together with the values from the
#'     Johnson-Neyman procedure (if specified) will be part of the probing procedure.
#' @importFrom stats qt quantile na.omit qnorm coef vcov
#' @importFrom methods hasArg
#' @examples
#' lm.1 <- lm(mpg ~ hp * wt, data = mtcars)
#' probeInteraction(lm.1, hp, wt, JN=TRUE, n.interval.moderator = 3,
#'                  quantile.moderator = c(0.1,0.9), values.moderator = 2)
#' @author Mathijs Deen
#' @export
probeInteraction <- function(object, antecedent, moderator, alpha=.05, JN=TRUE,
                             n.interval.moderator, quantile.moderator, values.moderator){
  arguments <- as.list(match.call())
  Xname     <- as.character(arguments$antecedent)
  Mname     <- as.character(arguments$moderator)
  if(!inherits(object, "lm")) stop("only lm class objects are currently supported", call. = FALSE)

  V         <- vcov(object)
  B         <- coef(object)
  Mobserved <- eval(arguments$moderator, object$model)

  Xpos    <- which(names(B) %in% Xname)
  Mpos    <- which(names(B) %in% Mname)
  XMpos   <- c(Xpos, Mpos)
  XMnames <- c(Xname, Mname)[which.min(XMpos):which.max(XMpos)]
  Iname   <- paste0(XMnames[1],":",XMnames[2])
  Ipos    <- which(names(B) %in% Iname)

  bX     <- B[Xpos]
  bI     <- B[Ipos]
  V.bX   <- V[Xpos, Xpos]
  V.bI   <- V[Ipos, Ipos]
  C.bXbI <- V[Xpos, Ipos]
  res.df <- object$df.residual
  tc     <- qt(p = 1 - alpha / 2, df = res.df)

  Mintervalpoints <- Mquantilepoints <- JNpoints <- Mrawpoints <- NA

  if(JN==TRUE){
    a <- tc^2 * V.bI - bI^2
    b <- 2 * tc^2 * C.bXbI - 2 * bX * bI
    c <- tc^2 * V.bX - bX^2
    D <- b^2 - 4 * a * c
    ifelse(D < 0, JNpoints <- NA, JNpoints <- (-b + c(-1, 1) * sqrt(D)) / (2 * a))
  }

  if(hasArg(n.interval.moderator)){
    Mintervalpoints <- diff(range(Mobserved)) * (0:n.interval.moderator)/n.interval.moderator +
      min(Mobserved)
  }

  if(hasArg(quantile.moderator)){
    Mquantilepoints <- quantile(Mobserved, quantile.moderator)
  }

  if(hasArg(values.moderator)){
    Mrawpoints <- values.moderator
  }

  Mvals <- sort(na.omit(unique(c(JNpoints, Mintervalpoints, Mquantilepoints, Mrawpoints))))

  effect <- bX + bI*Mvals
  SE     <- sqrt(V.bX + 2 * Mvals * C.bXbI + Mvals^2 * V.bI)
  tvals  <- effect / SE
  pvals  <- 2 * (1 - pt(q = abs(tvals), df = res.df))
  LLCI   <- effect - qt(1 - alpha/2, df = res.df) * SE
  ULCI   <- effect + qt(1 - alpha/2, df = res.df) * SE

  effects <- data.frame(Mvals, effect, SE, tvals, pvals, LLCI, ULCI)
  names(effects) <- c(names(B)[Mpos], "Effect", "se", "t", "p", "LLCI", "ULCI")
  out <- list(effects = effects, data = object$model,
              arguments = arguments, formula = object$call)
  class(out) <- "probeInteraction"
  return(out)
}

#' @title plot probed interaction
#' @description Plot the effects of the antecedent as a function of the moderator.
#'
#' `r lifecycle::badge("stable")`
#' @param x object of class \code{probeInteraction}.
#' @param ... other arguments (none are used).
#' @param col.JN color for Johnson-Neyman cut-off line(s).
#' @param lty.JN linetype for Johnson-Neyman cut-off line(s).
#' @param col.CI color of the shade for the confidence interval.
#' @param lty.CI linetype for confidence interval boundaries.
#' @param lty.0 linetype for the horizontal line where the effect of the focal predictor on
#'     the outcome equals 0.
#'
#' @return \code{plot.probeInteraction} returns a combined plot with p value on the
#'     first y axis and effect of the antecedent variable.
#' @export
#'
#' @examples
#' lm.1 <- lm(mpg ~ hp * wt, data = mtcars)
#' pI.1 <- probeInteraction(lm.1, hp, wt, JN=TRUE, n.interval.moderator = 3,
#'                          quantile.moderator = c(0.1,0.9), values.moderator = 2)
#' plot(pI.1)
#' lm.2 <- lm(mpg ~ qsec * drat, data = mtcars)
#' pI.2 <- probeInteraction(lm.2, qsec, drat, JN=TRUE, n.interval.moderator = 30,
#'                          quantile.moderator = c(0.1,0.9), values.moderator = 2)
#' plot(pI.2)
#' @importFrom grDevices rgb
#' @importFrom graphics mtext
#' @author Mathijs Deen
plot.probeInteraction <- function(x,
                                  ...,
                                  col.JN = "red",
                                  lty.JN = "dotted",
                                  col.CI = rgb(red = .5, green = .5, blue = .5, alpha = .2),
                                  lty.CI = "longdash",
                                  lty.0  = "dotted"){
  modName <- x$arguments$moderator
  antName <- x$arguments$antecedent
  modVals <- eval(modName, x$effects)
  pVals   <- x$effects$p
  effVals <- x$effects$Effect
  conName <- as.character(x$formula[[2]][2])
  LLCI    <- x$effects$LLCI
  ULCI    <- x$effects$ULCI
  yRange  <- c(min(LLCI), max(ULCI))
  ifelse("alpha" %in% names(x$arguments), alpha <- x$arguments$alpha, alpha <- .05)
  ifelse("JN" %in% names(x$arguments), JN <- x$arguments$JN, JN <- TRUE)

  plot(x    = modVals,
       y    = effVals,
       ylim = yRange,
       ylab = paste0("effect of ", antName, " on ", conName),
       xlab = modName,
       ...)
  lines(x = modVals, y = effVals, ...)
  lines(x = modVals, y = LLCI, lty=lty.CI)
  lines(x = modVals, y = ULCI, lty=lty.CI)
  abline(h=0, lty=lty.0)
  polygon(c(modVals, rev(modVals)),
          c(LLCI, rev(ULCI)),
          col=col.CI,
          border = NA)
  if(JN){
    JNpoints <- modVals[which(abs(pVals-alpha) < 10 * .Machine$double.eps)]
    abline(v   = JNpoints,
           col = col.JN,
           lty = lty.JN)
    mtext(sprintf("%.3f", JNpoints),side=3, at=JNpoints, line = 0.5)
  }
}

#' @title Print effects of probed interaction
#' @description Print the effects from a probed interaction.
#'
#' `r lifecycle::badge("stable")`
#' @param x object of class \code{probeInteraction}.
#' @param ... other parameters (none are used).
#'
#' @return \code{print.probeInteraction} prints the effects table of a
#'     \code{probeInteraction} object.
#' @export
#'
#' @examples
#' lm.1 <- lm(mpg ~ hp * wt, data = mtcars)
#' pI <- probeInteraction(lm.1, hp, wt, JN=TRUE, n.interval.moderator = 3,
#'                        quantile.moderator = c(0.1,0.9), values.moderator = 2)
#' print(pI)
#' @author Mathijs Deen
print.probeInteraction <- function(x, ...){
  print(x$effects)
}
