#' @title Receiver operator characteristic
#' @description Calculate ROC curve statistics.
#'
#' `r lifecycle::badge("stable")`
#' @param response response variable for which thresholds will be calculated.
#' @param group group variable.
#' @param levels relevant levels of \code{group} variable. Should have length 2.
#' @param state state level of \code{levels}..
#'
#' @return Returns a list with the following elements:
#' \item{data}{\code{data.frame} with two columns, containing the response and
#'     group variable for each level in \code{levels} without missings.}
#' \item{rdf}{ROC dataframe. This is a \code{data.frame} containing sensitivity and
#'     specificity values for all threshold values.}
#' \item{auc}{Area under the ROC curve.}
#' \item{response}{Response variable from input data.}
#' \item{group}{Group variable from the input data.}
#' \item{levels}{Used levels.}
#' \item{state}{State level.}
#'
#' @export
#'
#' @examples
#' roc(QIDS$QIDS, QIDS$depression, c("No","Yes"), "Yes") |>
#'   plot(ylim.3=c(0,.2))
#' @author Mathijs Deen
roc <- function(response, group, levels, state) {
  obs <- group[group %in% levels]
  data <- na.omit(data.frame(response, obs))
  x <- data$response
  xsort <- sort(unique(x))
  thresholds <- (c(xsort, Inf) + c(-Inf, xsort)) / 2
  obs <- data$obs
  obsBool <- obs == state
  xlist <- rep(list(x), length(thresholds))
  xWhich <- mapply(.Primitive(">="), xlist, thresholds)

  sensitivities <- colMeans(xWhich[obsBool == TRUE, ])
  specificities <- 1 - colMeans(xWhich[obsBool == FALSE, ])
  falsePosRates <- colMeans(xWhich[obsBool == FALSE, ])

  auc <- 1 - wilcox.test(x ~ obs)$statistic / prod(table(obs))

  rdf <- data.frame("thresholds"    = thresholds,
                    "sensitivities" = sensitivities,
                    "specificities" = specificities)
  outlist <- list(data = data, rdf = rdf, auc = as.numeric(auc),
                  response = response, group = group, levels = levels,
                  state = state)
  class(outlist) <- "roc"
  return(outlist)
}

#' @title plot roc curve
#' @description Plot an ROC curve.
#'
#' `r lifecycle::badge("stable")`
#' @param x object of class \code{roc}.
#' @param y argument for generic \code{plot} function, not used here.
#' @param which which plots to show (see Details).
#' @param orientation indicate whether the plots should be arranged horizontally
#'     or vertically.
#' @param cutoffs.1 cutoff value(s) to be shown in the first plot.
#' @param cutoffs.2 cutoff value(s) to be shown in the second plot.
#' @param cutoffs.3 cutoff value(s) to be shown in the third plot.
#' @param xlab.3 lable for x axis in third plot.
#' @param labels.3 legend labels for third plot.
#' @param xlim.3 xlim for third plot.
#' @param ylim.3 ylim for third plot.
#' @param pos.legend.2 legend position for second plot.
#' @param pos.legend.3 legend position for third plot.
#' @param ... other arguments for generic \code{plot} function, none are used here.
#'
#' @return \code{plot.roc} provides three plots:
#' \itemize{
#'  \item The first plot contains the ROC curve.
#'  \item The second plot contains curves for the sensitivity and the specificity
#'      for all threshold values.
#'  \item The third plot contains density plots for the two classification groups.
#' }
#'
#' @examples
#' a <- roc(QIDS$QIDS, QIDS$depression, c("Yes","No"), "Yes")
#' plot(a, ylim.3 = c(0,.2), xlab.3= "QIDS value", cutoffs.1 = 14.5,
#'      cutoffs.2 = 14.5, cutoffs.3 = 14.5)
#' @importFrom graphics abline legend lines par points text
#' @importFrom stats density wilcox.test
#' @author Mathijs Deen
#' @export
plot.roc <- function(x, y, which = 1:3,
                     orientation = c("horizontal", "vertical"),
                     cutoffs.1=NULL,
                     cutoffs.2=NULL, cutoffs.3=NULL,
                     xlab.3=NULL,
                     labels.3=NULL, xlim.3=NULL,
                     ylim.3=c(0,10), pos.legend.2="right",
                     pos.legend.3="topright", ...) {
  or <- match.arg(arg = orientation, several.ok = FALSE)
  nPlots <- length(which)
  ifelse(or == "horizontal",
         mfDim  <- c(1,nPlots),
         mfDim  <- c(nPlots,1))
  oldpar <- par(mfrow = mfDim,
                pty   = "s")
  on.exit(par(oldpar), add = TRUE)

  rr2 <- x$rdf
  rr2$falsePosRates <- 1 - x$rdf$specificities

  if(1 %in% which){
    plot(x    = rr2$falsePosRates,
         y    = rr2$sensitivities,
         ylim = c(0,1),
         xlim = c(0,1),
         xlab = "False positive rate (1 - specificity)",
         ylab = "True positive rate (sensitivity)",
         type = "l")
    abline(a=0,b=1,col="grey")

    if(!is.null(cutoffs.1)){
      trueCO.1 <- cutoffs.1 * NA
      for(i in 1:length(cutoffs.1)){
        trueCO.1[i] <- rr2$thresholds[which.min(abs(cutoffs.1[i]-rr2$thresholds))[1]]
      }
      points(x    = rr2$falsePosRates[which(rr2$thresholds %in% trueCO.1)],
             y    = rr2$sensitivities[which(rr2$thresholds %in% trueCO.1)])
      text(x      = rr2$falsePosRates[which(rr2$thresholds %in% trueCO.1)],
           y      = rr2$sensitivities[which(rr2$thresholds %in% trueCO.1)],
           labels = cutoffs.1,
           adj    = c(-0.5,1))
    }
    text(x      = .6,
         y      = .3,
         labels = sprintf("AUC = %.3f", x$auc))
  }

  if(2 %in% which){
    plot(x    = rr2$thresholds,
         y    = rr2$sensitivities,
         type = "l",
         ylim = c(0,1),
         xlim = c(min(rr2$thresholds[is.finite(rr2$thresholds)]),
                  max(rr2$thresholds[is.finite(rr2$thresholds)])),
         xlab = "Thresholds",
         ylab = "Sensitivity, Specificity",
         lty  = 1)
    lines(x   = rr2$thresholds,
          y   = rr2$specificities,
          lty = 2)
    legend(x      = pos.legend.2,
           legend = c("Sensitivity","Specificity"),
           lty    = c(1,2))
    abline(v   = cutoffs.2,
           lty = 3)
    abline(h   = -.01,
           col = "grey")
    abline(h   = 1.01,
           col = "grey")
  }

  if(3 %in% which){
    ifelse(is.null(labels.3),
           legendnames <- levels(x$group),
           legendnames <- labels.3)
    ifelse(is.null(xlim.3),
           xlm3 <- c(min(x$response),max(x$response)),
           xlm3 <- xlim.3)
    plot(x    = density(x$response[x$group==levels(x$group)[1]]),
         xlim = xlm3,
         main = "",
         xlab = ifelse(is.null(xlab.3), "", xlab.3),
         lty  = 1,
         ylim = ylim.3)
    lines(x   = density(x$response[x$group==levels(x$group)[2]]),
          lty = 2)
    legend(x      = pos.legend.3,
           legend = legendnames,
           lty    = c(1,2))
    abline(v   = cutoffs.3,
           lty = 3)
    abline(h   = 0,
           col ="grey")
  }
}
