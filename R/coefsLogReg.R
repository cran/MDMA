#'@title Coefficients for logistic regression analysis
#'@description Show odds ratios and their confidence intervals for logistic regression parameter estimates.
#'
#' `r lifecycle::badge("stable")`
#'@param model object of class \code{glm}, with family parameter set to \code{binomial}.
#'@param confint indicates whether a confidence interval for the odds ratio should be returned.
#'@param level the confidence level required.
#'@return \code{coefsLogReg} returns the same table as \code{summary(object)$coefficients},
#'    with the addition of the coefficients' odds ratios and their confidence intervals.
#'@examples
#'glm(formula = am ~  disp, family = binomial, data = mtcars) |>
#'  coefsLogReg()
#'@author Mathijs Deen
#'@importFrom stats confint summary.glm
#'@import MASS
#'@export
coefsLogReg <- function(model, confint = TRUE, level = 0.95) {
  if(as.character(model$call[1]) != "glm") stop("model should be of class glm", call.=FALSE)
  if(model$family$family != 'binomial') stop("This function only applies to logistic regression analyses", call.=FALSE)
  out <- cbind(summary.glm(model)$coefficients,
               exp(summary.glm(model)$coefficients)[ , 1])
  colnames(out) <- c("B", "SE", "Z", "p", "OR")
  if(confint){
    CI <- exp(confint(object = model, level = level))[ ,c(1, 2)]
    CIbounds <- c((1 - level)/2, level + (1 - level)/2)
    CIboundNames <- paste0("CI_", 100*CIbounds)
    out <- cbind(out, CI)
    colnames(out)[6:7] <- CIboundNames
  }
  return(out)
}
