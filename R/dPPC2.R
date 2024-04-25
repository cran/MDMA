#' @title Effect sizes from pretest-posttest-control group designs
#' @description \code{dPPC2} calculates an effect size for studies with
#'     pretest and posttest scores for two groups, usually a treatment and
#'     a control group. It is based on Morris (2008), who based it on Becker (1988).
#'
#' `r lifecycle::badge("stable")`
#' @param preT pre-scores for treatment group.
#' @param posT post-scores for treatment group.
#' @param preC pre-scores for control group.
#' @param posC post-scores for control group.
#' @param correct indicates whether a correction factor should be calculated (i.e., Hedges' *g* instead of Cohen's *d*).
#' @param CIlevel the confidence level required.
#' @return \code{dPPC2} returns a vector of length 6, containing:
#' \item{d}{the effect size estimate.}
#' \item{SE}{the standard error of the effect sie estimate.}
#' \item{lower.bound}{lower bound of the confidence interval.}
#' \item{upper.bound}{upper bound of the confidence interval.}
#' \item{NT}{sample size of treatment group.}
#' \item{NC}{sample size of control group.}
#' @references
#' \itemize{
#' \item{Becker, B.J. (1988). Synthesizing standardized mean-change measures. *British Journal of Mathematical and Statistical Psychology, 41*, 257-278.}
#' \item{Morris, S.B. (2008). Estimating effect sizes from pretest-posttest-control group designs. *Organizational Research Methods, 11*, 364-386.}}
#' @examples
#' library(MASS)
#' set.seed(1)
#' treatment <- mvrnorm(n=50, mu=c(50,40), Sigma = matrix(c(100,70,70,100), ncol=2), empirical = TRUE)
#' control <- mvrnorm(n=50, mu=c(50,45), Sigma = matrix(c(100,70,70,100), ncol=2), empirical = TRUE)
#' dPPC2(treatment[,1], treatment[,2], control[,1], control[,2])
#' @author Mathijs Deen
#' @importFrom stats qnorm na.omit cor sd
#' @export
dPPC2 <- function(preT, posT, preC, posC, correct = TRUE, CIlevel = .95){
  # pairwise deletion of NAs
  bindT <- na.omit(cbind(preT,posT))
  bindC <- na.omit(cbind(preC,posC))

  # inputs for d
  MpreT <- mean(bindT[,1])
  MposT <- mean(bindT[,2])
  MpreC <- mean(bindC[,1])
  MposC <- mean(bindC[,2])
  SpreT <- sd(bindT[,1])
  SpreC <- sd(bindC[,1])
  NT    <- nrow(bindT)
  NC    <- nrow(bindC)
  DF    <- NT + NC - 2
  rho   <- cor(rbind(bindT, bindC))[2,1]
  CP    <- ifelse(correct == TRUE, 1 - 3 / (4 * (DF) - 1), 1)
  SDpre <- sqrt(((NT - 1) * SpreT^2 + (NC - 1) * SpreC^2) / DF)

  # Morris (2008), Equation 8 (PPC2)
  d     <- CP * (((MposT - MpreT) - (MposC - MpreC)) / SDpre)

  # variance of d, chopped up because it's a long equation (Eq. 25 in Morris (2008))
  Vard1 <- 2 * CP^2 * (1 - rho)
  Vard2 <- ((NT + NC) / (NT * NC)) * (DF / (DF - 2))
  Vard3 <- 1 + d^2 / (2 * (1 - rho) * ((NT + NC) / (NT * NC)))
  Vard  <- Vard1 * Vard2 * Vard3 - d^2
  SEd   <- sqrt(Vard)
  CIlb  <- d - qnorm((1 + CIlevel) / 2) * SEd
  CIup  <- d + qnorm((1 + CIlevel) / 2) * SEd
  out   <- data.frame(d = d,
                      SE = SEd,
                      `lower.bound` = CIlb,
                      `upper.bound` = CIup,
                      NT = NT,
                      NC = NC)

  if(!is.null(attributes(bindC)$na.action) | !is.null(attributes(bindT)$na.action)) {
    message(sprintf("NAs omitted. Values based on total N = %d.", NT + NC))
  }

  return(out)
}
