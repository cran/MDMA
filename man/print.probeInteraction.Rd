% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/probeInteraction.R
\name{print.probeInteraction}
\alias{print.probeInteraction}
\title{Print effects of probed interaction}
\usage{
\method{print}{probeInteraction}(x, ...)
}
\arguments{
\item{x}{object of class \code{probeInteraction}.}

\item{...}{other parameters (none are used).}
}
\value{
\code{print.probeInteraction} prints the effects table of a
\code{probeInteraction} object.
}
\description{
Print the effects from a probed interaction.

\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}
}
\examples{
lm.1 <- lm(mpg ~ hp * wt, data = mtcars)
pI <- probeInteraction(lm.1, hp, wt, JN=TRUE, n.interval.moderator = 3,
                       quantile.moderator = c(0.1,0.9), values.moderator = 2)
print(pI)
}
\author{
Mathijs Deen
}
