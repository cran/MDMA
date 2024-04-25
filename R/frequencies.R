#' @title Display frequency table
#' @description Display frequency table with percentages and cumulative percentages.
#'
#' `r lifecycle::badge("stable")`
#' @param x vector of values.
#' @return object of type \code{data.frame} containing frequencies, percentages and cumulative percentages.
#' @examples
#' frequencies(datasets::mtcars$carb)
#' @author Mathijs Deen
#' @export
frequencies <- function(x) {
  q <- table(droplevels(as.factor(x)))
  out <- data.frame(cbind(n        = q,
                          perc     = 100*q/sum(q),
                          cum.perc = NA))
  out$cum.perc[1] <- out$perc[1]
  if(nrow(out)>1){
    for(i in 2:nrow(out)){
      out$cum.perc[i] <- out$cum.perc[i-1] + out$perc[i]
    }
  }
  return(out)
}
