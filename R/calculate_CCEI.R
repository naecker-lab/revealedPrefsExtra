#' @title Calculate_CCEI
#'
#' @description \code{calculate_CCEI} does something cool.
#'
#' @param df a dataframe
#' @param step the step does something
#' 
#' @return a integer
#'
#' @import revealedPrefs
#'
#' @export
#' @examples 
#' calculate_CCEI(simGarp(10, 2, afriat.par = 0.5), step = 0.001)

calculate_CCEI <- function(df, step = 0.01){
  index <- 1
  while (index > 0) {
    test <- checkGarp(df$x, df$p, afriat.par = index)
    if (test$violation) {
      index <- index - step
    } else {
      return(index)
    }
  }
  return(0)
}