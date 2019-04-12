#' @title Calculate_CCEI
#'
#' @description \code{calculate_CCEI} 
#'
#' @param df a dataframe
#' @param step 
#' 
#' @return the CCEI score
#'
#' @import dplyr
#' @import revealedPrefs
#'
#' @export
#' @examples 
#' set.seed(1234)
#' df <- gather_data(simGarp(10, 2, afriat.par = 0.5))
#' calculate_CCEI(df, step = 0.001, x1, x2, p1, p2)


calculate_CCEI <- function(df, step, ...){
  
  if (!is.data.frame(df)) {stop("df must be a dataframe")}
  
  variables <- quos(...)
  if (length(variables) == 0) {stop("enter columns for quanities and prices")}
  if (length(variables)%%2 != 0) {warning("enter an even number of columns")}
  
  list <- spread_data(df, ...)
  
  hi = 1
  lo = 0
  while (lo < hi){
    index = (lo + hi) / 2
    
    test <- checkGarp(list$x, list$p, afriat.par = index)
    
    # if passes Garp, return 1
    if (!test$violation) {
      return (1)
    }
    
    if (test$violation){
      if (checkGarp(list$x, list$p, afriat.par = index + step)$violation == FALSE){
        return (index)
      }
      hi = index 
    } else {
      lo = index + step
    }
  }
  return(index)
}

