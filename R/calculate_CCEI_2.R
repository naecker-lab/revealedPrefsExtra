#' @title Calculate_CCEI_2
#'
#' @description \code{calculate_CCEI_2} 
#'
#' @param df a dataframe
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

median_fct <- function(c) {
  if (length(c)%%2==1) {
    return (median(c))
  }
  else {
    return(median(sort(c)[-length(c)]))
  }
}

calculate_CCEI_2 <- function(df, ...){
  #browser()
  if (!is.data.frame(df)) {stop("df must be a dataframe")}
  
  variables <- quos(...)
  if (length(variables) == 0) {stop("enter columns for quanities and prices")}
  if (length(variables)%%2 != 0) {warning("enter an even number of columns")}
  
  list <- spread_data(df, ...)
  
  test <- checkGarp(list$x, list$p)
  if (!test$violation) {
    return (1)
  }
  x<- list$x
  p<- list$p
  
  Exp <- p %*% t(x) / matrix(diag(p %*% t(x)), byrow = F, ncol = nrow(x), nrow = nrow(x))
  set <- Exp[(upper.tri(Exp) | lower.tri(Exp)) & Exp<=1]
  median <- median_fct(set)
  while (length(set)>2) {
    if (checkGarp(x,p,median)$violation) {
      set <- set [set<=median] # get rid of everything above it as "median" has a violation, so everything \geq median must have  
      median <- median_fct(set) # setting new median
    }
    else {
      set <- set [set>=median] # median works, so every under it works and thus its an over estimate of irrationality
      median <-  median_fct(set) # setting new median
    }
  }
  lo <- min(set)
  hi <- max(set)
  epsilon <- abs(hi-lo)/2
  if (!checkGarp(x,p,lo)$violation & !checkGarp(x,p,hi)$violation){ # first cond
    if (checkGarp(x,p,hi+epsilon)$violation==F){
      return(1)
    }
    else{return(hi)}
  }
  if (checkGarp(x,p,lo)$violation & checkGarp(x,p,hi)$violation) { # second cond
    return(lo)
  }
  if (!checkGarp(x,p,lo)$violation & checkGarp(x,p,hi)$violation){ # third cond
    if (!checkGarp(x,p,lo+epsilon)$violation){
      return(hi)
    }
    else{return (lo)}
  }
}