#' @title Spread Data
#'
#' @description \code{spread_data} is written to work with functions from 
#' the \pkg{revealedPrefs} package.
#' 
#' \code{\link[revealedPrefs]{checkWarp}} and similar "check" functions
#' take as input two \code{n} by \code{i} matrices, one for goods and one for prices. 
#' \code{spread_data} takes a dataframe and returns a list with two matrices, which
#' can then be passed into \code{\link[revealedPrefs]{checkWarp}} 
#' and similar functions.
#' 
#'
#' @param df a dataframe containing columns with quantities and prices of goods
#' @param ... the columns within \code{df} that contain quantities and prices. 
#' The first half of the columns provided are assumed to be quantities and the
#' second half of the columns provided are be assumed to be prices.
#' 
#' @return a list containg two matrices:
#' \describe{
#'   \item{x}{a matrix containing quantities}
#'   \item{p}{a matrix containing prices}
#' }
#'
#' @import dplyr
#'
#' @export
#' @examples 
#' # use simGarp and gather_data to create a sample df containing 4 observations and 6 columns (3 quantities and 3 prices)
#' df <- simGarp(4, 3) %>% gather_data()
#' 
#' # use spread_data to return a list of two matrices
#' spread_data(df, x1, x2, x3, p1, p2, p3)

spread_data <- function(df, ...){
  if (!is.data.frame(df)) {stop("df must be a dataframe")}
  
  variables <- quos(...)
  last_good <- length(variables) / 2
  first_price <- last_good + 1

  if (length(variables) == 0) {stop("enter columns for quanities and prices")}
  if (length(variables)%%2 != 0) {stop("enter an even number of columns")}
  
  df_subset <- select(df, !!!variables)
  
  goods <- as.matrix(df_subset[1:last_good])
  prices <- as.matrix(df_subset[first_price: length(variables)])
  
  return(
    list("x" = goods,
         "p" = prices)
  )
}