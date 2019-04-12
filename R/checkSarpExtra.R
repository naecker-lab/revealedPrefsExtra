#' @title Check Sarp Extra
#'
#' @description \code{checkSarpExtra} is a wrapper function for the
#'\code{\link[revealedPrefs]{checkSarp}} function from the \pkg{revealedPrefs} package.
#' 
#'
#' @param df a dataframe containing columns with quantities and prices of goods
#' @param ... the columns within \code{df} that contain quantities and prices. 
#' The first half of the columns provided are assumed to be quantities and the
#' second half of the columns provided are be assumed to be prices.
#' 
#' @return 
#'
#' @import dplyr
#' @import revealedPrefs
#'
#' @export
#' @examples 
#' experiment_data<- tibble(subject = c(1, 1, 2, 2),
#'                               x1 = c(1:4),
#'                               x2 = c(5:8),
#'                               p1 = c(5:8),
#'                               p2 = c(5:8))
#'  
#'experiment_data %>%
#'  checkSarpExtra(x1, x2, p1, p2)



checkSarpExtra <- function(df, ...){
  if (!is.data.frame(df)) {stop("df must be a dataframe")}
  
  variables <- quos(...)
  if (length(variables) == 0) {stop("enter columns for quanities and prices")}
  if (length(variables)%%2 != 0) {stop("enter an even number of columns")}
  
  list <- spread_data(df, ...)
  checkSarp(list$x, list$p)
}