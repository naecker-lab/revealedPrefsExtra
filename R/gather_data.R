#' @title Gather Data
#'
#' @description \code{gather_data} is written to work with functions from 
#' the \pkg{revealedPrefs} package.
#' 
#' \code{\link[revealedPrefs]{checkWarp}} and similar "check" functions
#' take as input two \code{n} by \code{i} matrices, one for goods and one for prices. 
#' \code{gather_data} takes a list with two matrices and returns a dataframe.
#' 
#'
#' @param matrix_list a list containg two matrices:
#' \describe{
#'   \item{x}{a matrix containing quantities}
#'   \item{p}{a matrix containing prices}
#' } 
#' 
#' @return a dataframe where the rows represent an individual's choice and the columns
#' represent the quantities and prices of a good
#'
#' @import dplyr
#' @import stringr
#'
#' @export
#' @examples 
#' simGarp(2,4) %>%
#' gather_data()

gather_data <- function(matrix_list){
  if(!is.list(matrix_list) | is.data.frame(matrix_list))
  {stop("matrix_list must be a list object containing x, a matrix of quantities
        and p, a matrix of prices")}
  
  if(is.null(matrix_list$x) | is.null(matrix_list$p))
  {stop("matrix_list must be a list object containing x, a matrix of quantities
        and p, a matrix of prices")}
  
  quantities <- 
    matrix_list$x %>%
    as_tibble() %>%
    rename_all(funs(str_replace_all(., "V", "x"))) 
    # what is this rename doing?
  
  prices <- 
    matrix_list$p %>%
    as_tibble() %>%
    rename_all(funs(str_replace_all(., "V", "p"))) 
  
  # why return this as a tibble over a dataframe?
  return(
    as_tibble(cbind(quantities, prices)) %>%
      mutate(observation = 1:dim(prices)[1])
  )
  
  # return(
  #   cbind(quantities, prices) %>%
  #     mutate(observation = 1:dim(prices)[1])
  # )
}