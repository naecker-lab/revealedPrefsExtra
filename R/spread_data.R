#' @title Spread Data
#'
#' @description \code{spread_data} does something cool.
#'
#' @param df a dataframe
#' 
#' @return a tibble
#'
#' @import dplyr
#' @import stringr
#'
#' @export
#' @examples 
#' # simGarp is a function from revealedPrefs that creates a 
#' # a random data set that is returned as a list containing two
#' # matrices
#' simGarp(4, 2) %>%
#' spread_data()

spread_data <- function(df){
  quantities <- 
    df$x %>%
    as_tibble() %>%
    rename_all(funs(str_replace_all(., "V", "x"))) 
  
  prices <- 
    df$p %>%
    as_tibble() %>%
    rename_all(funs(str_replace_all(., "V", "p")))
  
  return(
    as_tibble(cbind(quantities, prices)) %>%
      mutate(observation = 1:dim(prices)[1])
  )
}