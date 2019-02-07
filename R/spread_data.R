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