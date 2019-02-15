#' @title Plot Data
#'
#' @description \code{plot_data} does something cool.
#'
#' @param df a dataframe
#'
#' @import dplyr
#' @import ggplot2
#'
#' @export
#' @examples
#' simGarp(4, 2) %>%
#' spread_data() %>%
#' plot_data()

plot_data <- function(df){
  df %>%
    mutate(
      income = p1*x1 + p2*x2,
      y_intercept = income/p2,
      x_intercept = income/p1,
      slope = -p1/p2
    ) %>%
    ggplot() +
    geom_point(aes(x = x1, y = x2, color = as.factor(observation))) +
    geom_segment(aes(x = x_intercept, y = 0, xend = 0, yend = y_intercept, color = as.factor(observation))) +
    theme_bw() +  
    theme(legend.position = "none")
}