library(revealedPrefs)
library(dplyr)
library(stringr)
library(ggplot2)

#### FUCTIONS ####

# each of these should get their own R file in the R/ directory of the package

# convert matrix-style data to single combined data set
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

# plot data sets on two-axis grid
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

# calculate CCEI given choice data
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

#### TESTS/EXAMPLES ####

# this stuff should go either in tests/ or be part of the examples in the documentation

# show how spread_data and plot_data work
simGarp(4, 2) %>%
  spread_data() %>%
  plot_data()
  
# test calculate_CCEI
data("noGarp")
data("noAxiom")
data("okSarp") 
calculate_CCEI(noAxiom)
calculate_CCEI(noGarp)
calculate_CCEI(noSarp)
calculate_CCEI(okSarp)
calculate_CCEI(simGarp(10, 2, afriat.par = 0.5), step = 0.001)
