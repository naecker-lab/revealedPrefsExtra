context("spread_data function")

test_that("errors generated for bad input", {
  x <- 2
  y <- "hello"
  z <- list("x" = x, "y" = y)
  
  expect_error(spread_data(x), "df must be a dataframe")
  expect_error(spread_data(y), "df must be a dataframe")
  expect_error(spread_data(z), "df must be a dataframe")
  expect_error(spread_data(mtcars), "enter columns for quanities and prices")
  expect_warning(spread_data(mtcars, wt, mpg, cyl), "enter an even number of columns")
})

test_that("output is correct", {
  test_df <- simGarp(5,2) %>% gather_data() 
  x <- spread_data(test_df, x1, x2, p1, p2)
  
  expect_equal(length(x), 2)
  expect_equal(class(x$x), "matrix")
  expect_equal(class(x$p), "matrix")
})

test_that("test for directionality", {
  # start with a list
  start <- simGarp(5,2)
  gather_df <- start %>% gather_data() 
  spread_list <- gather_df %>% spread_data(x1, x2, p1, p2)
  
  expect_equal(sum(start$x != spread_list$x), 0)
  expect_equal(sum(start$p != spread_list$p), 0)

})