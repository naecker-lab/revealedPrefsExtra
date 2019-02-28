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