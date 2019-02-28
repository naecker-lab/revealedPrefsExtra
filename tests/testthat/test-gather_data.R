context("gather_data function")

test_that("errors generated for bad input", {
  x <- 2
  y <- list("a" = 2.5, "b" = TRUE, "c" = 1:3)
  
  expect_error(gather_data(x), "matrix_list must be a list object")
  expect_error(gather_data(y), "matrix_list must be a list object")
  expect_error(gather_data(mtcars), "matrix_list must be a list object")
})

test_that("output is correct", {
  test_df <- simGarp(3,2) %>% gather_data()

  expect_equal(ncol(test_df), 4)
  expect_equal(nrow(test_df), 3)
  expect_equal(class(test_df), c("tbl_df", "tbl", "data.frame"))
})

test_that("test for directionality", {
  # start with a dataframe
  data(mtcars)
  
  spread_list <- mtcars %>% spread_data(wt, mpg)
  gather_df <- spread_list %>% gather_data()
  
  expect_equal(sum(gather_df != mtcars[c("wt", "mpg")]), 0)
  
})