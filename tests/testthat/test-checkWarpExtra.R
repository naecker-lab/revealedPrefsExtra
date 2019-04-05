context("check*arp function")

test_that("output is correct", {
  # update this with real data that will give both true/false outputs
  experiment_data<-
    tibble(subject = c(1, 1, 1, 2, 2),
           x1 = c(1:5),
           x2 = c(5:9),
           p1 = c(5:9),
           p2 = c(5:9))
  
  x <- experiment_data %>%
    group_by(subject) %>%
    summarise(violates_warp = checkWarpExtra(., x1, x2, p1, p2)$violation,
              violates_garp = checkGarpExtra(., x1, x2, p1, p2)$violation,
              violates_sarp = checkSarpExtra(., x1, x2, p1, p2)$violation)
  
  expect_equal(x$violates_warp, c(FALSE, FALSE))
  expect_equal(x$violates_garp, c(FALSE, FALSE))
  expect_equal(x$violates_sarp, c(FALSE, FALSE))
  expect_equal(class(x), c("tbl_df", "tbl", "data.frame"))
})

