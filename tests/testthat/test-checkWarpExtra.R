context("check*arp functions")


# Create test data --------------------------------------------------------


# noAxiom -----------------------------------------------------------------

data(noAxiom)
colnames(noAxiom$x) <- NULL
colnames(noAxiom$p) <- NULL

noAxiom_df <- gather_data(noAxiom)
noAxiom_df$id <- 1

# noWarp ------------------------------------------------------------------

data(noWarp)
colnames(noWarp$x) <- NULL
colnames(noWarp$p) <- NULL

noWarp_df <- gather_data(noWarp)
noWarp_df$id <- 2


# noGarp ------------------------------------------------------------------
data(noGarp)
colnames(noGarp$x) <- c("x1", "x2", "x3")
colnames(noGarp$p) <- c("p1", "p2", "p3")

noGarp_df<- gather_data(noGarp)
noGarp_df$id <- 3

# noSarp ------------------------------------------------------------------
data(noSarp)
colnames(noSarp$x) <- NULL
colnames(noSarp$p) <- NULL

noSarp_df <- gather_data(noSarp)


# okSarp ------------------------------------------------------------------
data(okSarp)
colnames(okSarp$x) <- NULL
colnames(okSarp$p) <- NULL

okSarp_df <- gather_data(okSarp)
okSarp_df$id <- 4

all_tests <- rbind(noAxiom_df, noWarp_df, noGarp_df, okSarp_df)


# Create tests ------------------------------------------------------------


test_that("output is correct", {

  x <- all_tests %>%
    group_by(id) %>%
    do(data.frame(violates_warp = checkWarpExtra(., x1, x2, x3, p1, p2, p3)$violation,
                  violates_garp = checkGarpExtra(., x1, x2, x3, p1, p2, p3)$violation,
                  violates_sarp = checkSarpExtra(., x1, x2, x3, p1, p2, p3)$violation))
  
  expect_equal(x$violates_warp, c(TRUE, TRUE, FALSE, FALSE))
  expect_equal(x$violates_garp, c(TRUE, FALSE, TRUE, FALSE))
  expect_equal(x$violates_sarp, c(TRUE, TRUE, TRUE, FALSE))
  expect_equal(class(x), c("grouped_df", "tbl_df", "tbl", "data.frame"))
})

test_that("noSarp is correct", {
  
  x <- noSarp_df %>%
    summarise(violates_warp = checkWarpExtra(., x1, x2, x3,x4, x5, x6,
                                             p1, p2, p3, p4, p5, p6)$violation,
              violates_garp = checkGarpExtra(., x1, x2, x3,x4, x5, x6,
                                             p1, p2, p3, p4, p5, p6)$violation,
              violates_sarp = checkSarpExtra(., x1, x2, x3,x4, x5, x6,
                                             p1, p2, p3, p4, p5, p6)$violation)
  
  expect_equal(x$violates_warp, FALSE)
  expect_equal(x$violates_garp, FALSE)
  expect_equal(x$violates_sarp, TRUE)
  expect_equal(class(x), c("tbl_df", "tbl", "data.frame"))
})

