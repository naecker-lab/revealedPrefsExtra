context("calculate_CCEI function")


# Create Test Data --------------------------------------------------------
# create dataframe with groups
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

noGarp_df <- gather_data(noGarp)
noGarp_df$id <- 3

# noSarp ------------------------------------------------------------------
data(noSarp)
colnames(noSarp$x) <- NULL
colnames(noSarp$p) <- NULL

noSarp_df <- gather_data(noSarp)
noSarp_df$id <- 4

# okSarp ------------------------------------------------------------------
data(okSarp)
colnames(okSarp$x) <- NULL
colnames(okSarp$p) <- NULL

okSarp_df <- gather_data(okSarp)
okSarp_df$id <- 5

all_tests <- rbind(noAxiom_df, noWarp_df, noGarp_df, okSarp_df)


# Create Tests -------------------------------------------------------------------

test_that("CCEI output is correct", {
  expect_equal(calculate_CCEI(noGarp_df, step = .1, x1, x2, x3, p1, p2, p3), .9)
  expect_equal(calculate_CCEI(noGarp_df, step = .001, x1, x2, x3, p1, p2, p3), .987)
})


test_that("CCEI function version 2 output is correct", {
  expect_equal(calculate_CCEI_2(noGarp_df, x1, x2, x3, p1, p2, p3), .987)
})