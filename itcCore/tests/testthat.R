if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("testthat is required to run these tests. Install with install.packages('testthat').")
}

library(testthat)

test_check("itcCore")
