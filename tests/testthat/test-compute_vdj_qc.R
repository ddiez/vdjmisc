d <- data.frame(barcode = c("bc1", "bc1", "bc2", "bc2", "bc2", "bc3", "bc4", "bc5", "bc5"))
d$chain <- c("TRA", "TRB", "TRA", "TRB", "TRB", "TRA", "TRB", "TRA", "TRA")

x <- compute_vdj_qc(d)


test_that("compute_vdj_qc works", {
  expect_false(is.null(x))
  expect_equal(dim(x), c(5, 5))
  expect_equal(colnames(x), c("barcode", "TRA", "TRB", "paired", "doublet"))
  expect_equal(x[["TRA"]], c(1, 1, 1, 0, 2))
  expect_equal(x[["TRB"]], c(1, 2, 0, 1, 0))
  expect_equal(x[["paired"]], c(TRUE, TRUE, FALSE, FALSE, FALSE))
  expect_equal(x[["doublet"]], c(FALSE, TRUE, FALSE, FALSE, TRUE))
})
