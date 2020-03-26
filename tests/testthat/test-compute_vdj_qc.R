d <- tibble(barcode = c("bc1", "bc1", "bc2", "bc2", "bc2", "bc3", "bc4", "bc5", "bc5"))
d <- d %>% mutate(chain = c("TRA", "TRB", "TRA", "TRB", "TRB", "TRA", "TRB", "TRA", "TRA"))

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


d <- add_vdj_qc(d)

test_that("add_vdj_qc works", {
  expect_false(is.null(d))
  expect_equal(dim(d), c(9, 4))
  expect_equal(colnames(d), c("barcode", "chain", "paired", "doublet"))
  expect_equal(d[["paired"]], c(TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE))
  expect_equal(d[["doublet"]], c(FALSE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE))
})


d <- d %>% select(barcode, paired, doublet) %>% distinct() %>% arrange(barcode)
x <- x %>% select(barcode, paired, doublet) %>% arrange(barcode)

test_that("compute_vdj_qc and add_vdj_qc give identical results", {
  expect_true(identical(d, x))
})
