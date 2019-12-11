context("read_vdjdb_full")

f <- list.files(file.path(system.file(package = "vdjmisc"), "extdata/vdjdb"), full.names = TRUE)

vdjdb <- read_vdjdb_full(f, paired = FALSE)

test_that("read_vdjdb_full works", {
  expect_equal(dim(vdjdb), c(100, 34))
})


vdjdb <- read_vdjdb_full(f, paired = TRUE)

test_that("read_vdjdb_full works", {
  expect_equal(dim(vdjdb), c(47, 34))
})
