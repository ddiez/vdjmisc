context("read_vdj_10x")

f <- list.files(file.path(system.file(package = "vdjmisc"), "extdata/10x"), full.names = TRUE)

vdj <- read_vdj_10x(f[1])


test_that("read_vdj_10x works", {
  expect_equal(dim(vdj), c(61, 18))
})


path <- file.path(system.file(package = "vdjmisc"), "extdata/10x")
vdj <- read_vdj_10x_batch(path, extension = "csv.gz")


test_that("read_vdj_10x_batch works", {
  expect_equal(dim(vdj), c(278, 19))
})
