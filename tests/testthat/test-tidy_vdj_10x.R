context("tidy_vdj_10x")

f <- list.files(file.path(system.file(package = "vdjmisc"), "extdata/10x"), full.names = TRUE)

vdj <- read_vdj_10x(f[1])
tidy_vdj <- tidy_vdj_10x(vdj)

test_that("tidy_vdj_10x works", {
  expect_equal(dim(tidy_vdj), c(18, 35))
})

path <- file.path(system.file(package = "vdjmisc"), "extdata/10x")
vdj <- read_vdj_10x_batch(path, extension = "csv.gz")
tidy_vdj <- tidy_vdj_10x(vdj)

test_that("tidy_vdj_10x works on batch", {
  expect_equal(dim(tidy_vdj), c(75, 37))
})
