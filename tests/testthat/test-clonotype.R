d <- data.frame(barcode = c("bc1", "bc1", "bc2", "bc2", "bc2", "bc3", "bc4", "bc5", "bc5"))
d$chain <- c("TRA", "TRB", "TRA", "TRB", "TRB", "TRA", "TRB", "TRA", "TRA")
d$v_gene <- c("TRAV1", "TRBV1", "TRAV1", "TRBV2", "TRBV3", "TRAV2", "TRBV1", "TRAV2", "TRAV3")
d$j_gene <- c("TRAJ1", "TRBJ2", "TRAJ2", "TRBJ2", "TRBJ1", "TRAJ2", "TRBJ2", "TRAJ2", "TRAJ5")
d$d_gene <- rep(NA_character_, 9)
d$cdr3 <- rep("ABC", 9)


d <- d %>% tidy_vdj_10x()


x <- d %>% add_clonotype_id() %>% add_clonotype_size()

test_that("add_clonotype_id and add_clonotype_size work", {
  expect_false(is.null(x))
  expect_equal(dim(x), c(3, 13))
  expect_true("clonotype_id" %in% colnames(x))
  expect_true("clonotype_size" %in% colnames(x))
})


x <- d %>% add_clonotype_id(name = "foo") %>% add_clonotype_size(by = "foo", name = "goo")

test_that("we can customize column names", {
  expect_false(is.null(x))
  expect_equal(dim(x), c(3, 13))
  expect_true("foo" %in% colnames(x))
  expect_true("goo" %in% colnames(x))
})


x <- d %>% clonotype_size()
test_that("clonotype_size works", {
  expect_false(is.null(x))
  expect_equal(dim(x), c(3, 3))
  expect_true("clonotype" %in% colnames(x))
  expect_true("clonotype_size" %in% colnames(x))
})

x <- d %>% clonotype_size(keep = c("barcode", "chain.a"))
test_that("clonotype_size can customize returned columns", {
  expect_false(is.null(x))
  expect_equal(dim(x), c(3, 4))
  expect_true("clonotype" %in% colnames(x))
  expect_true("clonotype_size" %in% colnames(x))
})

x <- d %>% clonotype_size(keep = everything())
test_that("clonotype_size can get all columns", {
  expect_false(is.null(x))
  expect_equal(dim(x), c(3, 13))
  expect_true("clonotype" %in% colnames(x))
  expect_true("clonotype_size" %in% colnames(x))
})
