context("tidy_vdj_10x")

d <- data.frame(barcode = c("bc1", "bc1", "bc2", "bc2", "bc2", "bc3", "bc4", "bc5", "bc5"))
d$chain <- c("TRA", "TRB", "TRA", "TRB", "TRB", "TRA", "TRB", "TRA", "TRA")
d$c_gene <- c("TRAC1", "TRBC1", "TRAC1", "TRBC2", "TRBC2", "TRAC1", "TRBC1", "TRAC2", "TRAC1")
d$v_gene <- c("TRAV1", "TRBV1", "TRAV1", "TRBV2", "TRBV3", "TRAV2", "TRBV1", "TRAV2", "TRAV3")
d$j_gene <- c("TRAJ1", "TRBJ2", "TRAJ2", "TRBJ2", "TRBJ1", "TRAJ2", "TRBJ2", "TRAJ2", "TRAJ5")

tidy_vdj <- tidy_vdj_10x(d)

test_that("tidy_vdj_10x works", {
  expect_equal(dim(tidy_vdj), c(3, 9))
})

