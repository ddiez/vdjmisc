#' tidy_vdj_10x
#'
#' Tidy a 10x VDJ format so that each line contains one barcode-alpha-beta-combination.
#'
#' @param x data.frame with VDJ data.
#' @param by column to use as index.
#' @param drop.na logical; whether to drop rows with NAs.
#' @param drop.multi logical; whether to drop rows with Multi chains.
#'
#' @export
#'
tidy_vdj_10x <- function(x, by = "cell_index", drop.na = TRUE, drop.multi = TRUE) {
  if (drop.multi) {
    index <- x %>% filter(.data$chain == "Multi") %>% pull("cell_index")
    x <- x %>% filter(! .data$cell_index %in% !!index)
  }

  dA <- x %>% filter(.data$chain == "TRA")
  dB <- x %>% filter(.data$chain == "TRB")

  dA <- remove_vdj_doublets(dA)
  dB <- remove_vdj_doublets(dB)

  d <- left_join(dA, dB, by = by, suffix = c(".a", ".b"))
  if (drop.na)
    d <- d %>% drop_na("v_gene.a", "j_gene.a", "v_gene.b", "j_gene.b")

  #class(d) <- c("tidy_vdj", class(d))
  d
}

#' remove_vdj_doublets
#'
#' Remove potential VDJ doublets by making sure there is at much 2 gene variants per cell.
#'
#' @param x data.frame.
#' @param cutoff cutoff for doublets.
#'
#' @export
#'
remove_vdj_doublets <- function(x, cutoff = 1) {
  index <- x %>% count(.data$cell_index) %>% filter(n <= cutoff) %>% pull("cell_index")
  x %>% filter(.data$cell_index %in% !!index)
}

