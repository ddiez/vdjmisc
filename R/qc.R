#' compute_vdj_qc
#'
#' Calculates some qc values for VDJ data including validity (cells must have at least 1 TRA and 1 TRB chain) and
#' doublet based on the maximum number of TRA and TRB chains (default 1 for both).
#'
#' @param x a data.frame with VDJ information.
#' @param barcode.col which column use as barcode or index.
#' @param chain.col which column contians the chain information.
#' @param max.alpha maximum number of alpha chains for singletons.
#' @param max.beta maximum number of beta chains for singletons.
#' @param drop_multi drop chain "Multi" from 10x VDJ data.
#'
#' @return a data.frame containing QC information for each barcode/chain.
#' @export
#'
compute_vdj_qc <- function(x, barcode.col= "barcode", chain.col = "chain", max.alpha = 1, max.beta = 1, drop_multi = TRUE) {
  if (drop_multi)
    x <- x %>% filter(.data[["chain"]] != "Multi")

  doublet <- x %>% group_by_at(.vars = c(barcode.col, chain.col)) %>% summarize(alleles = n())
  doublet <- doublet %>% spread(.data[["chain"]], .data[["alleles"]], fill = 0)

  doublet <- doublet %>%
    mutate(paired = ifelse(.data[["TRA"]] != 0 & .data[["TRB"]] != 0, TRUE, FALSE))

  doublet %>%
    mutate(doublet = ifelse(.data[["TRA"]] > max.alpha | .data[["TRB"]] > max.beta, TRUE, FALSE))
}
