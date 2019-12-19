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
#' @param check_valid logical; whether to check for validity.
#'
#' @return a data.frame containing QC information for each barcode/chain.
#' @export
#'
compute_vdj_qc <- function(x, barcode.col= "barcode", chain.col = "chain", max.alpha = 1, max.beta = 1, drop_multi = TRUE, check_valid = TRUE) {
  if (drop_multi)
    x <- x %>% filter(chain != "Multi")

  doublet <- x %>% group_by_at(.vars = c(barcode.col, chain.col)) %>% summarize(alleles = n())
  doublet <- doublet %>% spread(chain, alleles, fill = 0)

  if (check_valid)
    doublet <- doublet %>%
    mutate(valid = ifelse(TRA != 0 & TRB != 0, TRUE, FALSE))

  doublet %>%
    mutate(doublet = ifelse(TRA > max.alpha | TRB > max.beta, TRUE, FALSE))
}
