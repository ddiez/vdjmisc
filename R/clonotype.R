#' clonotype_size
#'
#' @param x a tidy VDJ object
#' @param keep keep this column(s) when reporting clonotype sizes.
#'
#' @export
#'
clonotype_size <- function(x, keep = "barcode") {
  x %>%
    mutate(clonotype = paste(.data$v_gene.a, .data$d_gene.a, .data$j_gene.a, .data$cdr3.a, .data$cdr3.b, .data$v_gene.b, .data$d_gene.b, .data$j_gene.b, sep = "_")) %>%
    add_count(.data$clonotype) %>%
    arrange(desc(n)) %>%
    select(keep, .data$clonotype, clonotype_size = .data$n)
}

