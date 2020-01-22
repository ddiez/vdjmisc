#' clonotype_size
#'
#' @param x a tidy VDJ object
#'
#' @export
#'
clonotype_size <- function(x, by = "barcode") {
  x %>%
    mutate(clonotype = paste(v_gene.a, d_gene.a, j_gene.a, cdr3.a, cdr3.b, v_gene.b, d_gene.b, j_gene.b, sep = "_")) %>%
    add_count(clonotype) %>%
    arrange(desc(n)) %>%
    select(by, clonotype, clonotype_size = n)
}

