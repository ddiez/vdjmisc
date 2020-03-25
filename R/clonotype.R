#' clonotype_size
#'
#' @param x a tidy VDJ object
#' @param keep keep this column(s) when reporting clonotype sizes.
#' @note This function now calls add_clonotype_id() and add_clonotype_size(). Kepts for compatibility reason. Will deprecate.
#'
#' @export
clonotype_size <- function(x, keep = "barcode") {
  x %>% add_clonotype_id(name = "clonotype") %>%
    add_clonotype_size(by = "clonotype") %>%
    select(keep, "clonotype", "clonotype_size")
}


#' add_clonotype_id
#'
#' @param x a tidy VDJ object
#' @param name name of the column to store clonotype id.
#'
#' @export
add_clonotype_id <- function(x, name = "clonotype_id") {
  x %>% mutate(
    !!name := paste(
      .data$c_gene.a,
      .data$j_gene.a,
      .data$d_gene.a,
      .data$v_gene.a,
      .data$cdr3.a,
      .data$cdr3.b,
      .data$v_gene.b,
      .data$d_gene.b,
      .data$j_gene.b,
      .data$c_gene.b,
      sep = "_"))
}

#' add_clonotype_size
#'
#' @param x a tidy VDJ object
#' @param by name of the column containing clonotype ids.
#' @param name name of the column to store clonotype size.
#'
#' @export
add_clonotype_size <- function(x, by = "clonotype_id", name = "clonotype_size") {
  x %>%
    add_count(.data[[by]], name = name) %>%
    arrange(desc(.data[[name]]))
}
