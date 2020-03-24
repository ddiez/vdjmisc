#' read_vdj_json
#'
#' Parses consensus_annotations.json from 10x VDJ.
#'
#' @param filename path to consensus_annotations.json file.
#'
#' @export
read_vdj_json <- function(filename) {
  x <- jsonlite::read_json(filename)
  parse_vdj_json(x)
}

parse_vdj_json <- function(x) {
  bind_rows(lapply(x, function(xx) {
    barcode <- unlist(xx$info$cells)

    annotations <- parse_annotations(xx$annotations)
    chain <- unique(annotations$chain)
    genes <- tidy_annotations(annotations)
    tibble(barcode = barcode,
           chain = chain,
           clonotype = xx$clonotype,
           cdr3 = xx$cdr3,
           cdr3_seq = xx$cdr3_seq,
           filtered = xx$filtered,
           high_confidence = xx$high_confidence,
           productive = xx$productive,
           v_gene = genes[["v_gene"]],
           j_gene = genes[["j_gene"]],
           c_gene = genes[["c_gene"]],
           d_gene = genes[["d_gene"]],
           sequence = xx$sequence)
  }))
}


parse_annotations <- function(x) {
  bind_rows(lapply(x, function(xx) {
    tibble(
      chain = xx$feature$chain,
      feature_id = xx$feature$feature_id,
      display_name = xx$feature$display_name,
      gene_name = xx$feature$gene_name,
      region_type = xx$feature$region_type
    )
  }))
}


find_or_na <- function(x, what) {
  x <- x %>% filter(.data$region_type == !!what)
  if (nrow(x) == 1)
    x %>% pull("gene_name")
  else
    NA
}

tidy_annotations <- function(x) {
  chain <- unique(x$chain)
  v_gene <- find_or_na(x, "L-REGION+V-REGION")
  j_gene <- find_or_na(x, "J-REGION")
  c_gene <- find_or_na(x, "C-REGION")
  d_gene <- find_or_na(x, "D-REGION")
  tibble(v_gene = v_gene, j_gene = j_gene, c_gene = c_gene, d_gene = d_gene)
}


