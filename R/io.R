#' read_vdj_10x
#'
#' Read a 10x VDJ file.
#'
#' @param filename filename.
#' @param productive.only logical; whether to filter out non-productive rows.
#'
#' @export
#'
read_vdj_10x <- function(filename, productive.only = TRUE) {
  vdj <- read_csv(filename)

  if (! is.logical(vdj$productive)) {
    vdj$productive <- as.logical(vdj$productive)
  }

  if (productive.only)
    vdj <- vdj %>% filter(.data$productive)
  vdj
}

#' read_vdj_10x_batch
#'
#' Read a batch of 10x VDJ files from a folder.
#'
#' @param path path name to the folder.
#' @param extension extension of the 10x VDJ files.
#' @param productive.only logical; whether to filter out non-productive rows.
#'
#' @export
#'
read_vdj_10x_batch <- function(path = ".", extension = "csv", productive.only = TRUE) {
  f <- dir_ls(path, regexp = paste0(extension, "$"))
  vdj <- lapply(f, read_vdj_10x, productive.only = productive.only)
  names(vdj) <- f
  bind_rows(vdj, .id = "filename")
}


#' read_vdjdb_full
#'
#' @param filename filename.
#' @param min.score if not NULL, return only entries with this score or greater.
#' @param paired return only entries with paired (alpha and beta) VJ information.
#'
#' @export
#'
read_vdjdb_full <- function(filename, min.score = NULL, paired = TRUE) {
  vdj <- read_tsv(filename)

  vdj <- vdj %>% rename(
    cdr3.a = .data$cdr3.alpha,
    v_gene.a = .data$v.alpha,
    j_gene.a = .data$j.alpha,
    cdr3.b = .data$cdr3.beta,
    v_gene.b = .data$v.beta,
    j_gene.b = .data$j.beta
  )

  if (!is.null(min.score))
    vdj <- vdj %>% filter(.data$vdjdb.score >= min.score)

  #vdj <- vdj %>% select("cdr3.a", "v_gene.a", "j_gene.a", "cdr3.b", "v_gene.b", "j_gene.b")

  if (paired)
    vdj <- vdj %>% tidyr::drop_na("v_gene.a", "j_gene.a", "v_gene.b", "j_gene.b")

  vdj
}
