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
  if (productive.only)
    vdj <- vdj %>% filter(productive == "True")
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
