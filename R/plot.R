# plot_vdj_usage <- function(x) {
#   UseMethod("plot_vdj_usaje")
# }

#' plot_vdj_usage
#'
#' Plot V/J gene usage in alpha/beta chains
#'
#' @param x data.frame
#' @param cols columns to include.
#' @param split.by combine and split the vdj plot by this columns.
#' @param title main title.
#' @param subtitle subtitlte.
#' @param size size of text in gene boxes.
#' @param alpha alpha of gene boxes.
#' @param fill fill color of gene boxes.
#' @param fill.vdj fill color of arrows.
#' @param axis.width width of gene boxes.
#' @param expand.x expand x axis by this amount.
#' @param expand.y expand y axis by this amount.
#'
#' @export
#'
plot_vdj_usage <- function(x, cols = NULL, split.by = NULL, title = "", subtitle = paste(nrow(x), "sequences"), size = 3, alpha = .4, fill = "violetred", fill.vdj = "limegreen", axis.width = .3, expand.x = 0.1, expand.y = 5) {

  if (is.null(split.by)) {
    plot_vdj_usage_raw(x, cols = cols, title = title, subtitle = subtitle, size = size, alpha = alpha, fill = fill, fill.vdj = fill.vdj, axis.width = axis.width, expand.x = expand.x, expand.y = expand.y)
  } else {
    x <- x %>% unite(".group", split.by, sep = "-")
    x <- split(x, x[[".group"]])
    pl <- lapply(names(x), function(n) {
      plot_vdj_usage_raw(x[[n]], cols = cols, title = n, subtitle = subtitle, size = size, alpha = alpha, fill = fill, fill.vdj = fill.vdj, axis.width = axis.width, expand.x = expand.x, expand.y = expand.y)
    })
    wrap_plots(pl)
  }
}

plot_vdj_usage_raw <- function(x, cols = NULL, title = "", subtitle = paste(nrow(x), "sequences"), size = 3, alpha = .4, fill = "violetred", fill.vdj = "limegreen", axis.width = .3, expand.x = 0.1, expand.y = 5) {
  get_parallel_vdj(x, cols) %>%
    ggplot(
      aes(x = .data$x, split = .data$y, value = .data$n, id = .data$id)) +
    geom_parallel_sets(fill = fill, alpha = alpha) +
    geom_parallel_sets_axes(axis.width = axis.width, fill = fill.vdj) +
    geom_parallel_sets_labels(size = size, angle = 0) +
    scale_x_discrete(expand = c(0, expand.x), labels = c("j_gene.a" = expression("J"~alpha), "v_gene.a" = expression("V"~alpha), "v_gene.b" = expression("V"~beta), "j_gene.b" = expression("J"~beta))) +
    scale_y_discrete(expand = c(0, expand.y)) +
    labs(x = "", y = "", title = title, subtitle = subtitle)
}


get_parallel_vdj <- function(x, cols = NULL) {
  cols <- c(c("j_gene.a", "v_gene.a", "v_gene.b", "j_gene.b"), cols)
  d <- x %>% group_by_at(cols) %>%
    summarize(n = n()) %>%
    gather_set_data(cols)

  d %>% mutate(x = factor(x, cols))
}
