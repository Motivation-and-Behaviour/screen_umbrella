#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param plot
#' @param plot_params
#' @return
#' @author Taren Sanders
#' @export
save_plot <- function(plot, plot_params) {
  file_name <- plot_params$filename

  ggsave(
    filename = file_name,
    plot = plot,
    width = plot_params$dims[[1]],
    height = plot_params$dims[[2]],
    dpi = 320
  )

  return(file_name)
}
