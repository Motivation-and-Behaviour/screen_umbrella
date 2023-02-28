#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param table Must be of type
#' @param filename
#' @return
#' @author Taren Sanders
#' @export
save_tables <- function(table, filename) {
  file_path <- here::here("tables", filename)

  gtsave(
    data = table,
    filename = file_path,
    zoom = 1
  )

  return(file_path)
}
