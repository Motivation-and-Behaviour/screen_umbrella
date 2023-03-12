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
save_table <- function(table, filename) {
  if (Sys.info()["sysname"] == "Linux") {
    # PhantomJS runs into issues without this
    Sys.setenv("OPENSSL_CONF" = "/dev/null")
  }

  # Note: this only works with gt 0.6.0 (webshot not webshot2)
  gtsave(
    data = table,
    filename = filename,
    zoom = 1
  )

  return(filename)
}
