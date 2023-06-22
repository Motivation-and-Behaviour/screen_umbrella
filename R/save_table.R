#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param table Must be of type
#' @param filename
#' @param method
#' @return
#' @author Taren Sanders
#' @export
save_table <- function(table, filename, method = "webshot2") {
  if (Sys.info()["sysname"] == "Linux") {
    # PhantomJS runs into issues without this
    Sys.setenv("OPENSSL_CONF" = "/dev/null")
  }

  if (method == "webshot2") {
    gtsave(
      data = table,
      filename = filename,
      zoom = 1,
      vwidth = 3440,
      vheight = 2000,
    )
  }

  if (method == "webshot") {
    temp_path <- tempfile(fileext = ".html")
    gt:::gt_save_html(data = table, filename = temp_path, path = NULL)
    webshot::webshot(url = paste0("file:///", temp_path), file = filename)
    file.remove(temp_path)
  }

  return(filename)
}
