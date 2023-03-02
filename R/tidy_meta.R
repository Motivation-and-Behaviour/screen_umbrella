#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param meta_results
#' @return
#' @author Taren Sanders
#' @export
tidy_meta <- function(meta_results) {
  df <- broom::tidy(meta_results, conf.int = TRUE)
  df_999 <-
    broom::tidy(meta_results, conf.int = TRUE, conf.level = 0.999) %>%
    mutate(
      conf.low = if_else(conf.low < -1, -1, conf.low),
      conf.high = if_else(conf.high > 1, 1, conf.high)
    )

  df$conf.low_999 <- df_999$conf.low
  df$conf.high_999 <- df_999$conf.high

  # Add extras not provided by broom
  df$k <- meta_results$k
  df$i2 <- meta_results$I2
  df$n <- sum(meta_results$ni)
  df$n_max <- max(meta_results$ni)
  df$effect_size_id <- meta_results$effect_size_id

  return(df)
}
