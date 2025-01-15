error_class <- function(class) {
  c(
    "gsm.digitpref-condition",
    "gsm.digitpref-error",
    paste0("gsm.digitpref-error-", class)
  )
}

nameColumns <- function(dfData, ...) {
  all_names <- colnames(dplyr::select(dfData, ...))
  glue::glue("{all_names}")
}
