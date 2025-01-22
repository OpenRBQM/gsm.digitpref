error_class <- function(class) {
  c(
    "gsm.digitpref-condition",
    "gsm.digitpref-error",
    paste0("gsm.digitpref-error-", class)
  )
}
