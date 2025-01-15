#' Convert a Power of Ten to Words
#'
#' This convenience function converts a power of ten to words, such as "Ones
#' Place" or "hundredths digit".
#'
#' @inheritParams shared-params
#' @param strAfter `character` Additional text to add after the ordinal, such as
#'   "place" or "digit".
#'
#' @returns The tens place expressed in words.
#' @export
#'
#' @examples
#' powerToOrdinal(0)
#' powerToOrdinal(3)
#' powerToOrdinal(-2)
#' powerToOrdinal(-2, strAfter = "digit")
powerToOrdinal <- function(power_10,
                           strCase = c("Title", "lower", "UPPER"),
                           strAfter = character()) {
  rlang::check_installed("english", "to convert a power of ten to an ordinal.")
  rlang::check_installed("stringr", "to convert a power of ten to an ordinal.")
  num <- 10^abs(power_10)
  word <- english::as.english(num) |>
    stringr::str_remove("^one ")
  if (power_10 < 0) {
    word <- paste0(word, "th")
  }
  word <- paste0(word, "s")
  if (length(strAfter) && nchar(strAfter)) {
    word <- paste(word, strAfter)
  }
  strCase <- match.arg(tolower(strCase), c("title", "lower", "upper"))
  switch(
    strCase,
    title = stringr::str_to_title(word),
    lower = word,
    upper = toupper(word)
  )
}
