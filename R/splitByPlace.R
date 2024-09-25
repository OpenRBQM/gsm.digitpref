#' Split a vector into a table of place values
#'
#' Generate a table of places (by power of 10, with column names such as "10^2"
#' and "10^-2") from a vector of numeric values.
#'
#' @inheritParams rlang::args_error_context
#' @param x `numeric` A vector of values to evaluate.
#' @param max_power_10 `integer` The highest 10s place to check, as a power of
#'   10. This will set the first column of the returned results.
#' @param min_power_10 `integer` The highest 10s place to check, as a power of
#'   10. Must be lower than `max_power_10`. This will set the first column of
#'   the returned results.
#' @param x_arg `character` The name of `x` to use in the output table and error
#'   messages. By default this is automatically deteermined from the calling
#'   context, and you can likely leave it as-is.
#'
#' @return A [tibble::tibble()] with a column for the original vector (named
#'   using `x_arg`) and columns for each power of 10 between `max_power_10` and
#'   `min_power_10`.
#' @export
#' @examples
#' random_data <- runif(10, 0, 1000)
#' splitByPlaces(random_data)
#' splitByPlaces(random_data, 2, -5)
#' splitByPlaces(random_data, 2, -5, "measurements")
splitByPlaces <- function(x,
                          max_power_10 = 2,
                          min_power_10 = -2,
                          x_arg = rlang::caller_arg(x),
                          call = rlang::caller_env()) {
  lPlaces <- makePlacesList(x, max_power_10:min_power_10, x_arg, call)
  tibble::tibble(
    !!x_arg := showDigits(x, min_power_10),
    !!!lPlaces
  )
}

#' Extract a 10s place
#'
#' @inheritParams rlang::args_error_context
#' @param mHalves `matrix` A 2-column character matrix of numbers split on the
#'   decimal point.
#' @param place The 10s place to return.
#'
#' @return An integer vector of digits from the numbers, at the given place.
#' @keywords internal
extractPlace <- function(mHalves, place, call = rlang::caller_env()) {
  if (
    length(place) != 1 || is.na(place) ||
    !is.numeric(place) || as.integer(place) != place
  ) {
    cli::cli_abort(
      "{.arg place} must be an integer.",
      class = error_class("bad_place"),
      call = call
    )
  }
  if (place < 0) {
    chrX <- mHalves[,2]
    place <- -1L * place
  } else {
    chrX <- stringi::stri_reverse(mHalves[,1])
    place <- place + 1L
  }
  as.integer(substr(chrX, place, place))
}

#' Set up display of digits for a column in a tibble
#'
#' @param x `numeric` The vector to display.
#' @param min_power_10 `integer` The minimum power of 10 to focus on. We show
#'   one place past this value, to avoid confusion from rounding.
#'
#' @return A special numeric vector class from [tibble::num()] to display the number
#' @keywords internal
showDigits <- function(x, min_power_10) {
  digits <- ifelse(min_power_10 <= 0, -1 * min_power_10 + 1, 0)
  tibble::num(x, digits = digits, label = "<dbl>", notation = "dec")
}

#' Divide a vector at a decimal point
#'
#' @inheritParams rlang::args_error_context
#' @param x `numeric` A vector of values to split.
#'
#' @return A 2-column character matrix, where the first column contains
#'   digits before the decimal point, and the second column contains digits
#'   after the decimal point (or `NA` if there weren't any decimal points).
#' @keywords internal
numToChrHalves <- function(x,
                           x_arg = rlang::caller_arg(x),
                           call = rlang::caller_env()) {
  if (!is.numeric(x)) {
    cli::cli_abort(
      "{.arg {x_arg}} must be a vector of numbers.",
      class = error_class("non_number")
    )
  }
  chrX <- as.character(x)
  mHalves <- stringi::stri_split_regex(chrX, "\\.", simplify = TRUE)
  if (ncol(mHalves) == 1) {
    mHalves <- cbind(mHalves, NA)
  }
  mHalves
}

#' Split a vector into a named list of place values
#'
#' This is the workhorse behind [splitByPlaces()].
#'
#' @inheritParams splitByPlaces
#' @param intPlaces `integer` A vector of 10's places to include, from highest
#'   10's place to lowest.
#'
#' @return A named list of vectors of digits. Each element of the list contains
#'   a value for each value of `x`, at that 10's place (given in the name of
#'   that element).
#' @keywords internal
makePlacesList <- function(x,
                           intPlaces,
                           x_arg = rlang::caller_arg(x),
                           call = rlang::caller_env()) {
  mHalves <- numToChrHalves(x, x_arg, call)
  rlang::set_names(
    purrr::map(
      intPlaces,
      function(place) {
        extractPlace(mHalves, place, call)
      }
    ),
    paste0("10^", intPlaces)
  )
}
