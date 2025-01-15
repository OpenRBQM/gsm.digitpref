#' Parameters used in multiple functions
#'
#' Reused parameter definitions are gathered here for easier usage.
#'
#' @param colData <[`data-masked`][dplyr::dplyr_data_masking]> A data that
#'   contains numeric data.
#' @param colGroup <[`data-masked`][dplyr::dplyr_data_masking]> A column to
#'   group by.
#' @param colPlace <[`data-masked`][dplyr::dplyr_data_masking]> A column with
#'   integer values between 0 and 9.
#' @param dfData `data.frame` A data.frame with a grouping column and at least
#'   one numeric data column.
#' @param dfDigits `data.frame` A data.frame with a grouping column and at least
#'   one column for a digit place.
#' @param max_power_10 `integer` The highest 10s place to check, as a power of
#'   10.
#' @param min_power_10 `integer` The lowest 10s place to check, as a power of
#'   10. Must be lower than `max_power_10`.
#' @param power_10 `integer` The 10s place to check, as a power of 10.
#' @param scaleDigitPalette An optional [ggplot2::scale_fill_discrete()] palette
#'   to use for the digits.
#' @param strGroupLabel `character` An optional name to use for the grouping
#'   column in labels.
#' @param strPlaceLabel `character` An optional name to use for the place column
#'   in labels.
#' @param strChartTitle `character` An optional title for the chart.
#' @param strCase `character` Whether to display text in "Title" case, "lower"
#'   case, or "UPPER" case.
#' @param themePlot An optional [ggplot2::theme()] to apply to the plot.
#' @param x `numeric` A vector of values to evaluate.
#' @param x_arg `character` The name of `x` to use in the output table and error
#'   messages. By default this is automatically determined from the calling
#'   context, and you can likely leave it as-is.
#'
#' @name shared-params
#' @keywords internal
NULL
