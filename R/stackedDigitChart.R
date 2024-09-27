#' Plot digit distribution by group
#'
#' Create a stacked bar chart of digit distribution, with a grouping variable on
#' the x axis, and the distribution of digits in a particular digit place on the
#' y axis.
#'
#' @param dfDigits `data.frame` A data.frame with a grouping column and at least
#'   one column for a digit place.
#' @param colGroup <[`data-masked`][dplyr::dplyr_data_masking]> The column to
#'   group by.
#' @param colPlace <[`data-masked`][dplyr::dplyr_data_masking]> The column with
#'   the digit place to be plotted.
#' @param strGroupLabel `character` An optional name to use for the grouping
#'   column in labels.
#' @param strPlaceLabel `character` An optional name to use for the place column
#'   in labels.
#' @param strChartTitle `character` An optional title for the chart.
#' @param scaleDigitPalette An optional [ggplot2::scale_fill_discrete()] palette
#'   to use for the digits.
#' @param themePlot An optional [ggplot2::theme()] to apply to the plot.
#'
#' @export
#' @examples
#' sample_data <- data.frame(
#'   siteID = sample(c("siteA", "siteB", "siteC"), 1000, replace = TRUE),
#'   onesPlace = sample(0:9, 1000, replace = TRUE)
#' )
#' stackedDigitChart(sample_data, siteID, onesPlace)
#' stackedDigitChart(sample_data, siteID, onesPlace, "Site", "One Place")
#' stackedDigitChart(sample_data, siteID, onesPlace, scaleDigitPalette = NULL)
stackedDigitChart <- function(
    dfDigits,
    colGroup,
    colPlace,
    strGroupLabel = rlang::ensym(colGroup),
    strPlaceLabel = rlang::ensym(colPlace),
    strChartTitle = glue::glue(
      "Digit Distribution for {strPlaceLabel} of Data by {strGroupLabel}"
    ),
    scaleDigitPalette = ggplot2::scale_fill_brewer(palette = "Set3"),
    themePlot = ggplot2::theme_bw()
) {
  ggplot2::ggplot(
    data = dfDigits,
    ggplot2::aes(
      x = {{ colGroup }},
      fill = factor(as.character({{ colPlace }}), levels = 0:9)
    )
  ) +
    ggplot2::geom_bar(position = "fill") +
    scaleDigitPalette +
    ggplot2::labs(
      title = as.character(strChartTitle),
      x = strGroupLabel,
      y = "Frequency",
      fill = "Digit"
    ) +
    themePlot +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
}
