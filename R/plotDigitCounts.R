#' Plot digit distribution by group
#'
#' Create a stacked bar chart of digit distribution, with a grouping variable on
#' the x axis, and the distribution of digits in a particular digit place on the
#' y axis.
#'
#' @inheritParams shared-params
#' @export
#' @examples
#' sample_data <- data.frame(
#'   siteID = sample(c("siteA", "siteB", "siteC"), 1000, replace = TRUE),
#'   onesPlace = sample(0:9, 1000, replace = TRUE)
#' )
#' plotDigitCounts(sample_data, "siteID", "onesPlace")
#' plotDigitCounts(sample_data, "siteID", "onesPlace", "Site", "One Place")
#' plotDigitCounts(sample_data, "siteID", "onesPlace", scaleDigitPalette = NULL)
plotDigitCounts <- function(
    dfDigits,
    colGroup,
    colPlace,
    strGroupLabel = colGroup,
    strPlaceLabel = colPlace,
    strChartTitle = glue::glue(
      "Digit Distribution for {strPlaceLabel} of Data by {strGroupLabel}"
    ),
    scaleDigitPalette = ggplot2::scale_fill_brewer(palette = "Set3"),
    themePlot = ggplot2::theme_bw()
) {
  ggplot2::ggplot(
    data = dfDigits,
    ggplot2::aes(
      x = .data[[colGroup]],
      fill = factor(as.character(.data[[colPlace]]), levels = 9:0)
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
