#' Plot digit distribution by group
#'
#' Create a stacked bar chart of digit distribution, with a grouping variable on
#' the x axis, and the distribution of digits in a particular digit place on the
#' y axis.
#'
#' @param digit_data A data.frame with a grouping column and at least one column
#'   for a digit place.
#' @param col_group <[`data-masked`][dplyr::dplyr_data_masking]> The column to
#'   group by.
#' @param col_digit <[`data-masked`][dplyr::dplyr_data_masking]> The column with
#'   the digit place to be plotted.
#' @param group_label An optional name to use for the grouping column in labels.
#' @param chart_title An optional title for the chart.
#' @param digit_palette An optional fill palette to use for the digits.
#' @param plot_theme An optional theme to apply to the plot.
#'
#' @export
#' @examples
#' sample_data <- data.frame(
#'   siteID = sample(c("siteA", "siteB", "siteC"), 1000, replace = TRUE),
#'   onesDigit = sample(0:9, 1000, replace = TRUE)
#' )
#' stackedDigitChart(sample_data, siteID, onesDigit)
#' stackedDigitChart(sample_data, siteID, onesDigit, "Site")
#' stackedDigitChart(sample_data, siteID, onesDigit, digit_palette = NULL)
stackedDigitChart <- function(
    digit_data,
    col_group,
    col_digit,
    group_label = rlang::ensym(col_group),
    chart_title = glue::glue(
      "Digit Distribution of Data by {group_label}"
    ),
    digit_palette = ggplot2::scale_fill_brewer(palette = "Set3"),
    plot_theme = ggplot2::theme_bw()
) {
  ggplot2::ggplot(
    data = digit_data,
    ggplot2::aes(
      x = {{ col_group }},
      fill = factor(as.character({{ col_digit }}), levels = 0:9)
    )
  ) +
    ggplot2::geom_bar(position = "fill") +
    digit_palette +
    ggplot2::labs(
      title = as.character(chart_title),
      x = group_label,
      y = "Frequency",
      fill = "Digit"
    ) +
    plot_theme +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90))
}
