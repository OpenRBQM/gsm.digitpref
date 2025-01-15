#' Plot Digit Distribution for a Place by Group
#'
#' Create a stacked bar chart of digit distribution, with a grouping variable on
#' the x axis, and the distribution of digits in a particular digit place on the
#' y axis.
#'
#' @inheritParams shared-params
#' @export
#' @examples
#' sample_data <- tibble::tibble(
#'   siteID = sample(c("siteA", "siteB", "siteC"), 1000, replace = TRUE),
#'   labResults = runif(1000)*10
#' )
#' plotPlaceDigits(sample_data, labResults, siteID, -2)
plotPlaceDigits <- function(
    dfData,
    colData,
    colGroup,
    power_10,
    strGroupLabel = rlang::ensym(colGroup),
    strPlaceLabel = powerToOrdinal(power_10, strAfter = "Place"),
    strChartTitle = glue::glue(
      "Digit Distribution for {strPlaceLabel} of Data by {strGroupLabel}"
    ),
    scaleDigitPalette = ggplot2::scale_fill_brewer(palette = "Set3"),
    themePlot = ggplot2::theme_bw()
) {
  placeDigits <- splitByPlace(
    unique(dplyr::pull(dfData, {{ colData }})),
    max_power_10 = power_10,
    min_power_10 = power_10,
    x_arg = rlang::ensym(colData)
  )

  dfDigits <- dplyr::left_join(
    dfData,
    placeDigits,
    by = dplyr::join_by({{ colData }})
  )

  plotDigitCounts(
    dfDigits,
    colPlace = paste0("10^", power_10),
    colGroup = {{ colGroup }},
    strGroupLabel = strGroupLabel,
    strPlaceLabel = strPlaceLabel,
    strChartTitle = strChartTitle,
    scaleDigitPalette = scaleDigitPalette,
    themePlot = themePlot
  )
}
