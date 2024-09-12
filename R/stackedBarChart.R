#' Function to plot a stacked bar chart for digit distribution by site
#'
#' @param digit_data a tibble with one column for siteID, and one column for each digit place
#' @param col_siteID the column name for siteID
#' @param col_digit the column name for the digit place to be plotted
#'
#' @importFrom ggplot2
#'
#' @export


stackedBarChart <- function(digit_data, col_siteID, col_digit){
  ggplot(data=digit_data, aes_string(x = col_siteID, fill = col_digit)) +
    geom_bar(position = 'fill') +
    scale_fill_brewer(palette = 'Set3') +
    labs(
      title = "Digit Distribution of Simulation Data by Site",
      x = "SiteID",
      y = "Frequency",
      fill = "Digit"
    ) +
    theme_bw() +
    theme(axis.text.x = element_text(angle=90))
}
