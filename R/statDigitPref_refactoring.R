# ref.:

# M. E. Stokes, C. S. Davis and G. G. Koch
# Categorical Data Analysis Using the SAS System.
# 3rd Edition.  Cary, NC: SAS Institute, Inc.

# Kari Kuulasmaa, Hans-Werner Hense and Hanna Tolonen
# Quality Assessment of Data on Blood Pressure in the WHO MONICA Project
# MONICA Memo, May 1998

# Installation of the function: gsm.digitpref
pak::pak("OpenRBQM/gsm.digitpref")

# Load necessary libraries
library(dplyr)
library(gsm.digitpref)
library(pharmaversesdtm)
library(stats)
library(tidyr)

#' Process data and define vectors
#'
#' @param data Laboratory measurements data
#' @param dm Demography data
#' @return A list containing vectors for siteID, testName, and digitToAnalyze
process_data <- function(data, dm) {
  data <- dplyr::left_join(data, dplyr::select(dm, USUBJID, SITEID), by = "USUBJID")

  siteID <- as.vector(data$SITEID)
  testName <- as.vector(data$LBTEST)
  digitToAnalyze <- gsm.digitpref::splitByPlace(data$LBSTRESN, 0, 0) %>%
    unlist() %>%
    as.vector()

  data$digit_to_analyse <- digitToAnalyze
  data <- dplyr::select(data, 6, 13, 14, 24, 25)

  return(list(siteID = siteID, testName = testName, digitToAnalyze = digitToAnalyze))
}

#' Calculate statistics for digit preference
#'
#' @param siteID Vector of Study Site Identifiers
#' @param testName Vector of Lab Tests
#' @param digitToAnalyze Vector of digits to analyze
#' @param method Statistical test method: "chsq.test" or "cmh.test"
#' @return Data frame with statistical results
statDigitPref <- function(siteID, testName, digitToAnalyze, method) {
  df <- create_dataframe(siteID, testName, digitToAnalyze)
  df <- calculate_frequencies(df)
  frequTab <- prepare_frequency_table(df, siteID, testName)

  if (method == "chsq.test") {
    outputStat <- perform_chsq_test(frequTab)
  } else if (method == "cmh.test") {
    outputStat <- perform_cmh_test(frequTab)
  } else {
    stop("Unknown method, available methods: 'chsq.test' and 'cmh.test'.")
  }

  return(outputStat)
}

#' Create data frame from input vectors
#'
#' @param siteID Vector of Study Site Identifiers
#' @param testName Vector of Lab Tests
#' @param digitToAnalyze Vector of digits to analyze
#' @return Data frame
create_dataframe <- function(siteID, testName, digitToAnalyze) {
  df <- data.frame(siteID, testName, digitToAnalyze = as.numeric(digitToAnalyze))
  return(df)
}

#' Calculate frequencies of the digits
#'
#' @param df Input data frame
#' @return Data frame with frequencies and percentages
calculate_frequencies <- function(df) {
  df <- df %>%
    dplyr::group_by(siteID, testName, digitToAnalyze) %>%
    dplyr::mutate(freqSITE = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(siteID, testName) %>%
    dplyr::mutate(n_2 = sum(freqSITE), percentSITE = freqSITE / n_2 * 100) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(testName, digitToAnalyze) %>%
    dplyr::mutate(n_3 = dplyr::n()) %>%
    dplyr::group_by(testName) %>%
    dplyr::mutate(n_4 = sum(n_3), freqNotSITE = n_3 - freqSITE) %>%
    dplyr::group_by(siteID, testName) %>%
    dplyr::mutate(n_6 = sum(freqNotSITE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(percentNotSITE = freqNotSITE / n_6 * 100) %>%
    dplyr::select(-n_2, -n_3, -n_4, -n_6) %>%
    dplyr::distinct()
  return(df)
}

#' Prepare frequency table
#'
#' @param df Input data frame
#' @param siteID Vector of Study Site Identifiers
#' @param testName Vector of Lab Tests
#' @return Data frame with prepared frequency table
prepare_frequency_table <- function(df, siteID, testName) {
  tests_list <- unique(testName)
  sites_list <- unique(siteID)
  digit_list <- 0:9
  list <- expand.grid(siteID = sites_list, testName = tests_list, digitToAnalyze = digit_list)
  frequTab <- list %>%
    dplyr::left_join(df, by = c("siteID", "testName", "digitToAnalyze")) %>%
    tidyr::replace_na(list(freqSITE = 0, freqNotSITE = 0, percentSITE = 0, percentNotSITE = 0)) %>%
    dplyr::group_by(siteID, testName) %>%
    dplyr::filter(sum(freqSITE) > 0) %>%
    dplyr::ungroup() %>%
    prepare_site_not_site_table()
  return(frequTab)
}

#' Prepare site and not-site table
#'
#' @param frequTab Input data frame
#' @return Data frame with combined site and not-site table
prepare_site_not_site_table <- function(frequTab) {
  siteTab <- frequTab %>%
    dplyr::select(siteID, testName, digitToAnalyze, Count = freqSITE, Percent = percentSITE) %>%
    dplyr::mutate(siteFlag = "Site")
  nonsiteTab <- frequTab %>%
    dplyr::select(siteID, testName, digitToAnalyze, Count = freqNotSITE, Percent = percentNotSITE) %>%
    dplyr::mutate(siteFlag = "NotSite")
  frequTab <- dplyr::bind_rows(siteTab, nonsiteTab) %>%
    dplyr::arrange(siteID, testName, siteFlag, digitToAnalyze)
  return(frequTab)
}

#' Perform chi-square test
#'
#' @param frequTab Input data frame
#' @return Data frame with chi-square test results
perform_chsq_test <- function(frequTab) {
  outputStat <- frequTab %>%
    dplyr::group_by(siteID, testName) %>%
    dplyr::summarize(
      observed = sum(Count),
      statistic = stats::chisq.test(Count, siteFlag)$statistic,
      p_value = stats::chisq.test(Count, siteFlag)$p.value,
      dps = round(100 * sqrt(statistic / observed * 9), 2),
      dpsClas = dplyr::case_when(
        dps < 8 ~ "Excellent",
        dps < 12 ~ "Good",
        dps < 20 ~ "Acceptable",
        TRUE ~ "Problematic"
      )
    )
  return(outputStat)
}

#' Perform Cochran-Mantel-Haenszel test
#'
#' @param frequTab Input data frame
#' @return Data frame with Cochran-Mantel-Haenszel test results
perform_cmh_test <- function(frequTab) {
  transformedTab <- frequTab %>%
    tidyr::pivot_wider(names_from = siteFlag, values_from = c(Count, Percent),
                       names_glue = "{.value}_{siteFlag}") %>%
    dplyr::rename(Count_site = Count_Site,
                  Percent_site = Percent_Site,
                  Count_others = Count_NotSite,
                  Percent_others = Percent_NotSite) %>%
    dplyr::arrange(siteID, testName, digitToAnalyze) %>%
    calculate_initial_values()

  detailed_result <- calculate_detailed_values(transformedTab)
  summary_result <- summarize_values(detailed_result)

  outputStat <- summary_result %>%
    dplyr::mutate(
      dps = round(100 * sqrt(statistic / observed * 9), 2),
      dpsClas = dplyr::case_when(
        dps < 8 ~ "Excellent",
        dps < 12 ~ "Good",
        dps < 20 ~ "Acceptable",
        TRUE ~ "Problematic"
      )
    )
  return(outputStat)
}






