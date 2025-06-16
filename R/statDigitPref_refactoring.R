#' Digit Preference Analysis at the Site Level for Laboratory Measurements
#'
#' Calculates digit preference score (DPS) and runs a digit preference hypothesis test
#' (Chi-square or Cochran-Mantel-Haenszel) for each site and laboratory test, based on digit frequency.
#'
#' @param siteID Character vector. Study Site Identifiers.
#' @param testName Character vector. Laboratory test names.
#' @param digitToAnalyze Integer/numeric vector. Digits to analyze for preference.
#' @param method Character. Statistical method ('chsq.test' or 'cmh.test').
#'
#' @return A tibble with summary statistics per site per test:
#'   siteID, testName, observed sample size, test statistic, p-value, DPS, DPS class.
#' @details
#'   DPS thresholds: Excellent (<8), Good (<12), Acceptable (<20), Problematic (>=20).
#'
#' @references
#'   Stokes, M. E., Davis, C. S., & Koch, G. G. (2012).
#'   Categorical Data Analysis Using the SAS System. 3rd Edition. SAS Institute.
#'   Kuulasmaa, K., Hense, H-W., & Tolonen, H. (1998). Quality Assessment of Data
#'   on Blood Pressure in the WHO MONICA Project. MONICA Memo.
#'
#' @importFrom dplyr group_by ungroup mutate summarize filter select bind_rows left_join distinct case_when n
#' @importFrom tidyr pivot_wider replace_na expand_grid
#' @importFrom stringr str_replace
#' @importFrom stats chisq.test pchisq
#' @importFrom tibble tibble
#' @export
statDigitPref <- function(siteID, testName, digitToAnalyze, method) {
  # 1. Validate Input Parameters --------------------------------------------
  validate_inputs(siteID, testName, digitToAnalyze, method)
  
  # 2. Data Preparation -----------------------------------------------------
  df <- prepare_digitpref_data(siteID = siteID, testName = testName, digitToAnalyze = digitToAnalyze)
  
  # 3. Frequency Tabulation -------------------------------------------------
  df_freq <- compute_digit_frequencies(df = df)
  
  # 4. Complete Table of All Combinations -----------------------------------
  frequTab <- expand_digitpref_combinations(
    df_freq = df_freq,
    testName = testName,
    siteID = siteID
  )
  
  # 5. Filter Groups with Data ----------------------------------------------
  frequTab <- filter_empty_digitpref_groups(frequTab = frequTab)
  
  # 6. Assemble Site/Non-site Table -----------------------------------------
  frequTab_long <- assemble_site_not_site_tables(frequTab = frequTab)
  
  # 7/8. Perform Digit Preference Test --------------------------------------
  if (method == "chsq.test") {
    outputStat <- perform_chsq_test(frequTab_long)
  } else if (method == "cmh.test") {
    outputStat <- perform_cmh_test(frequTab_long)
  } 
  return(outputStat)
}

#' 1. Validate Input Parameters for Digit Preference Analysis
#'
#' @param siteID Character vector containing site identifiers.
#' @param testName Character vector containing test names.
#' @param digitToAnalyze Numeric or integer vector containing terminal digits to analyze.
#' @param method Character string specifying the statistical method to use.
#'
#' @return No return value, called for side effects. Stops execution with error message if inputs are invalid.
#'
#' @keywords internal
validate_inputs <- function(siteID, testName, digitToAnalyze, method) {
  # Check vector lengths
  if (length(siteID) != length(testName) || length(siteID) != length(digitToAnalyze)) {
    stop("Input vectors siteID, testName, and digitToAnalyze must have the same length.")
  }
  
  # Check method parameter
  if (!method %in% c("chsq.test", "cmh.test")) {
    stop("Unknown method, available methods: 'chsq.test' and 'cmh.test'.")
  }
  
  # Check if digitToAnalyze contains valid digits
  if (!all(as.numeric(digitToAnalyze) %in% 0:9)) {
    warning("digitToAnalyze contains values outside the expected range 0-9.")
  }
  
  # Check for empty vectors
  if (length(siteID) == 0 || length(testName) == 0 || length(digitToAnalyze) == 0) {
    stop("Input vectors cannot be empty.")
  }
}


#' 2. Prepare Digit Preference Data
#'
#' @param siteID Character vector. Site identifiers.
#' @param testName Character vector. Test names.
#' @param digitToAnalyze Integer/numeric vector. Digits to analyze.
#'
#' @return A tibble with columns: siteID, testName, digitToAnalyze (as numeric).
#'
#' @importFrom tibble tibble
prepare_digitpref_data <- function(siteID, testName, digitToAnalyze) {
  tibble::tibble(
    siteID = as.character(siteID),
    testName = as.character(testName),
    digitToAnalyze = as.numeric(digitToAnalyze)
  )
}


#' 3. Compute Per-Site/Test/Digit Frequencies
#'
#' @param df Tibble. Columns: siteID, testName, digitToAnalyze.
#' @return A tibble with frequency and percent columns for site and not-site.
#'
#' @importFrom dplyr group_by ungroup mutate select distinct n
compute_digit_frequencies <- function(df) {
  df %>%
    dplyr::group_by(siteID, testName, digitToAnalyze) %>%
    dplyr::mutate(freqSITE = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(siteID, testName) %>%
    dplyr::mutate(
      n_2 = sum(freqSITE),
      percentSITE = freqSITE / n_2 * 100
    ) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(testName, digitToAnalyze) %>%
    dplyr::mutate(n_3 = dplyr::n()) %>%
    dplyr::group_by(testName) %>%
    dplyr::mutate(
      n_4 = sum(n_3),
      freqNotSITE = n_3 - freqSITE
    ) %>%
    dplyr::group_by(siteID, testName) %>%
    dplyr::mutate(n_6 = sum(freqNotSITE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(percentNotSITE = freqNotSITE / n_6 * 100) %>%
    dplyr::select(-n_2, -n_3, -n_4, -n_6) %>%
    dplyr::distinct()
}



#' 4. Expand to All Site/Test/Digit Combinations with Zeros for Missing
#'
#' @param df_freq Tibble. Output from compute_digit_frequencies().
#' @param testName Character vector. Test names.
#' @param siteID Character vector. Site IDs.
#'
#' @return Tibble.
#' @importFrom tidyr expand_grid
#' @importFrom dplyr left_join
#' @importFrom tidyr replace_na
expand_digitpref_combinations <- function(df_freq, testName, siteID) {
  tests_list <- unique(testName)
  sites_list <- unique(siteID)
  digit_list <- 0:9
  combo_grid <- tidyr::expand_grid(
    siteID = sites_list,
    testName = tests_list,
    digitToAnalyze = digit_list
  )
  combo_grid %>%
    dplyr::left_join(df_freq, by = c("siteID", "testName", "digitToAnalyze")) %>%
    tidyr::replace_na(list(
      freqSITE = 0,
      percentSITE = 0,
      freqNotSITE = 0,
      percentNotSITE = 0
    ))
}


#' 5. Remove Empty Site/Test Groups (No Data)
#'
#' @param frequTab Tibble. Frequency table.
#' @return Tibble with only groups with at least one observation.
#'
#' @importFrom dplyr group_by filter ungroup
filter_empty_digitpref_groups <- function(frequTab) {
  frequTab %>%
    dplyr::group_by(siteID, testName) %>%
    dplyr::filter(sum(freqSITE) > 0) %>%
    dplyr::ungroup()
}


#' 6. Assemble Site and Not-Site Tables
#'
#' Creates a long-format combined table for statistical testing.
#' @param frequTab Tibble. Frequency table.
#' @return Tibble.
#'
#' @importFrom dplyr select mutate bind_rows arrange
assemble_site_not_site_tables <- function(frequTab) {
  siteTab <- frequTab %>%
    dplyr::select(siteID, testName, digitToAnalyze, Count = freqSITE, Percent = percentSITE) %>%
    dplyr::mutate(siteFlag = "Site")
  nonsiteTab <- frequTab %>%
    dplyr::select(siteID, testName, digitToAnalyze, Count = freqNotSITE, Percent = percentNotSITE) %>%
    dplyr::mutate(siteFlag = "NotSite")
  dplyr::bind_rows(siteTab, nonsiteTab) %>%
    dplyr::arrange(siteID, testName, siteFlag, digitToAnalyze)
}


#' 7. Perform Chi-square Digit Preference Test
#'
#' @param frequTab_long Tibble. Output of assemble_site_not_site_tables().
#' @return Tibble. Summary statistics.
#'
#' @importFrom dplyr group_by summarize ungroup mutate case_when select
#' @importFrom stats chisq.test
perform_chsq_test <- function(frequTab_long) {
  frequTab_long %>%
    dplyr::group_by(siteID, testName) %>%
    dplyr::summarize(
      observed = sum(Count),
      statistic = stats::chisq.test(x = Count, y = siteFlag)$statistic,
      p_value = stats::chisq.test(x = Count, y = siteFlag)$p.value,
      dps = round(100 * sqrt(statistic / observed * 9), 2),
      dpsClas = dplyr::case_when(
        is.nan(dps) ~ '',
        dps < 8 ~ "Excellent",
        dps < 12 ~ "Good",
        dps < 20 ~ "Acceptable",
        TRUE ~ "Problematic"
      ),
      .groups = "drop"
    ) %>%
    dplyr::select(siteID, testName, observed, statistic, p_value, dps, dpsClas)
}


#' 8. Perform Cochran-Mantel-Haenszel Digit Preference Test
#'
#' @param frequTab_long Tibble. Output of assemble_site_not_site_tables().
#' @return Tibble. Summary statistics per site/test.
#'
#' @importFrom tidyr pivot_wider
#' @importFrom dplyr arrange group_by mutate ungroup select summarise case_when
#' @importFrom stats pchisq
perform_cmh_test <- function(frequTab_long) {
  transformedTab <- frequTab_long %>%
    tidyr::pivot_wider(
      names_from = siteFlag,
      values_from = c(Count, Percent),
      names_glue = "{.value}_{siteFlag}"
    ) %>%
    dplyr::rename(
      Count_site = Count_Site,
      Percent_site = Percent_Site,
      Count_others = Count_NotSite,
      Percent_others = Percent_NotSite
    ) %>%
    dplyr::arrange(siteID, testName, digitToAnalyze) %>%
    dplyr::group_by(siteID, testName) %>%
    dplyr::mutate(
      sum_site = sum(Count_site),
      sum_others = sum(Count_others),
      sum_all = sum_site + sum_others,
      n_j = Count_site + Count_others,
      aj = ((2 * n_j) - n_j + 1) / (2 * (sum_all + 1)),
      μα = sum(n_j * aj) / sum(n_j),
      vα = ((aj - μα)^2 * n_j) / sum_all,
      fs = (aj * Count_site) / sum_site,
      fo = (aj * Count_others) / sum_others
    ) %>%
    dplyr::ungroup()
  
  summary_result <- transformedTab %>%
    dplyr::group_by(siteID, testName) %>%
    dplyr::summarise(
      μα = unique(μα),
      sum_vα = sum(vα),
      sum_fs = sum(fs),
      sum_fo = sum(fo),
      sum_site = unique(sum_site),
      sum_others = unique(sum_others),
      sum_all = unique(sum_all),
      observed = sum(unique(sum_all)),
      statistic = (sum(sum_fs) - sum(sum_fo))^2 /
        ((1 / sum(unique(sum_site)) + 1 / sum(unique(sum_others))) *
           ((sum(unique(sum_all)) * sum(sum_vα)) / (sum(unique(sum_all)) - 1))),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      p_value = stats::pchisq(statistic, df = 1, lower.tail = FALSE),
      dps = round(100 * sqrt(statistic / observed * 9), 2),
      dpsClas = dplyr::case_when(
        is.nan(dps) ~ '',
        dps < 8 ~ "Excellent",
        dps < 12 ~ "Good",
        dps < 20 ~ "Acceptable",
        TRUE ~ "Problematic"
      )
    ) %>%
    dplyr::select(
      siteID, testName, observed, statistic, p_value, dps, dpsClas
    )
  summary_result
}

