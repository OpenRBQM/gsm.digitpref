# ref.:

# M. E. Stokes, C. S. Davis and G. G. Koch
# Categorical Data Analysis Using the SAS System.
# 3rd Edition.  Cary, NC: SAS Institute, Inc.

# Kari Kuulasmaa, Hans-Werner Hense and Hanna Tolonen
# Quality Assessment of Data on Blood Pressure in the WHO MONICA Project
# MONICA Memo, May 1998


# installation of the function: gsm.digitpref
pak::pak("OpenRBQM/gsm.digitpref")

# libraries
library(dplyr)
library(gsm.digitpref)
library(pharmaversesdtm)
library(stats)
library(tidyr)


# Exemplary of data set: data from the CDISC pilot project
# A set of Study Data Tabulation Model (SDTM):
  # Laboratory Measurements
  # Demography


data<-lb
dm<-dm
data <- data %>% left_join(dm %>% select(USUBJID,SITEID), by="USUBJID")

# define 3 vectors:
  # vector of Study Site Identifier (SITEID)
  # vector of Lab Tests (LBTEST)
  # vector of digit to analyze

siteID <- as.vector(data$SITEID)
testName <- as.vector(data$LBTEST)
digitToAnalyze <- splitByPlace(data$LBSTRESN, 0,0)
digitToAnalyze <- as.vector(unlist(digitToAnalyze[,2]))

data$digit_to_analyse <- splitByPlace(data$LBSTRESN, 0,0)
data <- data %>% select(6,13,14,24,25)

# function definition
statDigitPref <- function(siteID, testName, digitToAnalyze, method) {
  # data frame based on the input vectors
  df <- data.frame(siteID, testName, digitToAnalyze = as.numeric(digitToAnalyze))

  # frequencies of the digits
  df <- df %>%
    group_by(siteID, testName, digitToAnalyze) %>%
    mutate(freqSITE = n()) %>%
    ungroup() %>%
    group_by(siteID, testName) %>%
    mutate(n_2 = sum(freqSITE), percentSITE = freqSITE / n_2 * 100) %>%
    ungroup() %>%
    group_by(testName, digitToAnalyze) %>%
    mutate(n_3 = n()) %>%
    group_by(testName) %>%
    mutate(n_4 = sum(n_3), freqNotSITE = n_3 - freqSITE) %>%
    group_by(siteID, testName) %>%
    mutate(n_6 = sum(freqNotSITE)) %>%
    ungroup() %>%
    mutate(percentNotSITE = freqNotSITE / n_6 * 100) %>%
    select(-n_2, -n_3, -n_4, -n_6) %>%
    distinct()

  # list of the unique tests, sites, and digits to analyse
  tests_list <- unique(testName)
  sites_list <- unique(siteID)
  digit_list <- 0:9
  list <- expand.grid(siteID = sites_list, testName = tests_list, digitToAnalyze = digit_list)

  # combine list and input data frame
  frequTab <- list %>% left_join(df, by = c("siteID", "testName", "digitToAnalyze")) %>%
    replace(is.na(.), 0)

  # filtering rows where sum of freq across sites > 0
  frequTab <- frequTab %>%
    group_by(siteID, testName) %>%
    filter(sum(freqSITE) > 0) %>%
    ungroup()

  # defining table for site and not-site and bind both
  siteTab <- frequTab %>%
    select(siteID, testName, digitToAnalyze, Count = freqSITE, Percent = percentSITE) %>%
    mutate(siteFlag = "Site")
  nonsiteTab <- frequTab %>%
    select(siteID, testName, digitToAnalyze, Count = freqNotSITE, Percent = percentNotSITE) %>%
    mutate(siteFlag = "NotSite")
  frequTab <- bind_rows(siteTab, nonsiteTab) %>%
    arrange(siteID, testName, siteFlag, digitToAnalyze)

  # choosing method for statistical computations: chsq.test/cmh.test
  if (method == "chsq.test") {
    # chsq.test: p-value, statistic, observed, dps
    outputStat <- frequTab %>%
      group_by(siteID, testName) %>%
      summarize(
        observed = sum(Count),
        statistic = chisq.test(Count, siteFlag)$statistic,
        p_value = chisq.test(Count, siteFlag)$p.value,
        dps = round(100 * sqrt(statistic / observed * 9), 2),
        dpsClas = case_when(
          dps < 8 ~ "Excellent",
          dps < 12 ~ "Good",
          dps < 20 ~ "Acceptable",
          TRUE ~ "Problematic"
        )
      )
  } else if (method == "cmh.test") {
    # preparing data
    transformedTab <- frequTab %>%
      pivot_wider(names_from = siteFlag, values_from = c(Count, Percent),
                  names_glue = "{.value}_{siteFlag}") %>%
      rename(Count_site = Count_Site,
             Percent_site = Percent_Site,
             Count_others = Count_NotSite,
             Percent_others = Percent_NotSite) %>%
      arrange(siteID, testName, digitToAnalyze)

    # initial calcul
    transformedTab <- transformedTab %>%
      group_by(siteID, testName) %>%
      mutate(
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
      ungroup()

    detailed_result <- transformedTab %>%
      select(siteID, testName, digitToAnalyze, Count_site, Percent_site, Count_others, Percent_others, sum_site, sum_others, sum_all, n_j, aj, μα, vα, fs, fo)

    summary_result <- detailed_result %>%
      group_by(siteID, testName) %>%
      summarise(
        μα = unique(μα),
        sum_vα = sum(vα),
        sum_fs = sum(fs),
        sum_fo = sum(fo),
        sum_site = unique(sum_site),
        sum_others = unique(sum_others),
        sum_all = unique(sum_all),
        observed = sum(unique(sum_all)),
        statistic = (sum(fs) - sum(fo))^2 / ((1 / sum(unique(sum_site)) + 1 / sum(unique(sum_others))) * ((sum(unique(sum_all)) * sum(sum(vα))) / (sum(unique(sum_all)) - 1)))
      )

    # p-value for cmh.test
    summary_result <- summary_result %>%
      mutate(p_value = pchisq(statistic, df = 1, lower.tail = FALSE))

    outputStat <- summary_result %>%
      mutate(
        dps = round(100 * sqrt(statistic / observed * 9), 2),
        dpsClas = case_when(
          dps < 8 ~ "Excellent",
          dps < 12 ~ "Good",
          dps < 20 ~ "Acceptable",
          TRUE ~ "Problematic"
        )
      )
    outputStat <- outputStat %>% select(-"μα",-"sum_vα",-"sum_fs",-"sum_fo",-"sum_site",-"sum_others",-"sum_all")
  } else {
    stop("Unknown method, available methods: 'chsq.test' i 'cmh.test'.")
  }

  return(outputStat)
}

# run function
result <- statDigitPref(siteID, testName, digitToAnalyze, method = "chsq.test")

# output:
# table contains:
  # siteID - Study Site Identifier
  # testName - test name for numeric measurements
  # observed - sample size (number of measurements/digits to analyse)
  # statistic - for chsq.test or cmh.test respectively
  # p-value - for chsq.test or cmh.test respectively
  # dps - digit preference score (ref. + https://nutriverse.io/nipnTK/articles/dp.html)
  # dpsClass - interpretation for dps score






