test_that("plotDigitCounts produces an expected chart", {
  set.seed(42)
  sample_data <- data.frame(
    siteID = sample(c("siteA", "siteB", "siteC"), 1000, replace = TRUE),
    onesPlace = sample(0:9, 1000, replace = TRUE)
  )
  test_result <- plotDigitCounts(sample_data, "siteID", "onesPlace")
  expect_s3_class(test_result, "ggplot")
  expect_identical(as.character(test_result$labels$x), "siteID")
  expect_identical(as.character(test_result$labels$y), "Frequency")
  expect_identical(as.character(test_result$labels$fill), "Digit")
  expect_identical(
    test_result$labels$title,
    "Digit Distribution for onesPlace of Data by siteID"
  )
  vdiffr::expect_doppelganger(
    title = "plotDigitCounts default",
    fig = test_result
  )
})

test_that("plotDigitCounts accepts supplied group label", {
  set.seed(42)
  sample_data <- data.frame(
    siteID = sample(c("siteA", "siteB", "siteC"), 1000, replace = TRUE),
    onesPlace = sample(0:9, 1000, replace = TRUE)
  )
  test_result <- plotDigitCounts(
    sample_data,
    "siteID",
    "onesPlace",
    strGroupLabel = "Alternative Name"
  )
  expect_s3_class(test_result, "ggplot")
  expect_identical(test_result$labels$x, "Alternative Name")
  expect_identical(
    test_result$labels$title,
    "Digit Distribution for onesPlace of Data by Alternative Name"
  )
})

test_that("plotDigitCounts accepts supplied place label", {
  set.seed(42)
  sample_data <- data.frame(
    siteID = sample(c("siteA", "siteB", "siteC"), 1000, replace = TRUE),
    onesPlace = sample(0:9, 1000, replace = TRUE)
  )
  test_result <- plotDigitCounts(
    sample_data,
    "siteID",
    "onesPlace",
    strPlaceLabel = "Ones Place"
  )
  expect_s3_class(test_result, "ggplot")
  expect_identical(
    test_result$labels$title,
    "Digit Distribution for Ones Place of Data by siteID"
  )
})

test_that("plotDigitCounts accepts title override", {
  set.seed(42)
  sample_data <- data.frame(
    siteID = sample(c("siteA", "siteB", "siteC"), 1000, replace = TRUE),
    onesPlace = sample(0:9, 1000, replace = TRUE)
  )
  test_result <- plotDigitCounts(
    sample_data,
    "siteID",
    "onesPlace",
    strGroupLabel = "Alternative Name",
    strChartTitle = "My Chart"
  )
  expect_s3_class(test_result, "ggplot")
  expect_identical(test_result$labels$x, "Alternative Name")
  expect_identical(
    test_result$labels$title,
    "My Chart"
  )
})

test_that("plotDigitCounts accepts scaleDigitPalette override", {
  set.seed(42)
  sample_data <- data.frame(
    siteID = sample(c("siteA", "siteB", "siteC"), 1000, replace = TRUE),
    onesPlace = sample(0:9, 1000, replace = TRUE)
  )
  vdiffr::expect_doppelganger(
    title = "plotDigitCounts no palette",
    fig = plotDigitCounts(
      sample_data,
      "siteID",
      "onesPlace",
      scaleDigitPalette = NULL
    )
  )
  vdiffr::expect_doppelganger(
    title = "plotDigitCounts paired palette",
    fig = plotDigitCounts(
      sample_data,
      "siteID",
      "onesPlace",
      scaleDigitPalette = ggplot2::scale_fill_brewer(palette = "Paired")
    )
  )
})

test_that("plotDigitCounts accepts theme override", {
  set.seed(42)
  sample_data <- data.frame(
    siteID = sample(c("siteA", "siteB", "siteC"), 1000, replace = TRUE),
    onesPlace = sample(0:9, 1000, replace = TRUE)
  )
  vdiffr::expect_doppelganger(
    title = "plotDigitCounts no theme",
    fig = plotDigitCounts(
      sample_data,
      "siteID",
      "onesPlace",
      themePlot = NULL
    )
  )
  vdiffr::expect_doppelganger(
    title = "plotDigitCounts theme_minimal",
    fig = plotDigitCounts(
      sample_data,
      "siteID",
      "onesPlace",
      themePlot = ggplot2::theme_minimal()
    )
  )
})
