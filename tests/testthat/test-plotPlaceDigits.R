test_that("plotPlaceDigits produces an expected chart", {
  set.seed(42)
  sample_data <- tibble::tibble(
    siteID = sample(c("siteA", "siteB", "siteC"), 1000, replace = TRUE),
    labResults = runif(1000)*10
  )
  test_result <- plotPlaceDigits(sample_data, labResults, siteID, 0)
  expect_s3_class(test_result, "ggplot")
  expect_identical(as.character(test_result$labels$x), "siteID")
  expect_identical(as.character(test_result$labels$y), "Frequency")
  expect_identical(as.character(test_result$labels$fill), "Digit")
  expect_identical(
    test_result$labels$title,
    "Digit Distribution for Ones Place of Data by siteID"
  )
  vdiffr::expect_doppelganger(
    title = "plotPlaceDigits default",
    fig = test_result
  )
})
