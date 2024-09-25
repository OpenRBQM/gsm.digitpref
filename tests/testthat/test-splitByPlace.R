test_that("splitByPlaces errors for non-numbers", {
  expect_error(
    splitByPlaces(letters),
    class = "gsm.digitpref-error-non_number"
  )
})

test_that("splitByPlaces works for simple integers", {
  given <- 1:3*123
  test_result <- splitByPlaces(given, 2, -1)
  expect_named(test_result, c("given", "10^2", "10^1", "10^0", "10^-1"))
  expect_identical(test_result$`10^2`, 1:3)
  expect_identical(test_result$`10^1`, 1:3*2L)
  expect_identical(test_result$`10^0`, 1:3*3L)
  expect_identical(test_result$`10^-1`, rep(NA_integer_, 3))
  expect_snapshot({
    test_result
  })
})

test_that("splitByPlaces works for doubles", {
  given <- 1:3*123.21
  test_result <- splitByPlaces(given)
  expect_named(test_result, c("given", "10^2", "10^1", "10^0", "10^-1", "10^-2"))
  expect_identical(test_result$`10^2`, 1:3)
  expect_identical(test_result$`10^1`, 1:3*2L)
  expect_identical(test_result$`10^0`, 1:3*3L)
  expect_identical(test_result$`10^-1`, 1:3*2L)
  expect_identical(test_result$`10^-2`, 1:3)
  expect_snapshot({
    test_result
  })
})

test_that("extractPlace throws error in weird cases", {
  expect_error(
    extractPlace(place = 1.2),
    class = "gsm.digitpref-error-bad_place"
  )
  expect_error(
    extractPlace(place = NA),
    class = "gsm.digitpref-error-bad_place"
  )
  expect_error(
    extractPlace(place = NULL),
    class = "gsm.digitpref-error-bad_place"
  )
  expect_error(
    extractPlace(place = letters),
    class = "gsm.digitpref-error-bad_place"
  )
  expect_error(
    extractPlace(place = "a"),
    class = "gsm.digitpref-error-bad_place"
  )
})
