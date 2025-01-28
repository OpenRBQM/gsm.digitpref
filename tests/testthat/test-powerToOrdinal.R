test_that("powerToOrdinal works for positive powers", {
  expect_equal(powerToOrdinal(0), "Ones")
  expect_equal(powerToOrdinal(1), "Tens")
  expect_equal(powerToOrdinal(3), "Thousands")
  expect_equal(powerToOrdinal(4), "Ten Thousands")
})

test_that("powerToOrdinal works for negative powers", {
  expect_equal(powerToOrdinal(-1), "Tenths")
  expect_equal(powerToOrdinal(-3), "Thousandths")
  expect_equal(powerToOrdinal(-4), "Ten Thousandths")
})

test_that("powerToOrdinal applies case", {
  expect_equal(powerToOrdinal(0, strCase = "lower"), "ones")
  expect_equal(powerToOrdinal(0, strCase = "lOwEr"), "ones")
  expect_equal(powerToOrdinal(0, strCase = "UPPER"), "ONES")
  expect_equal(powerToOrdinal(0, strCase = "upper"), "ONES")
})

test_that("powerToOrdinal uses strAfter", {
  expect_equal(powerToOrdinal(0, strAfter = ""), "Ones")
  expect_equal(powerToOrdinal(0, strAfter = "place"), "Ones Place")
  expect_equal(powerToOrdinal(0, strAfter = "digit"), "Ones Digit")
  expect_equal(powerToOrdinal(0, strAfter = "Thingy"), "Ones Thingy")
})
