test_that("calculate_period_days returns correct days for Annual period", {
  expect_equal(calculate_period_days("2024-01-01", "Annual"), 366L) # leap year
  expect_equal(calculate_period_days("2023-01-01", "Annual"), 365L) # common year
})

test_that("calculate_period_days returns correct days for Monthly period", {
  expect_equal(calculate_period_days("2024-02-10", "Monthly"), 29L) # leap Feb
  expect_equal(calculate_period_days("2023-02-10", "Monthly"), 28L)
  expect_equal(calculate_period_days("2023-01-01", "Monthly"), 31L)
})

test_that("calculate_period_days returns correct days for Quarterly period", {
  expect_equal(calculate_period_days("2023-02-01", "Quarterly"), 90L) # Q1 non-leap
  expect_equal(calculate_period_days("2023-04-01", "Quarterly"), 91L) # Q2
})

test_that("calculate_period_days returns correct days for Semestral period", {
  expect_equal(calculate_period_days("2023-03-01", "Semestral"), 181L) # S1 non-leap
  expect_equal(calculate_period_days("2023-08-01", "Semestral"), 184L) # S2
})

test_that("calculate_period_days falls back to 365 for unknown period", {
  expect_equal(calculate_period_days("2023-06-01", "Unknown"), 365L)
})
