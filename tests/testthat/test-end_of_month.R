test_that("end_of_month returns the correct last day", {
  expect_equal(end_of_month(as.Date("2024-02-10")), as.Date("2024-02-29")) # leap
  expect_equal(end_of_month(as.Date("2023-02-10")), as.Date("2023-02-28"))
  expect_equal(end_of_month(as.Date("2023-01-15")), as.Date("2023-01-31"))
  expect_equal(end_of_month(as.Date("2023-11-01")), as.Date("2023-11-30"))
})
