test_that("clean_location maps region variants to canonical names", {
  expect_equal(clean_location("region 1"), "Region I (Ilocos Region)")
  expect_equal(clean_location("NCR"),      "National Capital Region (NCR)")
  expect_equal(clean_location("CARAGA"),   "Region XIII (CARAGA)")
  expect_equal(clean_location("BARMM"),
               "Bangsamoro Autonomous Region In Muslim Mindanao (BARMM)")
})

test_that("clean_location lowercases unrecognised names", {
  expect_equal(clean_location("UNKNOWN PLACE"), "unknown place")
})

test_that("clean_location handles province shorthand", {
  expect_equal(clean_location("davao norte"), "davao del norte")
  expect_equal(clean_location("agusan sur"),  "agusan del sur")
})
