context("Water Year")

test_that("water_year extracts correct water year", {
  x <- seq.Date(from = as.Date("2007-09-30"), to = as.Date("2007-10-02"), by = "day")
  expect_that(water_year(x), equals(c(2007, 2008, 2008)))
})

test_that("water_year extracts correct water year for January starting month", {
  x <- seq.Date(from = as.Date("2007-09-30"), to = as.Date("2007-10-02"), by = "day")
  expect_that(water_year(x, start_month = 1), equals(c(2007, 2007, 2007)))
})
