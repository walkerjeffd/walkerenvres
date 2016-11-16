context("Water Day")

test_that("water_day extracts correct water day", {
  x <- seq.Date(from = as.Date("2006-10-01"), to = as.Date("2007-09-30"), by = "day")
  expect_that(water_day(x, start_month = 10), equals(seq(1, 365)))

  y <- seq.Date(from = as.Date("2005-10-01"), to = as.Date("2006-09-30"), by = "day")
  expect_that(water_day(y, start_month = 10), equals(seq(1, 365)))
})

test_that("water_day extracts correct water day starting in Jan", {
  x <- seq.Date(from = as.Date("2007-12-31"), to = as.Date("2008-01-02"), by = "day")
  expect_that(water_day(x, start_month = 1), equals(c(365, 1, 2)))
})

test_that("water_day extracts correct water day with leap year", {
  x <- seq.Date(from = as.Date("2007-10-01"), to = as.Date("2008-09-30"), by = "day")
  expect_that(water_day(x, start_month = 10), equals(seq(1, 366)))

  y <- seq.Date(from = as.Date("2008-10-01"), to = as.Date("2009-09-30"), by = "day")
  expect_that(water_day(y, start_month = 10), equals(seq(1, 365)))
})

test_that("water_day extracts correct water day starting in February", {
  x <- seq.Date(from = as.Date("2006-02-01"), to = as.Date("2009-01-31"), by = "day")
  expect_that(water_day(x, start_month = 2), equals(c(seq(1, 365), seq(1, 365), seq(1, 366))))
})


