test_that("extend function works", {
  x = simRW(20, start=2000)
  x1 = window(x, start=2000, end=2010)
  x2 = window(x, start=2011, end=2019)
  x3 = window(x, start=2008, end=2019)
  z1 = extend(x1, x2)  # no overlapping
  z2 = extend(x1, x3)  # with overlapping
  expect_identical(z1, x)
  expect_identical(z2, x)
})

test_that("na.pad function works", {
  x = ts(rep(1, 12), frequency = 4, start = 2000)
  y = ts(c(rep(NA, 4), x), frequency = 4, start = 1999)
  z = ts(c(x, rep(NA, 4)), frequency = 4, start = 2000)
  expect_identical(na.pad(x, time(y)), y)
  expect_identical(na.pad(x, time(z)), z)
  expect_identical(na.pad(x, n=16, sides="left"), y)
  expect_identical(na.pad(x, n=16, sides="right"), z)
})

test_that("Interpolate levels from growth rates", {
  x = origx = simRW(12, freq = 4) + 100
  x[1:4] <- NA; x[9:12] <- NA
  yoy = (origx/lag(origx, -4) - 1)*100
  qoq = (origx/lag(origx, -1) - 1)*100
  expect_equal(interpGR(x, yoy, lag = 4), origx)
  expect_equal(interpGR(x, qoq, lag = 1), origx)
})

test_that("Differencing YTD series works", {
  # January value is preserved
  x = ts(rep(1, 36), start = 2000, frequency = 12)
  x_ytd = yearapply(x, cumsum)
  expect_identical(ytddiff(x_ytd), x)
  # if series starts in the middle of a year, NA is produced
  y = ts(rep(1, 36), start = 2000.5, frequency = 12)
  y_ytd = yearapply(y, cumsum)
  expect_identical(ytddiff(y_ytd), replace(y, 1, NA))
  # leading NA should be preserved
  x_na = na.pad(x, n = 40, sides = "left")
  x_ytd_na = na.pad(x_ytd, n = 40, sides = "left")
  expect_equal(ytddiff(x_ytd_na), x_na)
})
