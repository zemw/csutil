test_that("trans function works", {
  # aggregation / disaggregation works
  x = ts(rep(1,12), frequency = 4)
  x_sum = ts(rep(4, 3), frequency = 1)
  expect_equal(trans(x, freq = 1, agg = "sum"), x_sum)
  expect_equal(trans(x_sum, freq = 4, disagg = "sum"), x)
  # ytd differencing works
  x_ytd = yearapply(x, cumsum)
  expect_equal(trans(x_ytd, ytd = "diff"), x)
  # na_impute works
  x_na = replace(x, c(5,6), NA)
  expect_equal(trans(x_na, na_impute = "locf"), x)
  # leading NA will be trimmed
  x_na2 = replace(x, c(1:4), NA)
  expect_equal(trans(x_na2, na_impute = "locf"), window(x,start=2))
  # na_predict works
  x2 = x*2 + rnorm(12, sd=0.1)
  expect_equal(trans(x_na, na_predict = "reg", xreg = x2), x)
  # seasonal adjustment works
  expect_no_error(trans(AirPassengers, seas = "x11", seas_cny = NULL))
  expect_no_error(trans(AirPassengers, seas = "stl", seas_cny = NULL))
  # change output works
  x_cum = ts(cumsum(x), frequency = 4)
  expect_equal(trans(x_cum, chg = "d"), diff(x_cum))
  # padding works
  expect_equal(length(trans(x_cum, chg = "d", na_pad = T)), length(x_cum))
})
