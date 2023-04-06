
#' Numeric Representation of Dates
#'
#' Converts between [base::Date] objects and the numeric representations.
#' The numeric representation differs from the POSIX representation for
#' the benefit of readability.
#' For example, monthly date points in the year 2000 is represented as
#' 2000, 2000.083, 2000.167, etc.
#' Each month is converted to a fraction of 1/12 between integer years, and
#' each day is a fraction of 1/365.
#' The representation is designed to work with [zoo::yearmon] and [zoo::yearqtr].
#'
#' @param t An object of class `Date`
#' @rdname datenum
#' @return Returns the numeric representation of a calendar date or vice versa.
#' @export
date2Num = function(t) {
  t = as.Date(t)
  y = lubridate::year(t)
  m = lubridate::month(t)
  d = lubridate::day(t)
  x = y + (m-1)/12 + (d-1)/365
  return(x)
}

#' @param x A number representing a calendar date
#' @rdname datenum
#' @export
num2Date = function(x) {
  x = as.numeric(x)
  d = x %% (1/12)
  m = x %% 1 - d
  y = x %/% 1
  t = ISOdate(y, round(m*12+1), round(d*365+1))
  return(as.Date(t))
}

#' Coercion between `ts` and `date.frame`
#'
#' Methods to coercing between [stats::ts] and [base::data.frame] representation
#' of time series data. The time index is converted to numbers (see [date2Num])
#' when coercing to `ts` objects, and `Date` when coercing to `data.frame`.
#'
#' @param x An object of class `ts`
#' @return The time series data in `data.frame` or `ts`.
#' @rdname tsdf
#' @export
ts2DF = function(x) {
  stopifnot(is.ts(x) || zoo::is.zoo(x))
  y = as.data.frame(x, row.names = NULL)
  date = num2Date(time(x))
  return(cbind(date,y))
}


#' @param df A `data.frame` containing time series data
#' @param dateIndex Index of the `Date` column
#' @rdname tsdf
#' @export
df2TS = function(df, dateIndex=1) {
  y = as.matrix(df[,-dateIndex])
  t = date2Num(df[,dateIndex])
  z = zoo::zoo(y, t)
  return(as.ts(z))
}

#' Apply Function to Each Time Series
#'
#' A wrapper over [base::lapply], which applies function to each column
#' of an `mts` object, and returns the updated `mts` object.
#'
#' @param x an `mts` object
#' @param FUN the function to be applied
#' @param ... further arguments to be passed to `lapply`
#' @export
tsapply = function(x, FUN, ...) {
  res = lapply(x, FUN, ...)
  do.call(cbind, res)
}

#' Lagged Differences
#'
#' Lagged Differences for `ts` objects with `NA` padding.
#'
#' @param x input time series to be differenced.
#' @param lag an integer indicating which lag to use.
#' @param na.pad whether to pad the output series to have the same
#' length as the input series.
#'
#' @return the differenced series.
#' @export
tsdiff = function(x, lag = 1, na.pad = FALSE) {
  y = diff(as.zoo(x), lag = lag, na.pad = na.pad)
  return(as.ts(y))
}

#' Compute Growth Rate
#'
#' @param x input level series.
#' @param lag an integer indicating which lag to use.
#' @param na.pad whether to pad the output series to have the same
#' length as the input series.
#' @param pct output in percentage points.
#'
#' @return the growth rate series.
#' @export
tsgrowth = function(x, lag = 1, na.pad = FALSE, pct = TRUE) {
  r = x / lag(x, -lag) - 1
  r = na.pad(r, time(x))
  if (isTRUE(pct)) r = r*100
  return(r)
}

#' Apply Function within a Year
#'
#' Apply a function to quarterly or monthly series on yearly basis.
#' That is, the function will be applied only with observations within a year
#' and then move on to the next year.
#' Useful for computing year-to-date (YTD) accumulating sums or differencing
#' YTD sums.
#'
#' @param x the input series (`ts`-convertible object)
#' @param FUN the function to be applied
#'
#' @return Always return an `ts` object. Depending on the function provided,
#' the returned series may not have the same length as the input one.
#' @export
yearapply = function(x, FUN) {
  z = zoo::as.zooreg(x)
  years = floor(as.numeric(time(x)))
  z_chuncks = split(z, years) |> lapply(FUN)
  z_chuncks = z_chuncks[lengths(z_chuncks) > 0] # keep only non-empty items
  z_output = do.call(c, z_chuncks)
  return(as.ts(z_output))
}

#' Differencing YTD series
#'
#' Differencing year-to-date (YTD) series into quarterly or monthly levels.
#'
#' @param x the input series (`ts` alike object)
#' @param split methods to split the values of the first two months.
#' Due to the Chinese New Year effect, some series are released with only 11
#' observations in a year in which the first two months are reported together.
#' Specify `avg` to split the sum of the first two months evenly between
#' the two months. `default` will leave the value for each month as it is.
#' @return The differenced series.
#' @export
ytddiff = function(x, split = c("default", "avg")) {
  stopifnot(frequency(x) %in% c(4,12))
  y = as.ts(x)
  for (i in 1:length(x)) {
    if (cycle(x)[i] == 1) y[i] = x[i]
    else if (i > 1) y[i] = x[i] - x[i-1]
    else y[i] = NA_real_
  }
  if (match.arg(split) == "avg" && frequency(x) == 12) {
    for (i in 1:length(x))
      if (cycle(x)[i] == 2) {
        y[i] = x[i] / 2; if (i > 1) y[i-1] = x[i] / 2 }
  }
  return(y)
}

#' Extend Time Series
#'
#' Extend one time series with another. This is a wrapper over [zoo::rbind.zoo].
#' The difference is `rbind.zoo` will report error on overlapped indexes,
#' while this function will ignore the overlapped indexes on the right-hand-side
#' time series.
#'
#' @param x the time series to be extended.
#' @param ... time series objects to be extended to `x`.
#'
#' @return The extended series. Note that leading or trailing `NA`s will be removed.
#' @export
extend = function(x, ...) {
  l = Map(function(x) na.trim(as.zoo(x)), list(x, ...))
  z = Reduce(function(x, y) c(x, y[!(time(y) %in% time(x))]), l)
  return(as.ts(z))
}

#' Padding Time Series with `NA`s
#'
#' Pad time series with leading or trailing `NA`s
#'
#' @param x a univariate time series.
#' @param index pad the time series to have the same time index as provided.
#' If this argument is provided, other arguments will be ignored.
#' @param n the total length of the series after padding
#' @param sides insert `NA`s on the left or right side.
#'
#' @return The padded series with the provided time index or the target length.
#' @export
na.pad = function(x, index = NULL, n = 0, sides = c("left", "right")) {
  if (!is.null(index)) {
    # Warn: Index vectors are of different classes: numeric yearqtr
    suppressWarnings(t <- merge(zoo(NA, index), x))
    return(as.ts(t[,2]))
  } else if (n > length(x)) {
    f = frequency(x)
    m = n - length(x)
    y = if (match.arg(sides) == "left")  c(rep(NA, m), coredata(x))
        else c(coredata(x), rep(NA, m))
    s = if (match.arg(sides) == "left") time(x)[1] - m/f else time(x)[1]
    return(ts(y, start = s, frequency = f))
  } else return(x)
}

#' Interpolate with Growth Rates
#'
#' Interpolate or extrapolate missing values in a time series from its
#' corresponding growth rates.
#'
#' @param x the level series.
#' @param r the growth rates (with the same length as `x`)
#' @param lag number of lags when computing the growth rate. Default is the
#' frequency of the input series, i.e. `r` is the year-over-year growth rates.
#' `1` will indicate the growth rates are computed on two consecutive observations.
#' @param direction the direction in which to fill missing values.
#'
#' @return New series with missing values deducted from growth rates.
#' @export
interpGR = function(x, r, lag = frequency(x),
                    direction=c("both", "forward", "backward")) {
  if (NROW(x) != NROW(r)) { xr = cbind(x,r); x = xr[,1]; r = xr[,2] }
  stopifnot(rlang::is_scalar_integerish(lag)); k = lag
  direction = match.arg(direction)
  if (direction != "backward") {
    # filling forward
    for (i in (k + 1):length(x))
      if (is.na(x[i]) && !is.na(x[i - k]) && !is.na(r[i]))
        x[i] = x[i - k] * (1 + r[i]/100)
  }
  if (direction != "forward") {
    # filling backward
    for (i in (length(x) - k):1)
      if (is.na(x[i]) && !is.na(x[i + k]) && !is.na(r[i + k]))
        x[i] = x[i + k] / (1 + r[i + k]/100)
  }
  return(x)
}

#' Simulate a Random Walk
#'
#' @param n number of observations
#' @param drift drift term
#' @param sd standard deviation of the noise
#' @param freq frequency of the time series
#' @param start time of the first observation
#'
#' @return an `ts` object.
#' @export
simRW = function(n, drift=0, sd=1, freq=1, start=2000) {
  y = drift*(1:n) + cumsum(rnorm(n, sd = sd))
  ts(y, start = start, frequency = freq)
}

#' Simulate an AR(1) process
#'
#' @param n number of observations
#' @param ar the AR(1) coefficient
#' @param sd standard deviation of the noise
#' @param freq frequency of the time series
#' @param start time of the first observation
#'
#' @return an `ts` object.
#' @export
simAR1 = function(n, ar=.5, sd=1, freq=1, start=2000) {
  y = arima.sim(model = list(ar = ar, order = c(1,0,0)), n = n)
  ts(y, start = start, frequency = freq)
}
