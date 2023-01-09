
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
  y = as.data.frame(x)
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

#' Seasonal Adjustment with STL
#'
#' A seasonal adjustment procedure based on STL agnostic to the exact occasion
#' of the Lunar New Year (LNY) holiday. The LNY occurs in January or February
#' arbitrarily, which causes blips in Jan-Feb observations even after seasonal
#' smoothing. This function augments the standard procedure assuming the blips
#' in Jan-Feb values after subtracting the seasonal component are distortions
#' induced by the LNY holiday, and therefore can be discarded as long as they
#' are not unusually large (beyond some standard deviations).
#' The remedy makes it possible to perform seasonal adjustment to China's time
#' series data without lunar calendar regressors.
#'
#' @param x A univariate time series to be processed.
#' @param s.window The loess window for seasonal extraction to be passed on to [stats::stl].
#' Should be odd and at least 7. A default value is provided though a user-specified
#' value is strongly recommended.
#' @param lny Methods to deal with Lunar New Year effect. Default is `trim`, which
#' discards the blips in Jan and Feb if within a given number of standard deviations.
#' `keep` will keep all the blips. `discard` removes all blips and assumes Jan and Feb
#' observations align perfectly with the trend.
#' @param nsigma The number of standard deviations if `trim` is specified in `lny`.
#' @param simplify If `TRUE`, only the seasonally adjusted series is returned.
#' Otherwise, a matrix with the decomposed components will be returned.
#' @param na.action Function to deal with `NA`s in `x`. If `NULL`, linear
#' interpolation will be applied.
#' @param ... Further arguments to be passed to [stats::stl].
#'
#' @return The seasonally-adjusted series with `NA`s interpolated if `simplify`
#' is `TRUE`; or a matrix of STL components if `simplify` is `FALSE`.
#' @export
stlseas = function(x, s.window=7, lny=c("trim","keep","discard"), nsigma=3,
                   simplify=T, na.action = NULL, ...) {
  origx = x
  if (anyNA(x)) {
    x = if(is.function(na.action)) na.action(x) else zoo::na.approx(x)
  }
  m = stl(x, s.window, ...)
  t = m$time.series[,'trend']
  s = m$time.series[,'seasonal']
  r = m$time.series[,'remainder']
  # holiday effect: LNY only affect Jan and Feb
  u = r; u[cycle(u)>2] = 0;
  # remainder without holiday effect
  v = r; v[cycle(v)<=2] = 0;
  lny = match.arg(lny)
  if (frequency(x)==12 && lny=="trim") {
    ut = u / t # relative to trend
    # rolling standard deviation
    sig = zoo::rollapplyr(ut, width = 120, FUN = sd, partial = T)
    # arithmetic operation will fail if contains NA
    sig[is.na(sig)] = 0
    ## Diagnostic plot
    ## { plot(ut); lines(sig, col=2); lines(2*sig, col=3); }
    # trim the holiday effect with n std.dev.
    for (i in 1:length(ut)) {
      if (abs(ut[i]) <= nsigma*sig[i]) u[i] = 0
      else u[i] = t[i]*(ut[i] - sign(ut[i])*nsigma*sig[i])
    }
  } else if (frequency(x)==12 && lny=="discard") {
    u = 0  # discard all irregularities in Jan-Feb
  }
  sa = t + u + v
  # sa[is.na(origx)] = NA  # put NA back
  if (isTRUE(simplify)) return(sa)
  else {
    ret = cbind(origx, t, s, u, v, sa)
    colnames(ret) = c("Data", "Trend", "Seasonal", "LNY", "Remainder", "SA")
    return(ret)
  }
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


