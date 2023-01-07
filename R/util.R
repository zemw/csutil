
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

