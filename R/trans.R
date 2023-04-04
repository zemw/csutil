#' Time Series Transformation
#'
#' @param x time series object
#' @param ... arguments used to transform each single time series
#'
#' @rdname trans
#' @export
trans = function(x, ...) {
  UseMethod("trans")
}

#' Transform Single Time Series
#'
#' Performs time series transformation including aggregation, missing-value
#' imputation, seasonal adjustment and so on. Transformations are executed
#' sequentially as ordered in the argument list.
#'
#' @param freq frequency of the output series. If the intended frequency is
#' lower than the original one, aggregating method will apply. Transforming
#' to higher frequencies will generate NAs in the output series.
#' @param agg methods to aggregate time series to lower frequency.
#' @param disagg methods to disaggregate time series to higher frequency
#' (see [`tempdisagg`]).
#' @param multiplier a single number to scale up or down the values.
#' @param ytd methods to disaggregate YTD series into monthly or quarterly
#' series. `diff` for differencing within a year. Due to the Lunar New Year
#' (LNY) effect. NBS usually reports January and February observations
#' together (as the sum of the two month). Specify `diff_split` to split
#' the sum and accredit the values to Jan and Feb evenly.
#' @param na_impute methods to impute missing values (see package [`imputeTS`])
#' @param na_predict predicts missing values in the series.
#' `reg` predicts missing values by linear regression;
#' `lnreg` runs linear regression on log levels;
#' `yoy` deducts missing levels by year-over-year growth rates.
#' @param xreg regressors to predict missing values. Can be a vector
#' or a matrix. Must be the same length as the input time series.
#' If `na_predict` is set to `yoy`, must be a vector of growth rates
#' in percentage.
#' @param seas seasonal adjusting methods (see package [`seasonal`]).
#' @param seas_cny adjusting Chinese New Year effect in seasonal adjustment.
#' Must be a pair of integers indicating the start and end point of the holiday
#' around the New Year's Day. Set to `NULL` to ignore the holiday effect.
#' @param outlier deals with outliers. `rm` for removing the outliers;
#' `rp` for replacing outliers with interpolated values.
#' @param chg final transformation for the output series.
#' `log` for logarithmic transformation;
#' `d` for first-order difference;
#' `ld` stands for log-difference;
#' `pct` computes the percentage change from a period ago;
#' `yoy` computes the percentage change from a year ago;
#' `idx` indexify the time series with the first non-NA observation set to 1;
#' `ttm` computes the rolling sum of past 1 year.
#' @param na_pad whether the output series will be padded with `NA` to
#' ensure it has the same length as the input series.
#' @return Transformed series. Output series may or may not be in the same
#' length as the input series.
#' @rdname trans
#' @export
trans.ts = function(
    x,
    freq = frequency(x),
    agg = c("mean", "sum", "first", "last"),
    disagg = c("mean", "sum", "first", "last"),
    multiplier = 1,
    ytd = c("default", "diff", "diff_split"),
    na_impute = c("default", "interp", "locf", "ma", "kalman"),
    na_predict = c("default", "reg", "lnreg", "yoy"),
    xreg = NULL,
    seas = c("default", "x11", "seats", "stl"),
    seas_cny = c(-7, 7),
    chg = c("default", "log", "d", "ld", "pct", "yoy", "idx", "ttm"),
    outlier = c("default", "rm", "rp"),
    na_pad = FALSE, ...) {

  origx = x

  # unit multiplier
  if(rlang::is_scalar_double(multiplier)) {
    x = x * multiplier
  }

  # change frequency if required
  if (freq > frequency(x)) {
    # denton-cholette removes the transient movement at the beginning of series
    suppressMessages(td <- tempdisagg::td(x ~ 1, match.arg(disagg), freq, "denton"))
    x = predict(td)
  } else if (freq < frequency(x)) {
    f = switch(match.arg(agg), "mean" = mean, "sum" = sum,
               "first" = dplyr::first, "last" = dplyr::last)
    x = aggregate(x, freq, f)
  }

  # difference YTD series if required
  x = switch(match.arg(ytd), "default" = x, "diff" = ytddiff(x),
             "diff_split" = ytddiff(x, split = "avg"))

  # predict missing values by regression
  na_predict = match.arg(na_predict)
  if (na_predict == "reg" || na_predict == "lnreg") {
    stopifnot(NROW(xreg) == NROW(x))
    if (na_predict == "lnreg") {
      fit = lm(log(x) ~ log(xreg))
      pred = exp(predict(fit, newdata = log(xreg)))
    } else {
      fit = lm(x ~ xreg)
      pred = predict(fit, newdata = xreg)
    }
    x[is.na(x)] <- pred[is.na(x)]
  }
  else if (na_predict == "yoy") {
    # predict by yoy growth rates
    x = interpGR(x, xreg, direction = "both")
  }

  # impute missing values
  if (match.arg(na_impute) != "default") {
    x = na.trim(x)  # ignore leading NA for imputeTS
    x = switch(match.arg(na_impute),
      "interp" = imputeTS::na_interpolation(x),
      "kalman" = imputeTS::na_kalman(x),
      "locf" = imputeTS::na_locf(x),
      "ma" = imputeTS::na_ma(x))
  }

  # seasonal adjustment
  # note: x13seas will trim NA from the series
  x = switch(match.arg(seas), "default" = x,
    "x11" = x13seas(x, x11 = T, cny = seas_cny),
    "seats" = x13seas(x, x11 = F, cny = seas_cny),
    "stl" = stlseas(x))

  # final output transform
  y = switch (match.arg(chg),
    "default" = x,
    "log" = log(x),
    "d" = tsdiff(x),
    "ld" = tsdiff(log(x)),
    "pct" = tsgrowth(x, lag = 1),
    "yoy" = tsgrowth(x, lag = freq),
    "ttm" = rollsumr(x, freq),
    "idx" = x / na.omit(x)[1]
  )
  # clean outliers if necessary
  y = switch(match.arg(outlier), "default" = y,
    "rm" = forecast::tsclean(y, replace.missing = F),
    "rp" = forecast::tsclean(y, replace.missing = T))

  # pad the output to the same length as the original
  if (isTRUE(na_pad)) y = na.pad(y, time(origx))
  return(y)
}

#' Transform Multiple Time Series
#'
#' For multiple time series input, the same transformation will be applied
#' for each series.
#'
#' @param col_names rename the columns. Must be the same length as the number
#' of columns of the input series.
#'
#' @rdname trans
#' @export
trans.mts = function(x, ..., col_names = NULL) {
  . = NULL # silence CMD Check
  y = as.list(x) %>%
    purrr::map(function(.x) {
      do.call(trans.ts, args = list(x = .x, ...))
    }) %>%
    do.call(cbind, args = .)
  if (!is.null(col_names)) colnames(y) = col_names
  return(y)
}

#' @rdname trans
#' @export
trans.zoo = function(x,...) {
  trans(as.ts(x), ...)
}
