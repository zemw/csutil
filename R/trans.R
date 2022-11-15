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
#' @param agg aggregating method for frequency transformation.
#' @param unit a single number to scale up or down the values.
#' @param disYTD set to `TRUE` for YTD series to convert it back to monthly
#' or quarterly level series.
#' @param naFill methods to impute missing values (see package [`imputeTS`])
#' @param naReg impute missing values by a set of regressors. Can be a vector
#' or a matrix. Must be the same length as the input time series.
#' @param naYoY impute missing values by YoY growth rates. Must be a vector
#' (in percentage) with the same length as the input time series.
#' @param seasAdj seasonal adjusting methods (see package [`seasonal`])
#' @param hpFilter applying (boosted) HP filter (see package [`bHP`])
#' @param chg transform the output series.
#' `log` for logarithmic transformation;
#' `diff` for first-order difference;
#' `ld` stands for log-difference;
#' `pct` computes the percentage change from a period ago;
#' `yoy` computes the percentage change from a year ago;
#' `idx` indexify the time series with the first non-NA observation set to 1;
#' `ttm` computes the rolling sum of past 1 year.
#'
#' @return the transformed series. Output series are returned in the same
#' length as the input series if no frequency change applied.
#'
#' @import stats
#' @rdname trans
#' @export
trans.ts = function(
    x,
    freq = c("asis", "y", "q", "m"),
    agg = c("avg", "sum", "last"),
    unit = 1,
    disYTD = FALSE,
    naFill = c("none", "interp", "locf", "ma", "kalman"),
    naReg = numeric(0),
    naYoY = numeric(0),
    seasAdj = c("none", "x11", "x13"),
    hpFilter = c("none", "trend", "cycle"),
    chg = c("asis", "log", "diff", "ld", "pct", "yoy", "idx", "ttm"),
    ...) {

  stopifnot(is.ts(x))

  # unit multiplier
  if(rlang::is_scalar_double(unit)) {
    x = x * unit
  }

  # change frequency
  freq = match.arg(freq)
  nfreq = switch (freq, "y" = 1, "q" = 4, "m" = 12, frequency(x))
  if (nfreq > frequency(x)) {
    # stop("Cannot aggregate to higher frequency.")
    tmp = ts(NA, start = time(x)[1], end = time(x)[length(x)], nfreq)
    suppressWarnings(tmp <- zoo::merge.zoo(tmp, x))
    x = as.ts(tmp[,2])
  }
  if (nfreq < frequency(x)) {
    agg = match.arg(agg)
    fun = function(.x, method = agg) {
      switch (
        method,
        "avg" = mean(.x),
        "sum" = sum(.x),
        "last" = utils::tail(.x, n = 1)
      )
    }
    x = aggregate(x, nfreq, fun)
  }

  # output series
  y = x

  # disaggregate YTD series
  if (isTRUE(disYTD) && nfreq %in% c(4,12)) {
    for (i in 1:length(x)) {
      if (cycle(x)[i] == 1)
        y[i] = ifelse(i==length(x), NA_real_, x[i])
      # avoid possible double counting in Jan/Feb
      else if (cycle(x)[i] == 2 && i > 1 && identical(x[i], x[i-1]))
        y[(i-1):i] = NA_real_
      else if (i > 1)
        y[i] = x[i] - x[i - 1]
      else
        y[i] = NA_real_
    }
  }

  # impute missing values by regression
  if (is.numeric(naReg) && length(naReg) > 0) {
    if(NROW(naReg) != NROW(y)) {
      stop("Regressors have different length.")
    }
    fit = predict(lm(y ~ naReg), newdata = naReg)
    for (i in 1:length(y)) {
      if (is.na(y[i])) y[i] = fit[i]
    }
  }

  # impute missing values by YoY growth rates
  if (is.numeric(naYoY) && length(naYoY) > 0) {
    if (length(naYoY) != length(y)) {
      stop("YoY series has different length.")
    }
    # nfreq = frequency(y)
    # filling forward
    for (i in (nfreq + 1):length(y)) {
      if (is.na(y[i]) && !is.na(y[i - nfreq]) && !is.na(naYoY[i])) {
        y[i] = y[i - nfreq] * (1 + naYoY[i] / 100)
      }
    }
    # filling backward
    for (i in (length(y) - nfreq):1) {
      if (is.na(x[y]) && !is.na(y[i + nfreq]) && !is.na(naYoY[i + nfreq])) {
        y[i] = y[i + nfreq] / (1 + naYoY[i + nfreq] / 100)
      }
    }
  }

  ## skip leading and trailing NAs for imputation and seasAdj
  x = y  # back up series before trimming
  y = zoo::na.trim(y)

  # impute missing values
  naFill = match.arg(naFill)
  y = switch(
    naFill,
    "interp" = imputeTS::na_interpolation(y),
    "kalman" = imputeTS::na_kalman(y),
    "locf" = imputeTS::na_locf(y),
    "ma" = imputeTS::na_ma(y),
    y
  )

  # seasonal adjustment
  seasAdj = match.arg(seasAdj)
  if (seasAdj != "none") {
    na_pos = is.na(y) # replace NA with outliers
    tmp = replace(y, na_pos, 99999)
    # Chinese Lunar New Year regressors
    xreg = cbind(
      seasonal::genhol(seasonal::cny, start = -7, end = -1, center = "calendar"),
      seasonal::genhol(seasonal::cny, start = 0, end = 7, center = "calendar")
    )
    seasObj = seasonal::seas(tmp, xreg, regression.usertype = "holiday")
    if (seasAdj == "x11") update(seasObj, x11 = "")
    y = seasonal::final(seasObj) # seasonally adjusted series
    y[na_pos] <- NA
  }

  # boosted HP filtering
  hpFilter = match.arg(hpFilter)
  if (hpFilter != "none") {
    lambda = switch (
      as.character(nfreq),
      "1" = 6.25,
      "4" = 1600,
      "12" = 129600
    )
    bhp = bHP::BoostedHP(y, lambda)
    y = switch (hpFilter, "trend" = bhp$trend, "cycle" = bhp$cycle)
  }

  # output transform
  chg = match.arg(chg)
  y = switch (chg,
    "asis" = y,
    "log" = log(y),
    "diff" =diff(y),
    "ld" = diff(log(y)),
    "pct" = 100*(y/lag(y, -1)-1),
    "yoy" = 100*(y/lag(y, -nfreq)-1),
    "ttm" = zoo::rollsum(y, nfreq, align = "right"),
    "idx" = y / na.omit(y)[1]
  )

  # recover leading and trailing NAs
  # make sure y maintain the same tsp
  if (length(y) != length(x)) {
    tmp = zoo::merge.zoo(y, x)
    y = as.ts(tmp[,1])
  }
  return(y)
}

#' Transform Multiple Time Series
#'
#' For multiple time series input, the same transformation will be applied
#' for each series.
#'
#' @param colNames rename the columns. Must be the same length as the number
#' of columns of the input series.
#'
#' @rdname trans
#' @export
trans.mts = function(x, ..., colNames = NULL) {
  stopifnot(is.mts(x))
  . = NULL # silence CMD Check
  y = as.list(x) %>%
    purrr::map(function(.x) {
      do.call(trans.ts, args = list(x = .x, ...))
    }) %>%
    do.call(cbind, args = .)
  if (!is.null(colNames)) colnames(y) = colNames
  return(y)
}
