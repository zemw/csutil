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
#' @param unit a single number to scale up or down the values.
#' @param YTD methods to disaggregate YTD series into monthly or quarterly
#' series. `d` for consecutive differencing within a year. Due to the Lunar
#' New Year (LNY) effect. NBS usually reports January and February observations
#' together (as the sum of the two month). This results in duplicated or
#' missing values for the first two months and distorts the normal time series.
#' Specify `rm` to remote the values for the first two months; `split` to split
#' the sum and accredit the values to Jan and Feb according to the LNY holiday
#' patterns; `avg` to split the sum equally to each month.
#' @param na_impute methods to impute missing values (see package [`imputeTS`])
#' @param na_predict predicts missing values by regression.
#' `yoy` predicts missing levels by YoY growth rates;
#' `reg` predicts missing values by linear regression;
#' `lnreg` runs linear regression on log levels.
#' @param xreg regressors to predict missing values. Can be a vector
#' or a matrix. Must be the same length as the input time series.
#' If `na_predict` is specified as `yoy`, must be a vector of growth rates
#' in percentage.
#' @param seas seasonal adjusting methods (see package [`seasonal`]).
#' `x11a` and `x13a` include additional regressors to adjust for lunar
#' new year holiday.
#' @param outlier deals with outliers. `rm` for removing the outliers;
#' `rpl` for replacing outliers with interpolated values.
#' @param hp_filter applying (boosted) HP filter (see package [`bHP`])
#' @param chg transform the output series.
#' `log` for logarithmic transformation;
#' `d` for first-order difference;
#' `ld` stands for log-difference;
#' `pct` computes the percentage change from a period ago;
#' `yoy` computes the percentage change from a year ago;
#' `idx` indexify the time series with the first non-NA observation set to 1;
#' `ttm` computes the rolling sum of past 1 year.
#'
#' @return Transformed series. Output series are returned in the same
#' length as the input series if no frequency change applied.
#'
#' @import stats
#' @rdname trans
#' @export
trans.ts = function(
    x,
    freq = c("asis", "y", "q", "m"),
    agg = c("avg", "sum", "first", "last"),
    disagg = c("sum", "mean", "first", "last"),
    unit = 1,
    YTD = c("none", "d", "rm", "split", "avg"),
    na_impute = c("none", "interp", "locf", "ma", "kalman"),
    na_predict = c("none", "yoy", "reg", "lnreg"),
    xreg = NULL,
    seas = c("none", "x11", "x13", "x11a", "x13a"),
    hp_filter = c("none", "trend", "cycle"),
    chg = c("asis", "log", "d", "ld", "pct", "yoy", "idx", "ttm"),
    outlier = c("asis", "rm", "rpl"),
    ...) {

  origx = x

  # unit multiplier
  if(rlang::is_scalar_double(unit)) {
    x = x * unit
  }

  # change frequency
  freq = match.arg(freq)
  nfreq = switch (freq, "y" = 1, "q" = 4, "m" = 12, frequency(x))
  if (nfreq > frequency(x)) {
    # 'denton-cholette' removes the transient movement at the beginning of series
    suppressMessages(td <- tempdisagg::td(x ~ 1, match.arg(disagg), nfreq, "denton"))
    x = predict(td)
  }
  else if (nfreq < frequency(x)) {
    agg = match.arg(agg)
    fun = function(.x, method = agg) {
      switch (
        method,
        "avg" = mean(.x),
        "sum" = sum(.x),
        "first" = utils::head(.x, n = 1),
        "last" = utils::tail(.x, n = 1)
      )
    }
    x = aggregate(x, nfreq, fun)
  }

  # output series
  y = x

  # disaggregate YTD series if required
  transYTD = match.arg(YTD)
  if (transYTD != "none" && (nfreq %in% c(4,12))) {
    # take difference within each year
    for (i in 1:length(x)) {
      if (cycle(x)[i] == 1) y[i] = x[i]
      else if (i > 1) y[i] = x[i] - x[i-1]
      else y[i] = NA_real_
    }
    # fix the distortion caused by Lunar New Year
    if (transYTD == "rm" && nfreq == 12) {
      # simply remove Jan / Feb observations
      isJanFeb = cycle(x) == 1 | cycle(x) == 2
      y[isJanFeb] = NA_real_
    }
    # The YTD value for Feb is the sum of Jan and Feb
    # split the sum by half and half as the values for
    # Jan and Feb respectively
    if (transYTD == "avg" && nfreq == 12) {
      for (i in 1:length(x)) {
        if (cycle(x)[i] == 2) {
          y[i] = x[i] / 2
          if (i > 1)
            y[i-1] = x[i] / 2
        }
      }
    }
    # split the sum according to holiday patterns
    if (transYTD == "split" && nfreq == 12) {
      # lunar new year regressor
      lny = seasonal::genhol(seasonal::cny, -7, 7, nfreq, "calendar")
      lny = window(lny, start(x), end(x))
      for (i in 1:length(x)) {
        if (cycle(x)[i] == 2) {
          # this formula is based on the regression of other series
          # on the impact of lunar new year
          # the more severe the impact on Feb, the less the proportion
          # of value accredited to Feb as opposed to Jan
          s = pnorm(.0556 - 0.206 * lny[i])
          y[i] = x[i] * s
          if (i > 1)
            y[i-1] = x[i] * (1-s)
        }
      }
    }
  }

  # predict missing values by regression
  na_predict = match.arg(na_predict)
  if (na_predict == "reg" || na_predict == "lnreg") {
    if(NROW(xreg) != NROW(y))
      stop("Regressors must not have different length")
    if (na_predict == "lnreg") {
      fit = lm(log(y) ~ log(xreg))
      pred = exp(predict(fit, newdata = log(xreg)))
    } else {
      fit = lm(y ~ xreg)
      pred = predict(fit, newdata = xreg)
    }
    y[is.na(y)] <- pred[is.na(y)]
  }

  # predict missing values by YoY growth rates
  if (na_predict == "yoy") {
    if (NROW(xreg) != NROW(y))
      stop("YoY series must not has different length")
    # nfreq = frequency(y)
    # filling forward
    for (i in (nfreq + 1):length(y)) {
      if (is.na(y[i]) && !is.na(y[i - nfreq]) && !is.na(xreg[i])) {
        y[i] = y[i - nfreq] * (1 + xreg[i] / 100)
      }
    }
    # filling backward
    for (i in (length(y) - nfreq):1) {
      if (is.na(x[i]) && !is.na(y[i + nfreq]) && !is.na(xreg[i + nfreq])) {
        y[i] = y[i + nfreq] / (1 + xreg[i + nfreq] / 100)
      }
    }
  }

  ## skip leading and trailing NAs for imputation and seasAdj
  x = y  # back up series before trimming
  y = zoo::na.trim(y)

  # impute missing values
  na_impute = match.arg(na_impute)
  y = switch(
    na_impute,
    "interp" = imputeTS::na_interpolation(y),
    "kalman" = imputeTS::na_kalman(y),
    "locf" = imputeTS::na_locf(y),
    "ma" = imputeTS::na_ma(y),
    y
  )

  # seasonal adjustment
  seas = match.arg(seas)
  if (seas != "none") {
    na_pos = is.na(y)
    # replace NA with outliers otherwise seas would not run
    tmp = replace(y, na_pos, 999999)
    args = list(x=tmp)
    if (seas %in% c("x11a", "x13a") && nfreq == 12) {
      # Chinese Lunar New Year regressors monthly
      xreg = cbind(
        seasonal::genhol(seasonal::cny, start = -7, end = -1, center = "calendar"),
        seasonal::genhol(seasonal::cny, start = 0, end = 7, center = "calendar")
      )
      args$xreg = xreg
      args$regression.usertype = "holiday"
    }
    if (seas %in% c("x11", "x11a")) {
      args$x11 = ""
    }
    # suppress message: Model used in SEATS is different
    suppressMessages(seasObj <- do.call(seasonal::seas, args))
    y = seasonal::final(seasObj) # seasonally adjusted series
    y[na_pos] = NA  # put NA back
  }

  # boosted HP filtering
  hp_filter = match.arg(hp_filter)
  if (hp_filter != "none") {
    lambda = switch (
      as.character(nfreq),
      "1" = 6.25,
      "4" = 1600,
      "12" = 129600
    )
    bhp = bHP::BoostedHP(y, lambda)
    y = switch (hp_filter, "trend" = bhp$trend, "cycle" = bhp$cycle)
  }

  # output transform
  chg = match.arg(chg)
  y = switch (chg,
    "asis" = y,
    "log" = log(y),
    "d" = diff(y),
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

  # clean outliers if necessary
  outlier = match.arg(outlier)
  if (outlier == "rm") {
    y = forecast::tsclean(y, replace.missing = F)
  } else if (outlier == "rpl") {
    y = forecast::tsclean(y, replace.missing = T)
  }

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
