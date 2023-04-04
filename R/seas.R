#' Seasonal Adjustment with STL
#'
#' @param x a univariate time series to be adjusted
#' @param ... arguments to be passed to [stats::stl]
#' @param plot weather a visualization of the the trend-cycle
#' decomposition will be returned
#' @return If `plot` is set to `TRUE`, a `ggplot` object will be returned;
#' otherwise, the seasonally-adjusted series.
#' @import forecast ggplot2
#' @export
stlseas = function(x, ..., plot=FALSE) {
  if(!is.ts(x)) x = as.ts(x)
  fit = forecast::mstl(x, robust=TRUE, ...)
  if (isTRUE(plot)) {
    autoplot(x, series="Data") +
      autolayer(trendcycle(fit), series="Trend") +
      autolayer(seasadj(fit), series="S.A.") +
      scale_colour_manual(values=c("gray","blue","red"),
                          breaks=c("Data","S.A.","Trend"))
  } else {
    return(forecast::seasadj(fit))
  }
}

#' Seasonal Adjustment with X13
#'
#' A wrapper over [seasonal::seas] adjusting the Chinese New Year effect.
#'
#' @param x input time series
#' @param x11 whether to use X11 or SEATS (default)
#' @param cny parameters for the Chinese New Year holiday. If set to `NULL`
#' (default), no holiday effect will be adjusted. Otherwise, a pair of
#' integers indicating the start and end point of the holiday around
#' the New Year's Day.
#' @param ... other arguments to be passed to [seasonal::seas]
#' @param final whether to return the finalized (seasonally-adjusted)
#' series or the object of class "seas".
#' @return Returns the seasonally-adjusted series if `final` is set to `TRUE`;
#' otherwise the object of class "seas".
#' @export
x13seas = function(x, x11 = FALSE, cny = NULL, ..., final = TRUE) {
  if(!is.ts(x)) x = as.ts(x)
  # na_pos = is.na(x)
  args = list(x=x, ...)
  # replace NA with outliers otherwise seas would not run
  # args$x = replace(x, na_pos, max(x, na.rm=T)*10)
  if (!is.null(cny)) {
    stopifnot(is.numeric(cny))
    # Lunar New Year regressors monthly
    holiday = seasonal::genhol(seasonal::cny, start = cny[1], end = cny[2],
                               frequency = frequency(x), center = "calendar")
    args$xreg = holiday
    args$regression.usertype = "holiday"
  }
  if (isTRUE(x11)) args$x11 = ""
  # suppress message: Model used in SEATS is different
  suppressMessages(fit <- do.call(seasonal::seas, args))
  if (isTRUE(final)) return(seasonal::final(fit))
  else return(fit)
}

