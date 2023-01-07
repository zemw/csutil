#' Dual-Axis Plot
#'
#' A wrapper over [plotrix::twoord.plot] to plot two time series
#' on different axis.
#'
#' @param y1 `ts` series (left)
#' @param y2 `ts` series (right)
#' @param ... parameters of [plotrix::twoord.plot]
#'
#' @return nil
#' @export
dualplot = function(y1, y2, ...) {
  x1 = as.numeric(time(y1))
  x2 = as.numeric(time(y2))
  y1 = as.numeric(y1)
  y2 = as.numeric(y2)

  args = list(lx = x1, ly = y1, rx = x2, ry = y2)
  args = c(args, list(...))

  do.call(twoord.plot, args)
}


#' Plot Multiple Time Series
#'
#' A wrapper over [zoo::plot.zoo] to plot multiple time series.
#'
#' @param y an `mts` object
#' @param ... parameters of [zoo::plot.zoo]
#' @seealso [zoo::plot.zoo]
#' @export
multiplot = function(y, ...) {
  plot.zoo(y, ...)
}
