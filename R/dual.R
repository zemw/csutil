#' Dual-Axis Plot
#'
#' A wrapper over [plotrix::twoord.plot()] to plot two time series
#' on different axis.
#'
#' @param y1 `ts` series (left)
#' @param y2 `ts` series (right)
#' @param ... parameters of [plotrix::twoord.plot()]
#'
#' @return nil
#' @export
dualplot = function(y1, y2, ...) {

  stopifnot(stats::is.ts(y1) && stats::is.ts(y2))

  x1 = as.numeric(time(y1))
  x2 = as.numeric(time(y2))
  y1 = as.numeric(y1)
  y2 = as.numeric(y2)

  args = list(lx = x1, ly = y1, rx = x2, ry = y2)
  args = c(args, list(...))

  do.call(twoord.plot, args)
}
