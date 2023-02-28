#' Seasonal Adjustment with STL
#'
#' @param x univariate time series to be adjusted
#' @param ... arguments to be passed to [stats::stl]
#' @return the seasonally-adjusted series.
#' @importFrom forecast mstl
#' @export
stlseas = function(x, ...) {
  m = mstl(x, ...)
  return(m[, 'Trend'] + m[, 'Remainder'])
}

#' Plot STL-seasonally-adjusted series
#'
#' The original series will be plotted together with the seasonally-adjusted
#' series and the trend.
#'
#' @param x original univariate time series
#' @param ... arguments to be passed to [stats::stl]
#' @return An `ggplot` object.
#' @importFrom forecast mstl
#' @importFrom zoo fortify.zoo
#' @importFrom dplyr mutate
#' @importFrom rlang .data
#' @import ggplot2
#' @export
seasplot = function(x, ...) {
    fortify.zoo(mstl(x, ...)) |>
    mutate(SAdj = .data$Trend + .data$Remainder) |>
    ggplot(aes(x = .data$Index)) +
    geom_line(aes(y = .data$SAdj, col = "S.A.")) +
    geom_line(aes(y = .data$Trend, col = "Trend")) +
    geom_line(aes(y = .data$Data, col = "Original"), alpha=.5) +
    scale_color_manual(values = c(8,1,2)) +
    scale_x_continuous(breaks = pretty) +
    scale_y_continuous(breaks = pretty) +
    labs(x = NULL, y = NULL)
}
