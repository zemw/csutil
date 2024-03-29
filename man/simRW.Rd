% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{simRW}
\alias{simRW}
\title{Simulate a Random Walk}
\usage{
simRW(n, drift = 0, sd = 1, freq = 1, start = 2000)
}
\arguments{
\item{n}{number of observations}

\item{drift}{drift term}

\item{sd}{standard deviation of the noise}

\item{freq}{frequency of the time series}

\item{start}{time of the first observation}
}
\value{
an \code{ts} object.
}
\description{
Simulate a Random Walk
}
