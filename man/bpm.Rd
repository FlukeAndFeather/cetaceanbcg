% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{bpm}
\alias{bpm}
\title{Calculate beats-per-minute}
\usage{
bpm(beats, dt)
}
\arguments{
\item{beats}{Logical vector indicating heart beats}

\item{dt}{POSIXct vector of timestamps}
}
\value{
Where \code{beats} is TRUE, the BPM. Otherwise NA.
}
\description{
Calculate beats-per-minute
}
