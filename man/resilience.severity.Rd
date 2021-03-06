% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/resilience_functions.R
\name{resilience.severity}
\alias{resilience.severity}
\title{Calculate Severity}
\usage{
resilience.severity(time_stamp, Pt, Pa, Pmax, integral_method = 2)
}
\arguments{
\item{time_stamp}{vector containing timestamp (sorted in ascending order)}

\item{Pt}{vector with performance P(t) (same length as timestamp)}

\item{Pa}{accpetable performance}

\item{Pmax}{maximal failure (worst case)}

\item{integral_method}{either 1 or 2. Switches between two different versions
of integral calculation. Both methods should return the same but method
2 should be much faster when applied to long vectors. Default is method 2.}
}
\value{
Returns severity integrated over entire time series (one number)
}
\description{
calculates severity Sev (see \href{https://www.researchgate.net/publication/326040304_Quantitative_Beschreibung_der_Resilienz_urbaner_Wassersysteme}{Matzinger et al. 2018})
of failures for time series of performance P(t). 
Entire time period is used, failure is defined by acceptable
performance Pa and maximal failure Pmax.
}
