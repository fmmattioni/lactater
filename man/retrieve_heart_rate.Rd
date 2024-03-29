% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/retrieve-values.R
\name{retrieve_heart_rate}
\alias{retrieve_heart_rate}
\title{Retrieve heart rate}
\usage{
retrieve_heart_rate(raw_data, intensity_value)
}
\arguments{
\item{raw_data}{The raw data.}

\item{intensity_value}{The intensity value.}
}
\value{
the heart rate associated with the estimated lactate threshold
}
\description{
Retrieves the heart rate associated to a given intensity value.
It is mainly for internal use but it is exported for possible extensions.
}
\keyword{internal}
