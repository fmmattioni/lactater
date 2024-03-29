% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/retrieve-values.R
\name{retrieve_lactate}
\alias{retrieve_lactate}
\title{Retrieve lactate}
\usage{
retrieve_lactate(model, intensity_value)
}
\arguments{
\item{model}{The model chosen in \code{fit}.}

\item{intensity_value}{The intensity value.}
}
\value{
the lactate associated with the estimated lactate threshold
}
\description{
Retrieves the lactate associated to a given intensity value.
It is mainly for internal use but it is exported for possible extensions.
}
\keyword{internal}
