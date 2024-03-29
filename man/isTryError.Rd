% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helpers.R
\name{isTryError}
\alias{isTryError}
\title{Checks Whether a Vector of Object Inherits from the Class 'try-error'}
\usage{
isTryError(...)
}
\arguments{
\item{...}{objects that need testing.}
}
\value{
Returns \code{TRUE} if there's some object that's a try-error, \code{FALSE} when all objects are
not try-errors.
}
\description{
Checks whether any of the provided objects contains a try error.
}
\examples{
x <- 1
y <- "a"
z <- try(integrate(exp, -Inf, Inf))
isTryError(x, y)
isTryError(x, y, z)
}
