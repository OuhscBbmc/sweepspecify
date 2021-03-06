% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/trim.R
\name{trim}
\alias{trim}
\alias{trim_numeric}
\title{Trim extreme values}
\usage{
trim_numeric(x, bounds = c(-Inf, Inf), replacement = NA_real_)
}
\arguments{
\item{x}{The input vector to be trimmed.  Required}

\item{bounds}{A two-element vector that establishes the lower and upper \emph{inclusive} bounds of \code{x}.}

\item{replacement}{A scalar that will replace all instances of \code{x} that fall outside of \code{bounds}.}
}
\value{
An atomic vector with the same number of elements as \code{x}.
}
\description{
Trim extreme values from an atomic vector, and replace with a specific value (typically \code{NA_*}).
}
\note{
The data type of \code{x}, \code{bounds}, and \code{replacement} must match the atomic data type of the function.
In other words, \code{trim_numeric()} accepts only parameters of type 'numeric' (otherwise known as
'double-precision floating point').  Likewise, \code{trim_date()} accepts only parameters of type \code{Date}.

The lower bound must be less than or equal the upper bound.

The default bounds for numerics and integers are at the extremes of the data type.
The default bounds for \href{https://stat.ethz.ch/R-manual/R-devel/library/base/html/Dates.html}{dates} are
arbitrary, because the origin is slippery.
}
\examples{
library(sweepspecify)
trim_numeric(runif(10, -1, 10), bounds=c(4, 8))
}
\author{
Will Beasley
}
