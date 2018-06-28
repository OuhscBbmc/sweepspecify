#' @name trim
#' @aliases trim_numeric
#' @title Trim extreme values
#'
#' @description Trim extreme values from an atomic vector, and replace with a specific value (typically `NA_*`).
#'
#' @param x The input vector to be trimmed.  Required
#' @param bounds A two-element vector that establishes the lower and upper *inclusive* bounds of `x`.
#' @param replacement A scalar that will replace all instances of `x` that fall outside of `bounds`.
#'
#' @return An atomic vector with the same number of elements as `x`.
#'
#' @note
#' The data type of `x`, `bounds`, and `replacement` must match the atomic data type of the function.
#' In other words, `trim_numeric()` accepts only parameters of type 'numeric' (otherwise known as
#' 'double-precision floating point').  Likewise, `trim_date()` accepts only parameters of type `Date`.
#'
#' The lower bound must be less than or equal the upper bound.
#'
#' The default bounds for numerics and integers are at the extremes of the data type.
#' The default bounds for [dates](https://stat.ethz.ch/R-manual/R-devel/library/base/html/Dates.html) are
#' arbitrary, because the origin is slippery.
#'
#' @author Will Beasley
#'
#' @examples
#' library(sweepspecify)
#' trim_numeric(runif(10, -1, 10), bounds=c(4, 8))


#' @export
trim_numeric <- function(x, bounds=c(-Inf, Inf), replacement=NA_real_ ) {
  checkmate::assert_numeric(x, any.missing=T)
  checkmate::assert_numeric(bounds, min.len=2, max.len=2, any.missing=F)
  checkmate::assert_numeric(replacement, min.len=1, max.len=1)

  if( !(bounds[1] <= bounds[2]) )
    stop("The lower element of `bounds` must be equal or less than the upper element of `bounds`.")

  # Set values that are outside the thresholds to replacement.
  dplyr::if_else(
    condition = dplyr::between(x, bounds[1], bounds[2]),
    true      = x,
    false     = replacement
    # missing   = replacement # A future option is to set missing values to the replacement value also
  )
}
