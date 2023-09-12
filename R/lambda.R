#' @title Converting value of loadings between standardize and non-standardized metrics
#' @description
#' `lambda2stdLoading` computes value of the non-standardized loading given
#' a value of the standardized loading while `stdLoading2lambda` computes value
#' of the standardized loading given a value of the non-standardized loading.
#' @param x value(s) of the loading(s) (standardized or non-standardized)
#' @returns a numeric vector with loading(s) (non-standardized or standardized)
#' @examples
#' lambda2stdLoading(1)
#' stdLoading2lambda(0.5)
#' lambda2stdLoading(stdLoading2lambda(0.5))
#' @rdname lambda
#' @export
lambda2stdLoading <- function(x) {
  stopifnot(is.numeric(x))
  return(x / sqrt(1 + x^2))
}
#' @rdname lambda
#' @export
stdLoading2lambda <- function(x) {
  stopifnot(is.numeric(x),
            all(x > -1), all(x < 1))
  return(x / sqrt(1 - x^2))
}
