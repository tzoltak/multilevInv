#' @title Data generation
#' @description
#' Generates a matrix with the sampled values of latent variables.
#' @param m number of groups
#' @param n number of observations within each group
#' @param ife1 value of the fixed effect the first individual-level latent
#' predictor has on the latent dependent variable
#' @param ife2 value of the fixed effect the second individual-level latent
#' predictor has on the latent dependent variable
#' @param corIfe correlation between effects described by `ife1` and `ife2`
#' @param gfeI value the fixed effect the group-level predictor has on latent
#' dependent variable
#' @param gfeS value the fixed effect the group-level predictor has on the slope
#' of the first latent dependent variable while predicting the latent dependent
#' variable
#' @param greI standard deviation of the group-level random effect on the
#' intercept (mean of the latent dependent variable)
#' @param greS standard deviation of the group-level random effect on the slope
#' of the first latent dependent variable while predicting the latent dependent
#' variable
#' @param corIS correlation of the (group-level) above random effects
#' @returns a numeric matrix with `n*m` rows and columns:
#' \describe{
#'   \item{gr}{group-membership indicator}
#'   \item{x1}{values of the first individual-level latent predictor}
#'   \item{x2}{values of the second individual-level latent predictor}
#'   \item{w}{values of the group-level predictor}
#'   \item{ri}{values of the group random intercept}
#'   \item{rs}{values of the group random slope}
#'   \item{y}{values of the latent dependent variable}
#' }
#' @export
generate_latent <- function(m, n, ife1, ife2, corIfe, gfeI, gfeS, greI, greS, corIS){
  covIfe <- matrix(c(1,      corIfe,
                     corIfe, 1),
                   nrow = 2, byrow = TRUE,
                   dimnames = list(c("x1", "x2"), c("x1", "x2")))
  x <- mnormt::rmnorm(m*n, mean = rep(0, ncol(covIfe)), varcov = covIfe)

  w <- stats::rnorm(m, mean = 0, sd = 1)

  covIS <- corIS * greI * greS
  covRE <- matrix(c(greI^2, covIS,
                    covIS,  greS^2),
                  nrow = 2, byrow = TRUE,
                  dimnames = list(c("rvI", "rvS"), c("rvI", "rvS")))
  re <- mnormt::rmnorm(m, mean = rep(0, ncol(covRE)), varcov = covRE)

  ri <- rep(gfeI*w + re[, 1], each = n)
  rs <- rep(gfeS*w + re[, 2], each = n)
  w <- rep(w, each = n)

  sdResid <- (1 - ife1^2 - ife2^2 - ife1*ife2*2*corIfe - gfeI^2 - greI^2)^0.5
  resid <- stats::rnorm(m*n, mean = 0, sd = sdResid)

  y <- ri + (ife1 + rs)*x[, 1] + ife2*x[, 2] + resid

  return(cbind(gr = rep(1:m, each = n), x, w = w, ri = ri, rs = rs, y))
}
