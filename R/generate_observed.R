#' @title Data generation
#' @description
#' Generates a matrix with the sampled values of latent variables.
#' @param latent a matrix of latent variables' values, typically returned by
#' [generate_latent]
#' @param nItems number of observed indicators (items) for each latent variable
#' @param nNonInvItems number of non-invariant items (for latent variables
#' and groups affected by the non-invariance)
#' @param lambda baseline value of items' slope parameter
#' @param tau baseline value of items' intercept parameter
#' @param difLambda optionally a value of the fixed non-invariance effect on
#' items' slope parameter
#' @param difTau optionally a value of the fixed non-invariance effect on
#' items' intercept parameter
#' @param pctGroupsAffected percent of groups affected by non-invariance
#' @param appInvLambda optionally standard deviation of the random
#' non-invariance effect on items' slope parameter
#' @param appInvTau optionally standard deviation of the random
#' non-invariance effect on items' intercept parameter
#' @param nonInv string (will be partially matched) indicating whether
#' non-invariance should affect only independent latent variables, only
#' dependent latent variable, or all of them
#' @details
#' Function enables data generation under classical (partial) non-invariance -
#' by setting parameters `difLambda` and/or `difTau` to non-zero values -
#' or under *approximate non-invariance* model - by setting `appInvLambda`
#' and/or `appInvTau`to non-zero value. Technically it is even possible to mix
#' these two kinds of effects, but it will hardly make sense theoretically.
#'
#' Additional remarks:
#' \itemize{
#'   \item{Under **approximate invariance** data-generating model
#'         **non-invariance random effects affect all items in all groups**
#'         (irrespective the values of the `nNonInvItems` and
#'         `pctGroupsAffected` parameters).}
#'   \item{Value of `lambda` is **not** standardized - data is generated using
#'         the so-called *delta* parameterization, i.e. by setting the variance
#'         of the error term to 1 in each equation generating observed
#'         indicators. Consequently, also `difLambda` and `difTau` are given
#'         in a non-standardized metric. Please note, that You may use
#'         [lambda2stdLoading] and [stdLoading2lambda] for convenient conversion
#'         between non-standardized and standardized values of loadings.}
#'   \item{If both `difLambda` and `difTau` arguments are set to non-zero
#'         values, each non-invariant item will be affected by both of the
#'         corresponding non-invariance effects.}
#'   \item{Sign of the non-invariance effect is determined randomly and
#'         **independently** for slope and intercept parameters.}
#' }
#' @returns a numeric matrix with the same number of rows as `latent` and `nItems` times 3 columns
#' @export
generate_observed <- function(latent, nItems, nNonInvItems, lambda, tau,
                              difLambda = 0, difTau = 0, pctGroupsAffected = 0,
                              appInvLambda = 0, appInvTau = 0,
                              nonInv = c("all", "independent", "dependent")) {
  nonInv <- match.arg(nonInv)
  if (nonInv == "all") {
    nonInv <- c("x1", "x2", "y")
  } else if (nonInv == "independent") {
    nonInv <- c("x1", "x2")
  } else {
    nonInv <- c("y")
  }
  gr <- latent[, colnames(latent) == "gr"]
  latent <- latent[order(gr), colnames(latent) %in% c("x1", "x2", "y")]
  gr <- cbind(gr = unique(gr), n = table(gr))
  gr <- gr[order(gr[, 1]), ]
  gr <- cbind(gr,
              isDif = as.numeric(
                gr[, 1] %in%
                  sample(gr[, 1], round(pctGroupsAffected*nrow(gr), 0))))
  observed <- latent[, rep(order(colnames(latent)), each = nItems)]
  colnames(observed) <-
    paste0("i", colnames(observed), rep(1:nItems, ncol(latent)))

  lambda <- matrix(lambda, nrow = nrow(observed), ncol = ncol(observed))
  tau <- matrix(tau, nrow = nrow(observed), ncol = ncol(observed))

  # here information about some constructs being invariant is considered
  grWithDif <- matrix(ifelse(rep(colnames(latent)[rep(order(colnames(latent)),
                                                      each = nItems)],
                                 each = nrow(gr)) %in% nonInv,
                             rep(gr[, 3], nrow(gr)*ncol(lambda)), 0),
                      nrow = nrow(gr), ncol = ncol(lambda))
  # here information about some constructs being invariant is ommited
  # (it is sufficient to have it in `biasedGr` as those will be multiplied)
  itemsWithDif <- matrix(rep(c(rep(0, nItems - nNonInvItems),
                               rep(1, nNonInvItems)),
                             nrow(gr)*ncol(latent)),
                         nrow = nItems)
  itemsWithDif <- apply(itemsWithDif, 2, sample, size = nItems)
  itemsWithDif <- matrix(itemsWithDif, byrow = TRUE,
                         nrow = nrow(gr), ncol = ncol(lambda))
  lambdaDifSign <- matrix(sample(c(-1, 1), nrow(gr)*ncol(lambda),
                                 replace = TRUE),
                          nrow = nrow(gr), ncol = ncol(lambda))
  tauDifSign <- matrix(sample(c(-1, 1), nrow(gr)*ncol(lambda),
                              replace = TRUE),
                       nrow = nrow(gr), ncol = ncol(lambda))
  lambdaWithDif <- itemsWithDif * grWithDif * lambdaDifSign
  tauWithDif <- itemsWithDif * grWithDif * tauDifSign

  lambdaWithDif <- matrix(difLambda * rep(lambdaWithDif,
                                          rep(gr[, 2], ncol(lambda))),
                          nrow = nrow(lambda), ncol = ncol(lambda))
  tauWithDif <- matrix(difTau * rep(tauWithDif,
                                    rep(gr[, 2], ncol(tau))),
                       nrow = nrow(tau), ncol = ncol(tau))

  appInvLambda <- matrix(rep(stats::rnorm(nrow(gr)*ncol(lambda), 0, appInvLambda),
                             rep(gr[, 2], ncol(lambda))),
                         nrow = nrow(lambda), ncol = ncol(lambda))
  appInvTau <- matrix(rep(stats::rnorm(nrow(gr)*ncol(tau), 0, appInvTau),
                          rep(gr[, 2], ncol(tau))),
                      nrow = nrow(tau), ncol = ncol(tau))

  resid <- matrix(stats::rnorm(nrow(observed)*ncol(observed)),
                  nrow = nrow(observed), ncol = ncol(observed))

  return(tau + tauWithDif + appInvTau +
           (lambda + lambdaWithDif + appInvLambda)*observed + resid)
}
