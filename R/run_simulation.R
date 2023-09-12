#' @title Simulation flow
#' @description
#' Runs the simulation.
#' @param conditions a data frame with simulation conditions
#' @param nIterPerCond a positive integer - number of iterations to be run for
#' each condition
#' @param suffix optionally a string - suffix that will be added to the name
#' of a file storing simulation results (that will be saved to the disk)
#' @param estimateMLR a logical value - whether to estimate models using the
#' MLR approach?
#' @param estimateBayes a logical value - whether to estimate models using the
#' Bayesian approach?
#' @param estimateObserved a logical value - whether to estimate models using
#' the approach consisting first estimating CFAs, then obtaining factor scores
#' and finally estimating observed-scores multilevel regression?
#' @returns (invisibly) a data frame with results
#' Moreover, after completing each simulation condition-iteration function will
#' save results to the file "multilevelNonInvarianceResults\[suffix\].csv".
#' @examples
#' \dontrun{
#' str(conditions)
#' set.seed(12345)
#' run_simulation(conditions[1:2, ], nIterPerCond = 1L)
#' }
#' @export
run_simulation <- function(conditions, nIterPerCond, suffix = "",
                           estimateMLR = TRUE, estimateBayes = FALSE,
                           estimateObserved = FALSE) {
  check_conditions(conditions)
  stopifnot(is.numeric(nIterPerCond), length(nIterPerCond) == 1L,
            as.integer(nIterPerCond) == nIterPerCond, nIterPerCond > 0L,
            is.character(suffix), length(suffix) == 1L, !anyNA(suffix),
            is.logical(estimateMLR), length(estimateMLR) == 1L,
            estimateMLR %in% c(TRUE, FALSE),
            is.logical(estimateBayes), length(estimateBayes) == 1L,
            estimateBayes %in% c(TRUE, FALSE),
            is.logical(estimateObserved), length(estimateObserved) == 1L,
            estimateObserved %in% c(TRUE, FALSE))
  # it is easy to think about correlaction between random componenets of Y on X1
  # intercept and slope as a simulation condition but it is burdensome to check
  # confidence intervals coverage for correlation - for covariance this is
  # straightforward, because this is simply a model parameter
  conditions$covIS <- with(conditions, corIS * greI * greS)

  # multiplying conditions by the number of iterations
  nCond <- nrow(conditions)
  conditions <- cbind(as.data.frame(lapply(conditions, rep, times = nIterPerCond)),
                      iter = rep(seq_len(nIterPerCond), each = nCond))
  # adding columnd for parameters to record
  namesParRecord <- c("ife1", "ife2", "corIfe", "gfeI", "gfeS", "greI", "greS",
                      "covIS")
  newColumns <- c()
  if (estimateMLR) {
    newColumns <- c(newColumns, "mlr_errors", "mlr_warnings",
                    paste0(rep(namesParRecord, each = 3),
                           c("_mlr_pe", "_mlr_l95ci", "_mlr_u95ci")),
                    paste0(rep(paste0(rep(c("Parameters", "AIC", "BIC", "aBIC",
                                            "AICC", "ChiSquared", "df"), 3), "_",
                                      rep(c("CONFIGURAL", "METRIC", "SCALAR"),
                                          each = 7))[-c(20, 21)], 3), "_",
                           rep(c("x1", "x2", "y"), each = 19)))
  }
  if (estimateBayes) {
    newColumns <- c(newColumns, "bayes_errors", "bayes_warnings",
                    paste0(rep(namesParRecord, each = 3),
                           c("_bayes_pe", "_bayes_l95pi", "_bayes_u95pi")))
  }
  if (estimateObserved) {
    newColumns <- c(newColumns, "observed_errors", "observed_warnings",
                    paste0(rep(setdiff(namesParRecord, "corIfe"), each = 3),
                           c("_obs_pe", "_obs_l95ci_prof", "_obs_u95ci_prof")),
                    paste0(rep(setdiff(grep("fe", namesParRecord, value = TRUE),
                                       "corIfe"),
                               each = 2),
                           c("_obs_l95ci_appr", "_obs_u95ci_appr")))
  }
  conditions <- cbind(conditions, matrix(NA_real_, nrow = nrow(conditions),
                                         ncol = length(newColumns),
                                         dimnames = list(NULL,
                                                         newColumns)))

  for (i in seq_len(nrow(conditions))) {
    cat("\n###########################################################################\n Simulation iteration ",
        conditions$iter[i], " (out of ", nIterPerCond,
        "),\n condition number ", ifelse((i %% nCond) == 0, nCond, i %% nCond),
        " (out of ", nCond,")\n\n",
        sep = "")
    print(as.data.frame(conditions[i, get_required_conditions_names()]), row.names = FALSE)

    latent <- with(conditions[i, ],
                   generate_latent(m = m, n = n,
                                   # individual level fixed effects
                                   ife1 = ife1, ife2 = ife2, corIfe = corIfe,
                                   # group level fixed effects
                                   gfeI = gfeI, gfeS = gfeS,
                                   # [group level] random effects
                                   greI = greI, greS = greS, corIS = corIS))
    observed <- with(conditions[i, ],
                     generate_observed(latent,
                                       # number of items of each latent trait
                                       nItems = nItems, nNonInvItems = nNonInvItems,
                                       # unstnadardized loading and intercepp
                                       lambda = lambda, tau = tau,
                                       # magnitude of bias for lambda and tau
                                       difLambda = difLambda, difTau = difTau,
                                       # pct. of affected groups should be specified between 0 and 1
                                       pctGroupsAffected = pctGroupsAffected,
                                       # standard deviations of normal distributions used to
                                       # generate approximate invariance
                                       appInvLambda = appInvLambda,
                                       appInvTau = appInvTau,
                                       # whether non-invariance affects dependent,
                                       # independent or all the latent variables
                                       nonInv = nonInv))
    data <- cbind(as.data.frame(observed),
                  latent[, colnames(latent) %in% c("gr", "w")] )
    varResidY <- with(conditions[i, ],
                      1 - ife1^2 - ife2^2 - ife1*ife2*2*corIfe - gfeI^2 - greI^2)
    if (estimateMLR) {
      cat("\nEstimating MLR model")
      startTime <- Sys.time()
      mlr <- with(conditions[i, ],
                  estimate_sem(data, "mlr", varResidY = varResidY,
                               ife1Start = ife1, ife2Start = ife2,
                               covIfeStart = corIfe,
                               gfeIStart = gfeI, gfeSStart = gfeS,
                               greIStart = greI, greSStart = greS,
                               covISStart = corIS * greI * greS))
      cat("  ", lasted(startTime), "\n", sep = "")
      startTime <- Sys.time()
      cat("Estimating invariance test for separate constructs")
      invarianceMlr <- list(
        x1 = estimate_invariance(data[, grep("(^ix1)|^gr$", names(data))]),
        x2 = estimate_invariance(data[, grep("(^ix2)|^gr$", names(data))]),
        y = estimate_invariance(data[, grep("(^iy)|^gr$", names(data))]))
      cat(" ", lasted(startTime), "\n", sep = "")
      # delete data file written used by Mplus
      unlink(mlr$results$input$data$file)
      unlink(invarianceMlr$x1$results$input$data$file)
      unlink(invarianceMlr$x2$results$input$data$file)
      unlink(invarianceMlr$y$results$input$data$file)

      conditions$mlr_errors[i] <-
        paste(unlist(mlr$results$errors), collapse = "\n")
      conditions$mlr_warnings[i] <-
        paste(unlist(mlr$results$warnings), collapse = "\n")
      if ("se" %in% names(mlr$results$parameters$unstandardized)) {
        conditions[i, ] <- get_mlr_results(mlr, conditions[i, ])
      } else if (conditions$mlr_errors[i] == "")  {
        conditions$mlr_errors[i] <- ("Error while reading results from Mplus output.")
      }
      invarianceMlr <- mapply(test_invariance, invarianceMlr, names(invarianceMlr),
                              SIMPLIFY = FALSE)
      invarianceMlr <- cbind(invarianceMlr$x1,
                             invarianceMlr$x2,
                             invarianceMlr$y)
      conditions[i, names(invarianceMlr)] <- invarianceMlr
    }
    if (estimateBayes) {
      cat("Estimating Bayesian model")
      startTime <- Sys.time()
      bayes <- with(conditions[i, ],
                    estimate_sem(data, "bayes", varResidY = varResidY,
                                 ife1Start = ife1, ife2Start = ife2,
                                 covIfeStart = corIfe,
                                 gfeIStart = gfeI, gfeSStart = gfeS,
                                 greIStart = greI, greSStart = greS,
                                 covISStart = corIS * greI * greS))
      cat("  ", lasted(startTime), "\n", sep = "")
      # delete data file written used by Mplus
      unlink(bayes$results$input$data$file)

      conditions$bayes_errors[i] <-
        paste(unlist(bayes$results$errors), collapse = "\n")
      conditions$bayes_warnings[i] <-
        paste(unlist(bayes$results$warnings), collapse = "\n")
      if (all(c("lower_2.5ci", "upper_2.5ci") %in%
              names(bayes$results$parameters$unstandardized))) {
        conditions[i, ] <- get_bayes_results(bayes, conditions[i, ])
      } else if (conditions$bayes_errors[i] == "") {
        conditions$bayes_errors[i] <- ("Error while reading results from Mplus output.")
      }
    }
    if (estimateObserved) {
      cat("\nGetting factor scores from unidimensional CFA models")
      startTime <- Sys.time()
      fscores <- cbind(
        data[, c("gr", "w")],
        estimate_fscores(data[, grep("(^ix1)", names(data))], "x1"),
        estimate_fscores(data[, grep("(^ix2)", names(data))], "x2"),
        estimate_fscores(data[, grep("(^iy)|^gr$", names(data))], "y"))
      cat("  ", lasted(startTime), "\n", sep = "")
      if (!all(c("x1", "x2", "y") %in% names(fscores))) {
        conditions$observed_errors[i] <- paste0("Factor scores for: ",
                                                paste(setdiff(c("x1", "x2", "y"),
                                                              names(fscores)),
                                                      collapse = ", "),
                                                " can't be obtained.")
      } else {
        fscores[, c("x1", "x2", "y")] <- scale(fscores[, c("x1", "x2", "y")],
                                               center = TRUE, scale = TRUE)
        cat("\nEstimating and profiling multilevel model")
        startTime <- Sys.time()
        lmerModel <- try(lme4::lmer(y ~ x1*w + x2 + (1 + x1 | gr),
                                    data = fscores, REML = TRUE))
        if (inherits(lmerModel, "try-error")) {
          conditions$observed_errors[i] <- lmerModel
        } else {
          conditions$observed_warnings[i] <- paste(lmerModel@optinfo$warnings,
                                                   collapse = "\n")
          conditions[i, ] <- get_lmer_results(lmerModel, conditions[i, ])
          cat("  ", lasted(startTime), "\n", sep = "")
        }
      }
    }

    utils::write.csv(conditions, paste0("multilevelNonInvarianceResults", suffix, ".csv"),
                     row.names = FALSE, na = "")
  }
  invisible(conditions)
}
