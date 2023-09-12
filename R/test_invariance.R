#' @title Model estimation
#' @description
#' Performs non-invariance tests for a specific construct using results of the
#' estimation.
#' @param mplusModel an Mplus model object returned by [estimate_invariance]
#' @param suffix a string - suffix that will be added to the names of the
#' returned columns (typically describing name of the construct)
#' @returns a data frame
test_invariance <- function(mplusModel, suffix = "") {
  stats <- mplusModel$results$summaries
  if (length(stats) == 0L) {
    stats <- mplusModel$results$output
    nObs <- stats[grep("Number of observations", stats)]
    nObs <- as.integer(sub("^[^[:digit:]]+", "", nObs))
    models <- stats[grep("MODEL FIT INFORMATION FOR THE (CONFIGURAL|METRIC|SCALAR) MODEL",
                         stats)]
    models <- sub("MODEL FIT INFORMATION FOR THE (CONFIGURAL|METRIC|SCALAR) MODEL",
                  "\\1", models)
    parameters <- stats[grep("(Configural|Metric|Scalar) +[[:digit:]]+ +-[[:digit:]]+",
                             stats)]
    parameters <- as.integer(sub("^.*(Configural|Metric|Scalar) +([[:digit:]]+) +.*$",
                                 "\\2", parameters))
    chisq <- stats[grep("Metric against Configural|Scalar against Metric", stats)]
    df <- as.integer(sub("^.* against (Configural|Metric) +[[:digit:]\\.]+ +([[:digit:]]+) +.*$",
                         "\\2", chisq))
    chisq <- as.numeric(sub("^.* against (Configural|Metric) +([[:digit:]\\.]+) +[[:digit:]]+ +.*$",
                            "\\2", chisq))
    stats <- stats[grep("(H0|AIC|BIC)", stats)]
    stats <- data.frame(Model = models,
                        Parameters = parameters,
                        LL = as.numeric(sub("^.* ", "",
                                            stats[grep("H0 Value", stats)])),
                        LLCorrectionFactor =
                          as.numeric(sub("^.* ", "",
                                         stats[grep("H0 Scaling Correction Factor",
                                                    stats)])),
                        AIC = as.numeric(sub("^.* ", "",
                                             stats[grep("AIC", stats)])),
                        BIC = as.numeric(sub("^.* ", "",
                                             stats[grep("\\(BIC\\)", stats)])),
                        aBIC = as.numeric(sub("^.* ", "",
                                              stats[grep("Adjusted BIC", stats)])),
                        AICC = NA_real_,
                        ChiSquared = c(chisq, NA_real_),
                        df = c(df, NA_integer_))
    stats$AICC <- stats$AIC + 2*stats$Parameters*(stats$Parameters + 1) /
      (nObs - stats$Parameters - 1)
  } else {
    stats$ParamCorr <- stats$Parameters * stats$LLCorrectionFactor
    stats$ChiSquared <- c(
      with(stats,
           2*(LL[-nrow(stats)] - LL[-1]) /
             (ParamCorr[-nrow(stats)] - ParamCorr[-1]) *
             (Parameters[-nrow(stats)] - Parameters[-1])),
      NA)
    stats$df <- c(with(stats, Parameters[-nrow(stats)] - Parameters[-1]), NA)
    stats$Model = sub(" MODEL", "", stats$Model)
  }
  stats <- stats[, c("Model", "Parameters", "AIC", "BIC", "aBIC", "AICC",
                     "ChiSquared", "df")]
  stats$id <- 1
  stats <- stats::reshape(stats, timevar = "Model",
                          direction = "wide", sep = "_")[, -1]
  stats <- stats[, !sapply(stats, is.na)]
  if (suffix != "") {
    names(stats) <- paste(names(stats), suffix, sep = "_")
  }
  return(stats)
}
