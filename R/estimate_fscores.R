#' @title Model estimation
#' @description
#' Estimates a unidimensional CFA model in Mplus (using the [MplusAutomation]
#' package) for a given construct and saves factor scores.
#' @param data a data frame generated with values of the generated observed
#' indicators (which names are assumed to start from "i") of a single latent
#' variable and a group-membership indicator (named "gr")
#' @param name a one-column data frame with factor scores or `NULL` if factors
#' scores can't be obtained (probably because of model non-convergence)
#' @returns an Mplus model object, see [MplusAutomation::mplusModeler]
estimate_fscores <- function(data, name) {
  model <- MplusAutomation::mplusObject(
    TITLE = paste("Unidimensional CFA:", name),
    VARIABLE = paste0("
  USEVAR = ", paste(grep("^i", names(data), value = TRUE), collapse = " "), ";
  CLASSES = g(", length(unique(data$gr)), ");
  KNOWNCLASS = g(gr);"),
  ANALYSIS = "
  TYPE = MIXTURE;
  ESTIMATOR = MLR;
  PROCESSORS = 8;",
  MODEL = paste0("
%OVERALL%
  ", name, " by ", sub(" ", "* ",
                       paste(grep("^i", names(data), value = TRUE),
                             collapse = " ")), ";
  [", name, "*];
  ", name, "*;
%g#1%
  [", name, "@0];
  ", name, "@1;
"),
  SAVEDATA = "
  FILE IS fscores.dat;
  SAVE IS FSCORES;",
rdata = data,
usevariables = c(grep("^i", names(data), value = TRUE), "gr"))

  m <- MplusAutomation::mplusModeler(model, dataout = "1d_CFA.dat", run = 1)
  # delete data file written used by Mplus
  unlink(m$results$input$data$file)
  if ("savedata" %in% names(m$results)) {
    fs <- m$results$savedata
    if (is.data.frame(fs)) {
      names(fs) <- tolower(names(fs))
      fs <- fs[, tolower(name), drop = FALSE]
    } else {
      fs <- as.data.frame(data)[, c(), drop = FALSE]
    }
  } else {
    fs <- as.data.frame(data)[, c(), drop = FALSE]
  }
  return(fs)
}
