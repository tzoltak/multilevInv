#' @title Model estimation
#' @description
#' Estimates models in Mplus (using the [MplusAutomation] package), that will be
#' used to perform non-invariance tests for a specific construct.
#' @param data a data frame generated with values of the generated observed
#' indicators (which names are assumed to start from "i") of a single latent
#' variable and a group-membership indicator (named "gr")
#' @seealso [test_invariance]
#' @returns an Mplus model object, see [MplusAutomation::mplusModeler]
estimate_invariance <- function(data) {
  model <- MplusAutomation::mplusObject(
    TITLE = "Invariance tests by MLR",
    VARIABLE = paste0("
  USEVAR = ", paste(grep("^i", names(data), value = TRUE), collapse = " "), ";
  CLASSES = g(", length(unique(data$gr)), ");
  KNOWNCLASS = g(gr);"),
  ANALYSIS = "
  TYPE = MIXTURE;
  ESTIMATOR = MLR;
  ALGORITHM = INTEGRATION;
  INTEGRATION = 10;
  PROCESSORS = 8;
  MODEL = CONFIGURAL METRIC SCALAR;",
  MODEL = paste0("
%OVERALL%
  f by ", sub(" ", "* ",
              paste(grep("^i", names(data), value = TRUE), collapse = " ")), ";
  f@1;"),
rdata = data,
usevariables = c(grep("^i", names(data), value = TRUE), "gr"))

  return(suppressMessages(
    MplusAutomation::mplusModeler(model, dataout = "INVARIANCE_ML.dat", run = 1,
                                  quiet = TRUE)))
}
