#' @title Model estimation
#' @description
#' Sets and estimates multilevel SEM model using Mplus (using the
#' [MplusAutomation] package).
#' @param data a data frame generated with values of the generated observed
#' variables and a group-membership indicator
#' @param method a string indicating whicg type of estimation should be used
#' @param varResidY variance of the latent dependent variable error term - it is
#' fixed to assure structural model parameters' estimates are returned in the
#' metric comparable with data generating model
#' @param ife1Start starting values for model parameters (compare [generate_latent])
#' @param ife2Start starting values for model parameters (compare [generate_latent])
#' @param covIfeStart starting values for model parameters (compare [generate_latent])
#' @param gfeIStart starting values for model parameters (compare [generate_latent])
#' @param gfeSStart starting values for model parameters (compare [generate_latent])
#' @param greIStart starting values for model parameters (compare [generate_latent])
#' @param greSStart starting values for model parameters (compare [generate_latent])
#' @param covISStart starting values for model parameters (compare [generate_latent])
#' @returns an Mplus model object, see [MplusAutomation::mplusModeler]
estimate_sem <- function(data, method = c("mlr", "bayes"),
                         varResidY, ife1Start, ife2Start, covIfeStart,
                         gfeIStart, gfeSStart, greIStart, greSStart, covISStart) {
  method = match.arg(method)

  if (method == "bayes") {
    analysis = "
  TYPE = TWOLEVEL RANDOM;
  ESTIMATOR = BAYES;
  BITERATIONS = (1000);
  PROCESSORS = 2;"
    dataOut = "SEM_BAYES.dat"
  } else {
    analysis = "
  TYPE = TWOLEVEL RANDOM;
  ESTIMATOR = MLR;
  ALGORITHM = INTEGRATION;
  INTEGRATION = 10;
  PROCESSORS = 8;"
    dataOut = "SEM_ML.dat"
  }

  model <- MplusAutomation::mplusObject(
    TITLE = paste0("Multilevel SEM ", ifelse(method == "bayes", "BAYES", "MLR")),
    VARIABLE = paste0("
  USEVAR = ", paste(grep("^i", names(data), value = TRUE), collapse = " "), " gr w;
  CLUSTER = gr;
  BETWEEN = w;
  WITHIN = ", paste(grep("^ix", names(data), value = TRUE), collapse = " "), ";"),
  ANALYSIS = analysis,
  MODEL = paste0("
%WITHIN%
  x1 by ", sub("ix11 ", "ix11* ",
               paste(grep("^ix1", names(data), value = TRUE), collapse = " ")), ";
  x2 by ", sub("ix21 ", "ix21* ",
               paste(grep("^ix2", names(data), value = TRUE), collapse = " ")), ";
  x1@1; [x1@0];
  x2@1; [x2@0];
  x1 WITH x2*", covIfeStart, ";
", paste0("  y by ", grep("^iy", names(data), value = TRUE),
          "* (l", seq_len(sum(grepl("^iy", names(data)))), ");", collapse = "\n"), "
  y@", varResidY, "; [y@0];
  slope_x1 | y ON x1;
  y ON x2*", ife2Start, ";
%BETWEEN%
", paste0("  yb by ", grep("^iy", names(data), value = TRUE),
          "* (l", seq_len(sum(grepl("^iy", names(data)))), ");", collapse = "\n"), "
  yb on w*", gfeIStart, ";
  yb*", greIStart, "; ![yb@0];
  slope_x1 on w*", gfeSStart, ";
  slope_x1*", greSStart, "; [slope_x1*", ife1Start, "];
  yb WITH slope_x1*", covISStart, ";"),
OUTPUT = "!STANDARDIZED (STDYX);",
rdata = data,
usevariables = c(grep("^i", names(data), value = TRUE), "gr", "w"))

  return(suppressMessages(
    MplusAutomation::mplusModeler(model, dataout = dataOut, run = 1)))
}
