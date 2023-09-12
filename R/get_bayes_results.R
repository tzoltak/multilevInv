#' @title Getting estimation results
#' @description
#' Gets parameters of interest from the estimated model object.
#' @param model an Mplus model object returned by [estimate_sem] (called with argument `method` set to `"model"`)
#' @param condition a one-row data frame storing a corresponding simulation condition parameters
#' @returns a one row data frame
get_bayes_results <- function(model, condition) {
  stopifnot(is.data.frame(condition), nrow(condition) == 1L)

  model <- model$results$parameters$unstandardized
  for (k in c("est", "lower_2.5ci", "upper_2.5ci")) {
    if (is.character(model[[k]])) {
      model[[k]] <- suppressWarnings(as.numeric(model[[k]]))
    }
  }
  if (anyNA(c(model$est, model$lower_2.5ci, model$upper_2.5ci))) {
    condition$model_warnings <-
      paste0(condition$model_warnings,
             ifelse(condition$model_warnings == "", "", "\n\n"),
             "Estimates of some parameters were to large to be written in Mplus output file.")
  }
  condition$ife1_bayes_pe <-
    model$est[model$paramHeader == "Intercepts" & model$param == "SLOPE_X1"]
  condition$ife1_bayes_l95pi <-
    model$lower_2.5ci[model$paramHeader == "Intercepts" & model$param == "SLOPE_X1"]
  condition$ife1_bayes_u95pi <-
    model$upper_2.5ci[model$paramHeader == "Intercepts" & model$param == "SLOPE_X1"]
  condition$ife2_bayes_pe <-
    model$est[model$paramHeader == "Y.ON" & model$param == "X2"]
  condition$ife2_bayes_l95pi <-
    model$lower_2.5ci[model$paramHeader == "Y.ON" & model$param == "X2"]
  condition$ife2_bayes_u95pi <-
    model$upper_2.5ci[model$paramHeader == "Y.ON" & model$param == "X2"]
  condition$corIfe_bayes_pe <-
    model$est[model$paramHeader == "X1.WITH" & model$param == "X2"]
  condition$corIfe_bayes_l95pi <-
    model$lower_2.5ci[model$paramHeader == "X1.WITH" & model$param == "X2"]
  condition$corIfe_bayes_u95pi <-
    model$upper_2.5ci[model$paramHeader == "X1.WITH" & model$param == "X2"]
  condition$gfeI_bayes_pe <-
    model$est[model$paramHeader == "YB.ON" & model$param == "W"]
  condition$gfeI_bayes_l95pi <-
    model$lower_2.5ci[model$paramHeader == "YB.ON" & model$param == "W"]
  condition$gfeI_bayes_u95pi <-
    model$upper_2.5ci[model$paramHeader == "YB.ON" & model$param == "W"]
  condition$gfeS_bayes_pe <-
    model$est[model$paramHeader == "SLOPE_X1.ON" & model$param == "W"]
  condition$gfeS_bayes_l95pi <-
    model$lower_2.5ci[model$paramHeader == "SLOPE_X1.ON" & model$param == "W"]
  condition$gfeS_bayes_u95pi <-
    model$upper_2.5ci[model$paramHeader == "SLOPE_X1.ON" & model$param == "W"]
  condition$greI_bayes_pe <-
    model$est[model$paramHeader == "Residual.Variances" & model$param == "YB"]^0.5
  condition$greI_bayes_l95pi <-
    model$lower_2.5ci[model$paramHeader == "Residual.Variances" & model$param == "YB"]^0.5
  condition$greI_bayes_u95pi <-
    model$upper_2.5ci[model$paramHeader == "Residual.Variances" & model$param == "YB"]^0.5
  condition$greS_bayes_pe <-
    model$est[model$paramHeader == "Residual.Variances" & model$param == "SLOPE_X1"]^0.5
  condition$greS_bayes_l95pi <-
    model$lower_2.5ci[model$paramHeader == "Residual.Variances" & model$param == "SLOPE_X1"]^0.5
  condition$greS_bayes_u95pi <-
    model$upper_2.5ci[model$paramHeader == "Residual.Variances" & model$param == "SLOPE_X1"]^0.5
  condition$covIS_bayes_pe <-
    model$est[model$paramHeader == "YB.WITH" & model$param == "SLOPE_X1"]
  condition$covIS_bayes_l95pi <-
    model$lower_2.5ci[model$paramHeader == "YB.WITH" & model$param == "SLOPE_X1"]
  condition$covIS_bayes_u95pi <-
    model$upper_2.5ci[model$paramHeader == "YB.WITH" & model$param == "SLOPE_X1"]

  return(condition)
}
