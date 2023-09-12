#' @title Getting estimation results
#' @description
#' Gets parameters of interest from the estimated model object.
#' @param model an Mplus model object returned by [estimate_sem] (called with argument `method` set to `"mlr"`)
#' @param condition a one-row data frame storing a corresponding simulation condition parameters
#' @returns a one row data frame
get_mlr_results <- function(model, condition) {
  stopifnot(is.data.frame(condition), nrow(condition) == 1L)

  model <- model$results$parameters$unstandardized
  for (k in c("est", "se")) {
    if (is.character(model[[k]])) {
      model[[k]] <- suppressWarnings(as.numeric(model[[k]]))
    }
  }
  model$lower_2.5ci <- model$est + stats::qnorm(0.025) * model$se
  model$upper_2.5ci <- model$est + stats::qnorm(0.975) * model$se
  if (anyNA(model$lower_2.5ci)) {
    condition$model_warnings <-
      paste0(condition$model_warnings,
             ifelse(condition$model_warnings == "", "", "\n\n"),
             "Estimates of some parameters were to large to be written in Mplus output file.")
  }
  condition$ife1_mlr_pe <-
    model$est[model$paramHeader == "Intercepts" & model$param == "SLOPE_X1"]
  condition$ife1_mlr_l95ci <-
    model$lower_2.5ci[model$paramHeader == "Intercepts" & model$param == "SLOPE_X1"]
  condition$ife1_mlr_u95ci <-
    model$upper_2.5ci[model$paramHeader == "Intercepts" & model$param == "SLOPE_X1"]
  condition$ife2_mlr_pe <-
    model$est[model$paramHeader == "Y.ON" & model$param == "X2"]
  condition$ife2_mlr_l95ci <-
    model$lower_2.5ci[model$paramHeader == "Y.ON" & model$param == "X2"]
  condition$ife2_mlr_u95ci <-
    model$upper_2.5ci[model$paramHeader == "Y.ON" & model$param == "X2"]
  condition$corIfe_mlr_pe <-
    model$est[model$paramHeader == "X1.WITH" & model$param == "X2"]
  condition$corIfe_mlr_l95ci <-
    model$lower_2.5ci[model$paramHeader == "X1.WITH" & model$param == "X2"]
  condition$corIfe_mlr_u95ci <-
    model$upper_2.5ci[model$paramHeader == "X1.WITH" & model$param == "X2"]
  condition$gfeI_mlr_pe <-
    model$est[model$paramHeader == "YB.ON" & model$param == "W"]
  condition$gfeI_mlr_l95ci <-
    model$lower_2.5ci[model$paramHeader == "YB.ON" & model$param == "W"]
  condition$gfeI_mlr_u95ci <-
    model$upper_2.5ci[model$paramHeader == "YB.ON" & model$param == "W"]
  condition$gfeS_mlr_pe <-
    model$est[model$paramHeader == "SLOPE_X1.ON" & model$param == "W"]
  condition$gfeS_mlr_l95ci <-
    model$lower_2.5ci[model$paramHeader == "SLOPE_X1.ON" & model$param == "W"]
  condition$gfeS_mlr_u95ci <-
    model$upper_2.5ci[model$paramHeader == "SLOPE_X1.ON" & model$param == "W"]
  condition$greI_mlr_pe <-
    model$est[model$paramHeader == "Residual.Variances" & model$param == "YB"]^0.5
  condition$greI_mlr_l95ci <-
    model$lower_2.5ci[model$paramHeader == "Residual.Variances" & model$param == "YB"]^0.5
  condition$greI_mlr_l95ci <-
    ifelse(is.na(condition$greI_mlr_l95ci), 0, condition$greI_mlr_l95ci)
  condition$greI_mlr_u95ci <-
    model$upper_2.5ci[model$paramHeader == "Residual.Variances" & model$param == "YB"]^0.5
  condition$greS_mlr_pe <-
    model$est[model$paramHeader == "Residual.Variances" & model$param == "SLOPE_X1"]^0.5
  condition$greS_mlr_l95ci <-
    model$lower_2.5ci[model$paramHeader == "Residual.Variances" & model$param == "SLOPE_X1"]^0.5
  condition$greS_mlr_l95ci <-
    ifelse(is.na(condition$greS_mlr_l95ci), 0, condition$greS_mlr_l95ci)
  condition$greS_mlr_u95ci <-
    model$upper_2.5ci[model$paramHeader == "Residual.Variances" & model$param == "SLOPE_X1"]^0.5
  condition$covIS_mlr_pe <-
    model$est[model$paramHeader == "YB.WITH" & model$param == "SLOPE_X1"]
  condition$covIS_mlr_l95ci <-
    model$lower_2.5ci[model$paramHeader == "YB.WITH" & model$param == "SLOPE_X1"]
  condition$covIS_mlr_u95ci <-
    model$upper_2.5ci[model$paramHeader == "YB.WITH" & model$param == "SLOPE_X1"]

  return(condition)
}
