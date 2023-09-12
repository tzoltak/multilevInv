#' @title Getting estimation results
#' @description
#' Gets parameters of interest from the estimated model object.
#' @param model a *merMod* model object
#' @param condition a one-row data frame storing a corresponding simulation condition parameters
#' @returns a one row data frame
get_lmer_results <- function(model, condition) {
  fixedEffs <- lme4::fixef(model)
  randEffs <- lme4::VarCorr(model)$gr
  confIntProf <- rbind(stats::confint(profile(model,
                                              which = c(1L, 3L, 6L:9L),
                                              signames = FALSE,
                                              prof.scale = "sdcor"),
                                      level = 0.95),
                       stats::confint(profile(model,
                                              which = 2L,
                                              signames = FALSE,
                                              prof.scale = "varcov"),
                                      level = 0.95))
  confIntAppr <- as.data.frame(summary(
    lmerTest::as_lmerModLmerTest(model))$coefficients)
  confIntAppr$l95ci <- confIntAppr$Estimate +
    confIntAppr$`Std. Error`*stats::qt(0.025, confIntAppr$df)
  confIntAppr$u95ci <- confIntAppr$Estimate +
    confIntAppr$`Std. Error`*stats::qt(0.975, confIntAppr$df)

  condition$ife1_obs_pe <- fixedEffs["x1"]
  condition$ife2_obs_pe <- fixedEffs["x2"]
  condition$gfeI_obs_pe <- fixedEffs["w"]
  condition$gfeS_obs_pe <-  fixedEffs["x1:w"]
  condition$greI_obs_pe <- attributes(randEffs)$stddev["(Intercept)"]
  condition$greS_obs_pe <- attributes(randEffs)$stddev["x1"]
  condition$covIS_obs_pe <- randEffs["(Intercept)", "x1"]
  condition$ife1_obs_l95ci_prof <- confIntProf["x1", "2.5 %"]
  condition$ife1_obs_u95ci_prof <- confIntProf["x1", "97.5 %"]
  condition$ife2_obs_l95ci_prof <- confIntProf["x2", "2.5 %"]
  condition$ife2_obs_u95ci_prof <- confIntProf["x2", "97.5 %"]
  condition$gfeI_obs_l95ci_prof <- confIntProf["w", "2.5 %"]
  condition$gfeI_obs_u95ci_prof <- confIntProf["w", "97.5 %"]
  condition$gfeS_obs_l95ci_prof <- confIntProf["x1:w", "2.5 %"]
  condition$gfeS_obs_u95ci_prof <- confIntProf["x1:w", "97.5 %"]
  condition$greI_obs_l95ci_prof <- confIntProf["sd_(Intercept)|gr", "2.5 %"]
  condition$greI_obs_u95ci_prof <- confIntProf["sd_(Intercept)|gr", "97.5 %"]
  condition$greS_obs_l95ci_prof <- confIntProf["sd_x1|gr", "2.5 %"]
  condition$greS_obs_u95ci_prof <- confIntProf["sd_x1|gr", "97.5 %"]
  condition$covIS_obs_l95ci_prof <- confIntProf["cov_x1.(Intercept)|gr", "2.5 %"]
  condition$covIS_obs_u95ci_prof <- confIntProf["cov_x1.(Intercept)|gr", "97.5 %"]
  condition$ife1_obs_l95ci_appr <- confIntAppr["x1", "l95ci"]
  condition$ife1_obs_u95ci_appr <- confIntAppr["x1", "u95ci"]
  condition$ife2_obs_l95ci_appr <- confIntAppr["x2", "l95ci"]
  condition$ife2_obs_u95ci_appr <- confIntAppr["x2", "u95ci"]
  condition$gfeI_obs_l95ci_appr <- confIntAppr["w", "l95ci"]
  condition$gfeI_obs_u95ci_appr <- confIntAppr["w", "u95ci"]
  condition$gfeS_obs_l95ci_appr <- confIntAppr["x1:w", "l95ci"]
  condition$gfeS_obs_u95ci_appr <- confIntAppr["x1:w", "u95ci"]

  return(condition)
}
