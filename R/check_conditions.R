#' @title Preparing simulation conditions
#' @description
#' Returns names of parameters required to specify a simulation condition.
#' @returns a character vector
#' @seealso [check_conditions_names], [check_conditions]
#' @export
get_required_conditions_names <- function() {
  return(c("m", "n", "ife1", "ife2", "corIfe", "gfeI", "gfeS", "greI", "greS",
           "corIS", "nItems", "lambda", "tau", "nNonInvItems", "difLambda",
           "difTau", "appInvLambda", "appInvTau", "pctGroupsAffected", "nonInv"))
}
#' @title Preparing simulation conditions
#' @description
#' Checks whether a given vector of names contains names of all the parameters
#' required to specify a simulation condition
#' @param parNames a character vector
#' @returns its argument (or throws an error)
#' @seealso [check_conditions]
check_conditions_names <- function(parNames) {
  stopifnot(is.character(parNames),
            !anyNA(parNames))
  reqNames <- get_required_conditions_names()
  if (!all(reqNames %in% parNames)) {
    stop(paste0("Some required parameters has not been specified: '",
                paste(setdiff(reqNames, parNames), collapse = "', '"), "'."))
  }
  if (any(!(parNames %in% reqNames))) {
    message(paste0("Some additional parameters have been specified: '",
                   paste(setdiff(parNames, reqNames), collapse = "', '"), "'. ",
                   "These ones won't affect the simulation design."))
  }
  if (any(duplicated(parNames))) {
    stop(paste0("Each argument must be specified as a column in exactly one data frame (duplicated names: '",
                paste(parNames[duplicated(parNames)], collapse = "', '"), "')."))
  }
  return(parNames)
}
#' @title Preparing simulation conditions
#' @description
#' Checks whether columns in the data frame storing specifications of simulation
#' conditions have a correct set o values
#' @param conditions a data frame
#' @returns its argument (or throws an error)
#' @seealso [check_conditions_names]
#' @export
check_conditions <- function(conditions) {
  stopifnot(is.data.frame(conditions))
  check_conditions_names(names(conditions))

  stopifnot(!anyNA(conditions[, get_required_conditions_names()]),
            is.numeric(conditions$n), all(conditions$n > 0),
            all(as.integer(conditions$n) == conditions$n),
            is.numeric(conditions$m), all(conditions$m > 0),
            all(as.integer(conditions$m) == conditions$m),
            is.numeric(conditions$ife1), is.numeric(conditions$ife2),
            is.numeric(conditions$corIfe),
            all(conditions$corIfe < 1), all(conditions$corIfe > -1),
            is.numeric(conditions$gfeI), is.numeric(conditions$gfeS),
            is.numeric(conditions$greI), conditions$greI >= 0,
            is.numeric(conditions$greS), conditions$greS >= 0,
            is.numeric(conditions$corIS),
            all(conditions$corIS < 1), all(conditions$corIS > -1),
            is.numeric(conditions$nItems), all(conditions$nItems > 0),
            all(as.integer(conditions$nItems) == conditions$nItems),
            is.numeric(conditions$lambda),
            is.numeric(conditions$tau),
            is.numeric(conditions$nNonInvItems), all(conditions$nNonInvItems >= 0),
            all(as.integer(conditions$nNonInvItems) == conditions$nNonInvItems),
            all(conditions$nNonInvItems <= conditions$nItems),
            is.numeric(conditions$difLambda), all(conditions$difLambda >= 0),
            is.numeric(conditions$difTau), all(conditions$difTau >= 0),
            is.numeric(conditions$appInvLambda), all(conditions$appInvLambda >= 0),
            is.numeric(conditions$appInvTau), all(conditions$appInvTau >= 0),
            is.numeric(conditions$pctGroupsAffected),
            all(conditions$pctGroupsAffected >= 0),
            all(conditions$pctGroupsAffected <= 1),
            all(conditions$nonInv %in% c("all", "independent", "dependent")),
            "In some condition(s) dependent latent variable error term's variance won't be positive." =
              all((conditions$ife1^2 + conditions$ife2^2 +
                     conditions$ife1*conditions$ife2*2*conditions$corIfe +
                     conditions$gfeI^2 + conditions$greI^2) < 1))
  if (any((conditions$difLambda > 0 | conditions$difTau > 0) &
          (conditions$appInvLambda > 0 | conditions$appInvtau > 0))) {
    warning("There are some conditions in which both fixed non-invariance effect(s) and approximate invariance random effect(s) are set to non-zero values, but this hardly makes sense.")
  }
  return(conditions)
}
