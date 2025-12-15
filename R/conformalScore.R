#' @title conformalScore
#'
#' @description
#' Computes conformal scores and the associated cutoff for a fitted multiview model
#' under early, late, or intermediate fusion.
#'
#' @param fit
#' A fitted model object. For early and late fusion, this should be an
#' \emph{IntegratedLearner} object. For intermediate fusion, this should be a
#' \emph{BayesCOOP} object.
#'
#' @param data_calib
#' Calibration data list containing feature_table, feature_metadata,
#' and sample_metadata. See \emph{IntegratedLearner} for details.
#'
#' @param fusion_choice
#' Fusion strategy to use. One of \code{"early"}, \code{"intermediate"}, or
#' \code{"late"}. Default is \code{"early"}.
#'
#' @param conf_level
#' Desired marginal confidence level. Default is \code{0.95}.
#'
#' @return
#' A list with components:
#' \item{d}{Conformal cutoff corresponding to \code{conf_level}.}
#' \item{conf_level}{Confidence level used.}
#' \item{fusion_choice}{Fusion strategy used.}
#' \item{fit}{Fitted model object.}
#' \item{conformal_scores}{Vector of conformal scores.}
#'
#' @importFrom stats quantile predict
#'
#' @export
conformalScore <- function(fit, data_calib,
                           fusion_choice = "early",
                           conf_level = 0.95) {

  ## Input checks
  if (is.null(fit)) {
    stop("`fit` must be provided.", call. = FALSE)
  }

  if (!is.numeric(conf_level) || length(conf_level) != 1 ||
      conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must be a number strictly between 0 and 1.", call. = FALSE)
  }

  trueY <- data_calib$sample_metadata$Y

  ## Extract calibration data
  if (fusion_choice %in% c("early", "late")) {

    if (is.null(fit$validPrediction) || is.null(fit$validY)) {
      stop("`fit` must contain `validPrediction` and `validY` for early/late fusion.",
           call. = FALSE)
    }

    calib.pred <- fit$validPrediction[, 1]

  } else {  # intermediate fusion

    # if (is.null(fit$y_pred) || is.null(fit$y_valid)) {
    #   stop("`fit` must contain `y_pred` and `y_valid` for intermediate fusion.",
    #        call. = FALSE)
    # }

    calib.pred <- predict(fit, data_calib)$y_pred
  }

  ## Conformal score computation
  conformal_scores <- unlist(abs(calib.pred - trueY))
  n <- length(conformal_scores)

  ## Finite-sample conformal quantile
  # q_level <- ceiling((n + 1) * conf_level / n)
  d <- as.numeric(
    stats::quantile(conformal_scores, probs = conf_level, type = 1)
  )

  ## Output
  out <- list(
    d = d,
    conf_level = conf_level,
    fusion_choice = fusion_choice,
    fit = fit,
    conformal_scores = conformal_scores
  )

  return(out)
}
