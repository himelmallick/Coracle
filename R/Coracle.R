#' @title Coracle
#'
#' @description
#' Conformalized multiview prediction under early, late, or intermediate fusion.
#' Produces finite-sample valid prediction intervals and empirical coverage
#' on a validation set.
#'
#' @param fit
#' A fitted \emph{IntegratedLearner} object from early/late fusion. See \emph{IntegratedLearner} for details. Default: NULL.
#'
#' @param fit_coop
#' A fitted \emph{BayesCOOP} object for intermediate fusion. See \emph{BayesCOOP} for details. Default: NULL.
#'
#' @param data_calib
#' Calibration data list containing feature_table, feature_metadata,
#' and sample_metadata. See \emph{IntegratedLearner} for details.
#'
#' @param data_valid
#' Validation data list containing feature_table, feature_metadata,
#' and sample_metadata. See \emph{IntegratedLearner} for details.
#'
#'
#' @param fusion_choice
#' Fusion strategy to use. One of \code{"early"}, \code{"intermediate"}, or
#' \code{"late"}. Default is \code{"early"}.
#'
#'
#' @param conf_level
#' Desired marginal confidence level. Default: 0.95.
#'
#' @return
#' A list with components:
#' \item{df}{A dataframe containing the Coracle output for different fusion choices.}
#' \item{coverage}{A list containing the coverage for different fusion choices.}
#' \item{conf_level}{Desired marginal confidence level.}
#'
#' @importFrom SuperLearner predict.SuperLearner
#' @importFrom stats predict
#' @export

Coracle <- function(fit = NULL,
                    fit_coop = NULL,
                    data_calib,
                    data_valid,
                    fusion_choice = c("late", "early", "intermediate"),
                    conf_level = 0.95) {

  ## -----------------------------
  ## Input checks
  ## -----------------------------
  if (is.null(fit) && is.null(fit_coop)) {
    stop("At least one of `fit` or `fit_coop` must be provided.", call. = FALSE)
  }

  if (!is.list(data_valid)) {
    stop("`data_valid` must be a list.", call. = FALSE)
  }

  if (!is.numeric(conf_level) || conf_level <= 0 || conf_level >= 1) {
    stop("`conf_level` must lie strictly between 0 and 1.", call. = FALSE)
  }

  fusion_choice <- intersect(
    fusion_choice,
    c("early", "late", "intermediate")
  )

  if (length(fusion_choice) == 0) {
    stop("Invalid `fusion_choice`.", call. = FALSE)
  }

  ## -----------------------------
  ## Validation data
  ## -----------------------------
  feature_metadata <- data_valid$feature_metadata
  feature_table <- data_valid$feature_table
  sample_metadata <- data_valid$sample_metadata
  Y_valid <- sample_metadata$Y
  subjectID <- sample_metadata$subjectID
  layers <- unique(feature_metadata$featureType)

  ## -----------------------------
  ## Initialize output
  ## -----------------------------
  res <- list()

  df <- data.frame(
    subjectID = subjectID,
    Y_true = Y_valid
  )

  coverage <- list()

  ## ==========================================================
  ## Helper function: build intervals + coverage
  ## ==========================================================
  build_coracle_output <- function(pred, d, tag) {

    lower <- pred - d
    upper <- pred + d

    df[[paste0("Y_pred(", tag, ")")]] <<- pred
    df[[paste0("Coracle(", tag, ")")]] <<-
      paste0("[", round(lower, 3), ", ", round(upper, 3), "]")

    df[[paste0("Coverage(", tag, ")")]] <<-
      as.integer(Y_valid > lower & Y_valid < upper)

    mean(df[[paste0("Coverage(", tag, ")")]])
  }

  ## ==========================================================
  ## Early fusion
  ## ==========================================================
  if ("early" %in% fusion_choice) {

    cs <- conformalScore(
      fit = fit$SL_fits$SL_fit_concat,
      data_calib = data_calib,
      fusion_choice = "early",
      conf_level = conf_level
    )

    X_valid <- as.data.frame(t(feature_table))
    pred <- predict.SuperLearner(cs$fit, newdata = X_valid)$pred

    coverage[["Coracle(early)"]] <- build_coracle_output(pred, cs$d, "early")
  }

  ## ==========================================================
  ## Late fusion
  ## ==========================================================
  if ("late" %in% fusion_choice) {

    cs <- conformalScore(
      fit = fit$SL_fits$SL_fit_stacked,
      data_calib = data_calib,
      fusion_choice = "late",
      conf_level = conf_level
    )

    layer_preds <- lapply(layers, function(layer) {
      idx <- feature_metadata$featureType == layer
      X_layer <- as.data.frame(t(feature_table[idx, ]))
      predict.SuperLearner(
        fit$SL_fits$SL_fit_layers[[layer]],
        newdata = X_layer
      )$pred
    })

    X_valid <- as.data.frame(do.call(cbind, layer_preds))
    names(X_valid) <- layers

    pred <- predict.SuperLearner(cs$fit, newdata = X_valid)$pred

    coverage[["Coracle(late)"]] <- build_coracle_output(pred, cs$d, "late")
  }

  ## ==========================================================
  ## Intermediate fusion
  ## ==========================================================
  if ("intermediate" %in% fusion_choice) {

    cs <- conformalScore(
      fit = fit_coop,
      data_calib = data_calib,
      fusion_choice = "intermediate",
      conf_level = conf_level
    )

    pred <- predict(fit_coop, data_valid)$y_pred

    coverage[["Coracle(intermediate)"]] <- build_coracle_output(pred, cs$d, "intermediate")
  }

  res$df <- df
  res[["coverage"]] <- coverage
  res[["conf_level"]] <- conf_level

  return(res)
}
