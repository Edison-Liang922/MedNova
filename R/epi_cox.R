#' Multivariable Cox Regression
#'
#' @param data A data frame containing time, status, and predictors.
#' @param time_col Column name for survival time.
#' @param status_col Column name for event status.
#' @param predictors Character vector of predictor terms.
#' @param event_label Optional value in `status_col` mapped to 1.
#' @param censor_label Optional value in `status_col` mapped to 0.
#' @param remove_na Logical; whether to drop rows with missing values.
#' @param conf_level Confidence level for HR intervals.
#' @param ties Ties method passed to `survival::coxph`.
#' @param pvalue_accuracy Accuracy for p-value formatting.
#' @param ... Additional arguments passed to `survival::coxph`.
#'
#' @return A list with `results`, `model`, `concordance`, and `n`.
#' @export
Epi_cox_multiv <- function(data,
                           time_col,
                           status_col,
                           predictors,
                           event_label = NULL,
                           censor_label = NULL,
                           remove_na = TRUE,
                           conf_level = 0.95,
                           ties = "efron",
                           pvalue_accuracy = 0.001,
                           ...) {
  if (!requireNamespace("survival", quietly = TRUE)) {
    stop("Package 'survival' is required.", call. = FALSE)
  }

  df <- as.data.frame(data)
  .mednova_assert_has_columns(df, c(time_col, status_col), data_name = "data")

  if (!length(predictors)) {
    stop("`predictors` is empty.", call. = FALSE)
  }

  fml <- stats::as.formula(paste0(
    "survival::Surv(",
    time_col,
    ", ",
    status_col,
    ") ~ ",
    paste(predictors, collapse = " + ")
  ))

  vars_needed <- unique(all.vars(fml))
  .mednova_assert_has_columns(df, vars_needed, data_name = "data")

  df <- df[, vars_needed, drop = FALSE]
  if (isTRUE(remove_na)) {
    df <- df[stats::complete.cases(df), , drop = FALSE]
  }

  df[[status_col]] <- .mednova_prepare_binary_response(
    x = df[[status_col]],
    positive_label = event_label,
    negative_label = censor_label,
    value_name = "status"
  )

  time_val <- as.numeric(df[[time_col]])
  if (any(is.na(time_val))) {
    stop("`time_col` contains NA after coercion.", call. = FALSE)
  }
  if (any(time_val <= 0)) {
    stop("`time_col` must be > 0.", call. = FALSE)
  }
  df[[time_col]] <- time_val

  fit <- survival::coxph(fml, data = df, ties = ties, ...)
  cox_sum <- summary(fit, conf.int = conf_level)
  coef_mat <- cox_sum$coefficients
  conf_mat <- cox_sum$conf.int

  p_val <- coef_mat[, "Pr(>|z|)"]
  p_fmt <- if (requireNamespace("scales", quietly = TRUE)) {
    scales::pvalue(p_val, accuracy = pvalue_accuracy)
  } else {
    signif(p_val, 3)
  }

  results <- data.frame(
    term = rownames(coef_mat),
    beta = coef_mat[, "coef"],
    HR = conf_mat[, "exp(coef)"],
    lower95 = conf_mat[, "lower .95"],
    upper95 = conf_mat[, "upper .95"],
    P = p_fmt,
    stringsAsFactors = FALSE
  )

  list(
    results = results,
    model = fit,
    concordance = as.numeric(cox_sum$concordance[1]),
    n = nrow(df)
  )
}

#' Unified MedNova Entry for Cox Regression
#'
#' @param data A data frame containing survival inputs.
#' @param time_col Column name for survival time.
#' @param status_col Column name for event status.
#' @param predictors Character vector of predictor terms.
#' @param ... Additional arguments passed to `Epi_cox_multiv()`.
#'
#' @return A list with a single `multiv` result.
#' @export
med_cox <- function(data,
                    time_col,
                    status_col,
                    predictors,
                    ...) {
  list(
    multiv = Epi_cox_multiv(
      data = data,
      time_col = time_col,
      status_col = status_col,
      predictors = predictors,
      ...
    )
  )
}
