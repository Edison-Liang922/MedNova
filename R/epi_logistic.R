#' Univariable Logistic Regression
#'
#' @param data A data frame containing outcome and predictors.
#' @param outcome_col Column name for binary outcome.
#' @param predictors Character vector of predictor column names.
#' @param positive_label Optional value in `outcome_col` mapped to 1.
#' @param negative_label Optional value in `outcome_col` mapped to 0.
#' @param remove_na Logical; whether to drop rows with missing values.
#' @param pvalue_accuracy Accuracy for p-value formatting if `scales` is
#'   available.
#'
#' @return A list with `results` and `models`.
#' @export
Epi_logistic_univ <- function(data,
                              outcome_col,
                              predictors,
                              positive_label = NULL,
                              negative_label = NULL,
                              remove_na = TRUE,
                              pvalue_accuracy = 0.001) {
  prepared <- .mednova_prepare_logistic_data(
    data = data,
    outcome_col = outcome_col,
    predictors = predictors,
    positive_label = positive_label,
    negative_label = negative_label,
    remove_na = remove_na
  )

  df <- prepared$data
  models <- list()
  results <- data.frame()

  for (var in predictors) {
    fml <- stats::as.formula(paste(".y ~", var))
    model <- stats::glm(fml, data = df, family = stats::binomial())
    models[[var]] <- model
    coef_df <- as.data.frame(summary(model)$coefficients)

    if (nrow(coef_df) > 1L) {
      for (j in 2:nrow(coef_df)) {
        results <- rbind(
          results,
          .mednova_logistic_result_row(
            coef_df = coef_df,
            row_id = j,
            variable = var,
            pvalue_accuracy = pvalue_accuracy
          )
        )
      }
    }
  }

  list(results = results, models = models)
}

#' Multivariable Logistic Regression
#'
#' @param data A data frame containing outcome and predictors.
#' @param outcome_col Column name for binary outcome.
#' @param predictors Character vector of predictor column names.
#' @param positive_label Optional value in `outcome_col` mapped to 1.
#' @param negative_label Optional value in `outcome_col` mapped to 0.
#' @param remove_na Logical; whether to drop rows with missing values.
#' @param pvalue_accuracy Accuracy for p-value formatting if `scales` is
#'   available.
#'
#' @return A list with `results` and `model`.
#' @export
Epi_logistic_multiv <- function(data,
                                outcome_col,
                                predictors,
                                positive_label = NULL,
                                negative_label = NULL,
                                remove_na = TRUE,
                                pvalue_accuracy = 0.001) {
  prepared <- .mednova_prepare_logistic_data(
    data = data,
    outcome_col = outcome_col,
    predictors = predictors,
    positive_label = positive_label,
    negative_label = negative_label,
    remove_na = remove_na
  )

  df <- prepared$data
  fml <- stats::as.formula(paste(".y ~", paste(predictors, collapse = " + ")))
  model <- stats::glm(fml, data = df, family = stats::binomial())
  coef_df <- as.data.frame(summary(model)$coefficients)
  results <- data.frame()

  if (nrow(coef_df) > 1L) {
    for (j in 2:nrow(coef_df)) {
      level <- rownames(coef_df)[j]
      var_name <- predictors[sapply(predictors, function(x) grepl(x, level))]
      if (!length(var_name)) {
        var_name <- level
      }

      results <- rbind(
        results,
        .mednova_logistic_result_row(
          coef_df = coef_df,
          row_id = j,
          variable = var_name,
          pvalue_accuracy = pvalue_accuracy
        )
      )
    }
  }

  list(results = results, model = model)
}

#' Unified MedNova Entry for Logistic Regression
#'
#' @param data A data frame containing outcome and predictors.
#' @param outcome_col Column name for binary outcome.
#' @param predictors Character vector of predictor column names.
#' @param mode One of `"multiv"`, `"univ"`, or `"both"`.
#' @param positive_label Optional positive class label.
#' @param negative_label Optional negative class label.
#' @param remove_na Logical; whether to drop rows with missing values.
#' @param pvalue_accuracy Accuracy for p-value formatting.
#'
#' @return A list with `mode`, optional `univ`, and optional `multiv`.
#' @export
med_logistic <- function(data,
                         outcome_col,
                         predictors,
                         mode = c("both", "multiv", "univ"),
                         positive_label = NULL,
                         negative_label = NULL,
                         remove_na = TRUE,
                         pvalue_accuracy = 0.001) {
  mode <- match.arg(mode)
  out <- list(mode = mode)

  if (mode %in% c("both", "univ")) {
    out$univ <- Epi_logistic_univ(
      data = data,
      outcome_col = outcome_col,
      predictors = predictors,
      positive_label = positive_label,
      negative_label = negative_label,
      remove_na = remove_na,
      pvalue_accuracy = pvalue_accuracy
    )
  }

  if (mode %in% c("both", "multiv")) {
    out$multiv <- Epi_logistic_multiv(
      data = data,
      outcome_col = outcome_col,
      predictors = predictors,
      positive_label = positive_label,
      negative_label = negative_label,
      remove_na = remove_na,
      pvalue_accuracy = pvalue_accuracy
    )
  }

  out
}

.mednova_prepare_logistic_data <- function(data,
                                           outcome_col,
                                           predictors,
                                           positive_label = NULL,
                                           negative_label = NULL,
                                           remove_na = TRUE) {
  df <- as.data.frame(data)
  .mednova_assert_has_columns(df, c(outcome_col, predictors), data_name = "data")

  if (!length(predictors)) {
    stop("`predictors` is empty.", call. = FALSE)
  }

  df <- df[, c(outcome_col, predictors), drop = FALSE]
  if (isTRUE(remove_na)) {
    df <- df[stats::complete.cases(df), , drop = FALSE]
  }

  df$.y <- .mednova_prepare_binary_response(
    x = df[[outcome_col]],
    positive_label = positive_label,
    negative_label = negative_label,
    value_name = "outcome"
  )

  list(data = df)
}

.mednova_prepare_binary_response <- function(x,
                                             positive_label = NULL,
                                             negative_label = NULL,
                                             value_name = "value") {
  if (!is.null(positive_label) || !is.null(negative_label)) {
    if (is.null(positive_label) || is.null(negative_label)) {
      stop("Both `positive_label` and `negative_label` must be provided.", call. = FALSE)
    }
    mapped <- ifelse(
      x == positive_label, 1,
      ifelse(x == negative_label, 0, NA_real_)
    )
    if (any(is.na(mapped))) {
      stop("`", value_name, "` has values other than positive/negative labels.", call. = FALSE)
    }
    return(mapped)
  }

  if (is.numeric(x)) {
    vals <- sort(unique(x))
    if (!all(vals %in% c(0, 1))) {
      if (length(vals) == 2L) {
        message("Mapping ", value_name, ": ", vals[1], " -> 0, ", vals[2], " -> 1.")
        return(ifelse(x == vals[2], 1, 0))
      }
      stop("Numeric ", value_name, " must be binary (0/1) or have two unique values.", call. = FALSE)
    }
    return(x)
  }

  levs <- unique(as.character(x))
  if (length(levs) != 2L) {
    stop("`", value_name, "` must have exactly two groups.", call. = FALSE)
  }
  message("Mapping ", value_name, ": ", levs[1], " -> 0, ", levs[2], " -> 1.")
  ifelse(x == levs[2], 1, 0)
}

.mednova_logistic_result_row <- function(coef_df,
                                         row_id,
                                         variable,
                                         pvalue_accuracy = 0.001) {
  level <- rownames(coef_df)[row_id]
  beta <- coef_df[row_id, "Estimate"]
  se <- coef_df[row_id, "Std. Error"]
  p_value <- coef_df[row_id, "Pr(>|z|)"]
  or <- exp(beta)
  or_lower <- exp(beta - 1.96 * se)
  or_upper <- exp(beta + 1.96 * se)

  p_fmt <- if (requireNamespace("scales", quietly = TRUE)) {
    scales::pvalue(p_value, accuracy = pvalue_accuracy)
  } else {
    signif(p_value, 3)
  }

  data.frame(
    variable = variable,
    level = level,
    OR = round(or, 3),
    OR_95CI = paste0(round(or_lower, 3), "-", round(or_upper, 3)),
    P = p_fmt,
    stringsAsFactors = FALSE
  )
}
