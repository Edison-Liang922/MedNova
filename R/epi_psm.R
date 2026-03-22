#' Propensity Score Matching (PSM)
#'
#' Generic PSM wrapper using MatchIt. Returns matched data for downstream
#' analysis.
#'
#' @param data A data frame containing treatment/exposure and covariates.
#' @param treat_col Column name for treatment/exposure variable (binary).
#' @param covariates Character vector of covariate column names used to
#'   estimate the propensity score.
#' @param case_label Optional value in `treat_col` representing the treated
#'   group.
#' @param control_label Optional value in `treat_col` representing the control
#'   group.
#' @param method Matching method.
#' @param distance Distance model.
#' @param ratio Matching ratio.
#' @param caliper Caliper width.
#' @param replace Whether to match with replacement.
#' @param return_model Logical; return the `matchit` object as well.
#' @param ... Additional arguments passed to `MatchIt::matchit`.
#'
#' @return If `return_model = FALSE`, a matched data frame. Otherwise a list
#'   with `matched_data` and `matchit_model`.
#' @export
Epi_PSM_match <- function(data,
                          treat_col,
                          covariates,
                          case_label = NULL,
                          control_label = NULL,
                          method = "nearest",
                          distance = "glm",
                          ratio = 1,
                          caliper = 0.1,
                          replace = FALSE,
                          return_model = FALSE,
                          ...) {
  if (!requireNamespace("MatchIt", quietly = TRUE)) {
    stop("Package 'MatchIt' is required.", call. = FALSE)
  }

  df <- as.data.frame(data)
  .mednova_assert_has_columns(df, c(treat_col, covariates), data_name = "data")

  if (!length(covariates)) {
    stop("`covariates` is empty.", call. = FALSE)
  }

  treat_vec <- .mednova_psm_prepare_treatment(
    x = df[[treat_col]],
    case_label = case_label,
    control_label = control_label
  )

  df$.treat <- treat_vec
  fml <- stats::as.formula(paste(".treat ~", paste(covariates, collapse = " + ")))

  matchit_model <- MatchIt::matchit(
    formula = fml,
    data = df,
    method = method,
    distance = distance,
    ratio = ratio,
    caliper = caliper,
    replace = replace,
    ...
  )

  matched_data <- MatchIt::match.data(matchit_model)

  if (isTRUE(return_model)) {
    return(list(matched_data = matched_data, matchit_model = matchit_model))
  }

  matched_data
}

#' PSM Baseline Tables Before and After Matching
#'
#' Create baseline characteristic tables before and after matching.
#'
#' @param data Original data frame containing treatment and covariates.
#' @param treat_col Column name for treatment/exposure variable.
#' @param covariates Character vector of covariate column names.
#' @param matched_data Optional matched data frame. If `NULL`, provide
#'   `matchit_model`.
#' @param matchit_model Optional MatchIt model object to derive matched data.
#' @param labels Optional named list to relabel variables in the table.
#' @param missing_text Text for missing values.
#' @param pvalue_fun Optional function for p-values.
#' @param pvalue_accuracy Accuracy for `scales::pvalue` if used.
#'
#' @return A list with `before` and `after` gtsummary tables.
#' @export
Epi_PSM_baseline_tables <- function(data,
                                    treat_col,
                                    covariates,
                                    matched_data = NULL,
                                    matchit_model = NULL,
                                    labels = NULL,
                                    missing_text = "缺失",
                                    pvalue_fun = NULL,
                                    pvalue_accuracy = 0.001) {
  if (!requireNamespace("gtsummary", quietly = TRUE)) {
    stop("Package 'gtsummary' is required.", call. = FALSE)
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required.", call. = FALSE)
  }
  if (!requireNamespace("rlang", quietly = TRUE)) {
    stop("Package 'rlang' is required.", call. = FALSE)
  }

  df <- as.data.frame(data)
  .mednova_assert_has_columns(df, c(treat_col, covariates), data_name = "data")

  if (!length(covariates)) {
    stop("`covariates` is empty.", call. = FALSE)
  }

  if (is.null(matched_data)) {
    if (is.null(matchit_model)) {
      stop("Provide either `matched_data` or `matchit_model`.", call. = FALSE)
    }
    if (!requireNamespace("MatchIt", quietly = TRUE)) {
      stop("Package 'MatchIt' is required to derive matched data.", call. = FALSE)
    }
    matched_data <- MatchIt::match.data(matchit_model)
  }

  .mednova_assert_has_columns(
    as.data.frame(matched_data),
    treat_col,
    data_name = "matched_data"
  )

  tbl_before <- gtsummary::tbl_summary(
    data = df,
    by = !!rlang::sym(treat_col),
    include = dplyr::all_of(covariates),
    label = labels,
    missing_text = missing_text
  )

  tbl_after <- gtsummary::tbl_summary(
    data = matched_data,
    by = !!rlang::sym(treat_col),
    include = dplyr::all_of(covariates),
    label = labels,
    missing_text = missing_text
  )

  if (is.null(pvalue_fun) && requireNamespace("scales", quietly = TRUE)) {
    pvalue_fun <- function(x) scales::pvalue(x, accuracy = pvalue_accuracy)
  }

  if (!is.null(pvalue_fun)) {
    tbl_before <- gtsummary::add_p(tbl_before, pvalue_fun = pvalue_fun)
    tbl_after <- gtsummary::add_p(tbl_after, pvalue_fun = pvalue_fun)
  } else {
    tbl_before <- gtsummary::add_p(tbl_before)
    tbl_after <- gtsummary::add_p(tbl_after)
  }

  list(before = tbl_before, after = tbl_after)
}

#' Unified MedNova Entry for Propensity Score Matching
#'
#' @param data A data frame containing treatment and covariates.
#' @param treat_col Column name for treatment/exposure variable.
#' @param covariates Character vector of covariate column names.
#' @param baseline Deprecated compatibility alias for `return_baseline`.
#' @param return_baseline Logical; whether to also compute baseline tables.
#' @param return_love_plot Logical; whether to also generate a love plot.
#' @param love_data Optional data frame already prepared for `plot_love()`. If
#'   omitted, `med_psm()` will derive love plot data from the matching summary.
#' @param ... Additional arguments passed to `Epi_PSM_match()`.
#'
#' @return A structured list containing `matched_object`, `matchit_model`,
#'   `matched_data`, optional `baseline_tables`, `balance_table`,
#'   `love_plot_data`, and optional `love_plot`.
#' @export
med_psm <- function(data,
                    treat_col,
                    covariates,
                    baseline = FALSE,
                    return_baseline = baseline,
                    return_love_plot = FALSE,
                    love_data = NULL,
                    ...) {
  matched_res <- Epi_PSM_match(
    data = data,
    treat_col = treat_col,
    covariates = covariates,
    return_model = TRUE,
    ...
  )

  matched_data <- matched_res$matched_data
  matchit_model <- matched_res$matchit_model

  balance_table <- .mednova_psm_balance_table(matchit_model)
  derived_love_data <- .mednova_psm_love_data(
    balance_table = balance_table,
    love_data = love_data
  )

  if (isTRUE(return_baseline)) {
    baseline_tables <- .mednova_psm_try_baseline_tables(
      data = data,
      treat_col = treat_col,
      covariates = covariates,
      matched_data = matched_data,
      matchit_model = matchit_model
    )
  } else {
    baseline_tables <- NULL
  }

  love_plot <- NULL
  if (isTRUE(return_love_plot)) {
    love_plot <- .mednova_psm_try_love_plot(derived_love_data)
  }

  structure(
    list(
      matched_object = matchit_model,
      matchit_model = matchit_model,
      matched_data = matched_data,
      baseline_tables = baseline_tables,
      balance_table = balance_table,
      love_plot_data = derived_love_data,
      love_plot = love_plot
    ),
    class = "mednova_psm_result"
  )
}

.mednova_psm_try_baseline_tables <- function(data,
                                             treat_col,
                                             covariates,
                                             matched_data,
                                             matchit_model) {
  tryCatch(
    Epi_PSM_baseline_tables(
      data = data,
      treat_col = treat_col,
      covariates = covariates,
      matched_data = matched_data,
      matchit_model = matchit_model
    ),
    error = function(e) {
      warning(
        "Baseline tables were not generated: ",
        conditionMessage(e),
        call. = FALSE
      )
      NULL
    }
  )
}

.mednova_psm_try_love_plot <- function(love_plot_data) {
  if (is.null(love_plot_data) || !nrow(love_plot_data)) {
    return(NULL)
  }

  plot_res <- tryCatch(
    plot_love(love_plot_data),
    error = function(e) {
      warning(
        "Love plot was not generated: ",
        conditionMessage(e),
        call. = FALSE
      )
      NULL
    }
  )

  if (is.null(plot_res)) {
    return(NULL)
  }

  if (is.list(plot_res) && "plot" %in% names(plot_res)) {
    return(plot_res$plot)
  }

  plot_res
}

.mednova_psm_love_data <- function(balance_table, love_data = NULL) {
  if (!is.null(love_data)) {
    return(as.data.frame(love_data, stringsAsFactors = FALSE, check.names = FALSE))
  }

  if (is.null(balance_table) || !nrow(balance_table)) {
    return(NULL)
  }

  love_cols <- c("Covariate", "AbsSMD_Before_max", "AbsSMD_After_max")
  balance_table[, love_cols, drop = FALSE]
}

.mednova_psm_balance_table <- function(matchit_model, include_distance = FALSE) {
  summary_obj <- summary(matchit_model, standardize = TRUE)
  sum_all <- as.data.frame(summary_obj$sum.all, check.names = FALSE)
  sum_matched <- as.data.frame(summary_obj$sum.matched, check.names = FALSE)

  all_terms <- union(rownames(sum_all), rownames(sum_matched))

  if (!isTRUE(include_distance)) {
    all_terms <- setdiff(all_terms, "distance")
  }

  if (!length(all_terms)) {
    return(data.frame())
  }

  pick_metric <- function(df, metric_name, terms) {
    if (is.null(df) || !nrow(df) || !metric_name %in% names(df)) {
      return(rep(NA_real_, length(terms)))
    }

    values <- df[terms, metric_name, drop = TRUE]
    as.numeric(values)
  }

  data.frame(
    covariate = all_terms,
    Covariate = all_terms,
    mean_treated_before = pick_metric(sum_all, "Means Treated", all_terms),
    mean_control_before = pick_metric(sum_all, "Means Control", all_terms),
    smd_before = pick_metric(sum_all, "Std. Mean Diff.", all_terms),
    abs_smd_before = abs(pick_metric(sum_all, "Std. Mean Diff.", all_terms)),
    var_ratio_before = pick_metric(sum_all, "Var. Ratio", all_terms),
    ecdf_mean_before = pick_metric(sum_all, "eCDF Mean", all_terms),
    ecdf_max_before = pick_metric(sum_all, "eCDF Max", all_terms),
    std_pair_dist_before = pick_metric(sum_all, "Std. Pair Dist.", all_terms),
    mean_treated_after = pick_metric(sum_matched, "Means Treated", all_terms),
    mean_control_after = pick_metric(sum_matched, "Means Control", all_terms),
    smd_after = pick_metric(sum_matched, "Std. Mean Diff.", all_terms),
    abs_smd_after = abs(pick_metric(sum_matched, "Std. Mean Diff.", all_terms)),
    var_ratio_after = pick_metric(sum_matched, "Var. Ratio", all_terms),
    ecdf_mean_after = pick_metric(sum_matched, "eCDF Mean", all_terms),
    ecdf_max_after = pick_metric(sum_matched, "eCDF Max", all_terms),
    std_pair_dist_after = pick_metric(sum_matched, "Std. Pair Dist.", all_terms),
    AbsSMD_Before_max = abs(pick_metric(sum_all, "Std. Mean Diff.", all_terms)),
    AbsSMD_After_max = abs(pick_metric(sum_matched, "Std. Mean Diff.", all_terms)),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

#' @export
print.mednova_psm_result <- function(x, ...) {
  matched_rows <- if (is.null(x$matched_data)) NA_integer_ else nrow(x$matched_data)
  balance_rows <- if (is.null(x$balance_table)) NA_integer_ else nrow(x$balance_table)

  cat("<mednova_psm_result>\n")
  cat("Matched rows:", matched_rows, "\n")
  cat("Balance rows:", balance_rows, "\n")
  cat(
    "Baseline tables:",
    if (is.null(x$baseline_tables)) "no" else "yes",
    "\n"
  )
  cat(
    "Love plot:",
    if (is.null(x$love_plot)) "no" else "yes",
    "\n"
  )
  invisible(x)
}

.mednova_psm_prepare_treatment <- function(x,
                                           case_label = NULL,
                                           control_label = NULL) {
  if (!is.null(case_label) || !is.null(control_label)) {
    if (is.null(case_label) || is.null(control_label)) {
      stop("Both `case_label` and `control_label` must be provided.", call. = FALSE)
    }
    mapped <- ifelse(
      x == case_label, 1,
      ifelse(x == control_label, 0, NA_real_)
    )
    if (any(is.na(mapped))) {
      stop("`treat_col` has values other than case/control labels.", call. = FALSE)
    }
    return(mapped)
  }

  if (is.factor(x) || is.character(x)) {
    levs <- unique(as.character(x))
    if (length(levs) != 2L) {
      stop("`treat_col` must have exactly two groups.", call. = FALSE)
    }
    return(factor(x, levels = levs))
  }

  if (is.numeric(x)) {
    vals <- sort(unique(x))
    if (!all(vals %in% c(0, 1))) {
      if (length(vals) == 2L) {
        message("Mapping treat_col: ", vals[1], " -> 0, ", vals[2], " -> 1.")
        return(ifelse(x == vals[2], 1, 0))
      }
      stop("Numeric `treat_col` must be binary (0/1) or two unique values.", call. = FALSE)
    }
    return(x)
  }

  stop("Unsupported `treat_col` type.", call. = FALSE)
}

.mednova_assert_has_columns <- function(data, cols, data_name = "data") {
  cols <- unique(cols[!is.na(cols) & nzchar(cols)])
  missing_cols <- setdiff(cols, names(data))
  if (length(missing_cols) > 0) {
    stop(
      "`", data_name, "` is missing columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }
}
