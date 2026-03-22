#' 为支持任务生成可运行的 R 脚本
#'
#' @param task 任务描述，或者 `mednova_match_task()` 的返回结果。
#' @param data 用户提供的数据对象，或者一个字符标量，用于指定脚本中的对象名。
#' @param spec 可选的命名 list，用于提供任务相关提示。
#' @param data_profile 可选的 `mednova_inspect_data()` 输出结果。省略时会自动生成。
#' @param assumptions 可选字符向量，用于在脚本顶部写入假设说明。
#'
#' @return 返回单个字符字符串，即生成好的 R 脚本文本。
#' @export
mednova_generate_script <- function(task,
                                    data,
                                    spec = list(),
                                    data_profile = NULL,
                                    assumptions = NULL) {
  task_match <- .mednova_resolve_task(task)

  if (!is.list(spec)) {
    stop("`spec` must be a named list.", call. = FALSE)
  }

  if (is.null(data_profile)) {
    data_profile <- mednova_inspect_data(data)
  }

  context <- .mednova_build_script_context(
    domain = task_match$domain,
    data = data,
    spec = spec,
    data_profile = data_profile
  )

  if (is.null(assumptions)) {
    assumptions <- context$assumptions
  }

  reference_script <- .mednova_build_reference_script(
    task_match = task_match,
    context = context
  )

  if (!is.null(reference_script)) {
    script_body <- reference_script$body
    script_packages <- reference_script$packages
  } else {
    template_text <- .mednova_read_template_text(task_match$template_id)
    script_body <- .mednova_fill_template(template_text, context$replacements)
    script_packages <- .mednova_template_libraries(task_match$template_id)
  }

  script_header <- .mednova_script_header(
    task_match = task_match,
    data_profile = data_profile,
    assumptions = assumptions,
    packages = script_packages
  )

  paste(c(script_header, script_body), collapse = "\n")
}

.mednova_build_script_context <- function(domain, data, spec, data_profile) {
  merged_spec <- utils::modifyList(data_profile$guessed_columns, spec)
  data_name <- data_profile$object_name %||% .mednova_resolve_data_name(data, substitute(data))
  column_names <- data_profile$column_names %||% character()

  switch(
    domain,
    propensity_score_matching = .mednova_psm_context(data_name, column_names, merged_spec, data_profile),
    logistic_regression = .mednova_logistic_context(data_name, column_names, merged_spec, data_profile),
    cox_regression = .mednova_cox_context(data_name, column_names, merged_spec, data_profile),
    bulk_rna_differential_expression = .mednova_bulk_deg_context(data, data_name, merged_spec, data_profile),
    mendelian_randomization = .mednova_mr_context(data, data_name, merged_spec, data_profile),
    diagnostic_modeling = .mednova_diagnostic_context(data_name, column_names, merged_spec, data_profile),
    stop("Unsupported domain: ", domain, call. = FALSE)
  )
}

.mednova_psm_context <- function(data_name, column_names, spec, data_profile) {
  treatment <- .mednova_first_value(
    spec$treatment,
    spec$exposure,
    spec$group,
    "treatment"
  )
  covariates <- .mednova_covariates(column_names, spec$covariates, exclude = treatment)
  formula_text <- .mednova_formula_text(treatment, covariates)
  assumptions <- c(
    .mednova_base_assumptions(data_profile),
    paste0("`", treatment, "` is the binary treatment or exposure column."),
    .mednova_covariate_assumption(covariates)
  )

  list(
    replacements = list(
      DATA_NAME = data_name,
      PSM_FORMULA = .mednova_string_literal(formula_text)
    ),
    values = list(
      data_name = data_name,
      treatment = treatment,
      covariates = covariates,
      formula_text = formula_text
    ),
    assumptions = assumptions
  )
}

.mednova_logistic_context <- function(data_name, column_names, spec, data_profile) {
  outcome <- .mednova_first_value(
    spec$outcome,
    spec$event,
    spec$case_control,
    "outcome"
  )
  covariates <- .mednova_covariates(column_names, spec$covariates, exclude = outcome)
  formula_text <- .mednova_formula_text(outcome, covariates)
  assumptions <- c(
    .mednova_base_assumptions(data_profile),
    paste0("`", outcome, "` is a binary outcome column."),
    .mednova_covariate_assumption(covariates)
  )

  list(
    replacements = list(
      DATA_NAME = data_name,
      MODEL_FORMULA = .mednova_string_literal(formula_text)
    ),
    values = list(
      data_name = data_name,
      outcome = outcome,
      covariates = covariates,
      formula_text = formula_text
    ),
    assumptions = assumptions
  )
}

.mednova_cox_context <- function(data_name, column_names, spec, data_profile) {
  time_col <- .mednova_first_value(spec$time, "time")
  event_col <- .mednova_first_value(spec$event, spec$outcome, "event")
  covariates <- .mednova_covariates(
    column_names,
    spec$covariates,
    exclude = c(time_col, event_col)
  )
  rhs <- .mednova_rhs_text(covariates)
  formula_text <- paste0(
    "survival::Surv(",
    .mednova_name(time_col),
    ", ",
    .mednova_name(event_col),
    ") ~ ",
    rhs
  )
  assumptions <- c(
    .mednova_base_assumptions(data_profile),
    paste0("`", time_col, "` contains positive follow-up time values."),
    paste0("`", event_col, "` is the event indicator column coded as 0/1 or a two-level label."),
    .mednova_covariate_assumption(covariates)
  )

  list(
    replacements = list(
      DATA_NAME = data_name,
      COX_FORMULA = .mednova_string_literal(formula_text)
    ),
    values = list(
      data_name = data_name,
      time_col = time_col,
      event_col = event_col,
      covariates = covariates,
      formula_text = formula_text
    ),
    assumptions = assumptions
  )
}

.mednova_bulk_deg_context <- function(data, data_name, spec, data_profile) {
  group_column <- .mednova_first_value(spec$group, "group")
  sample_id <- .mednova_optional_first_value(spec$sample_id)
  group_levels <- .mednova_detect_two_levels(data, group_column, defaults = c("Control", "Case"))
  control_level <- .mednova_first_value(spec$control_level, group_levels[1L], "Control")
  case_level <- .mednova_first_value(spec$case_level, group_levels[2L], "Case")
  column_names <- data_profile$column_names %||% character()

  if (is.list(data) && all(c("counts", "col_data") %in% names(data))) {
    input_style <- "bundle"
    count_matrix_name <- paste0(data_name, "$counts")
    group_data_name <- paste0(data_name, "$col_data")
    feature_columns <- character()
  } else if (is.matrix(data)) {
    input_style <- "matrix"
    count_matrix_name <- data_name
    group_data_name <- .mednova_first_value(spec$group_data_name, "group_data")
    feature_columns <- character()
  } else {
    input_style <- "sample_table"
    count_matrix_name <- "count_matrix"
    group_data_name <- "group_data"
    feature_columns <- setdiff(column_names, stats::na.omit(c(group_column, sample_id)))
  }

  assumptions <- c(
    .mednova_base_assumptions(data_profile),
    paste0("`", group_column, "` contains the comparison groups `", case_level, "` and `", control_level, "`."),
    if (identical(input_style, "sample_table")) {
      "All remaining columns will be treated as expression features."
    } else {
      paste0("`", count_matrix_name, "` and `", group_data_name, "` are ready for `med_bulk_deg()`.")
    }
  )

  list(
    replacements = list(
      COUNT_MATRIX_NAME = count_matrix_name,
      COL_DATA_NAME = group_data_name,
      GROUP_COLUMN = .mednova_string_literal(group_column),
      DESIGN_TERM = .mednova_name(group_column),
      CASE_LEVEL = .mednova_string_literal(case_level),
      CONTROL_LEVEL = .mednova_string_literal(control_level)
    ),
    values = list(
      input_style = input_style,
      data_name = data_name,
      count_matrix_name = count_matrix_name,
      group_data_name = group_data_name,
      group_column = group_column,
      sample_id = sample_id,
      feature_columns = feature_columns,
      case_level = case_level,
      control_level = control_level
    ),
    assumptions = assumptions
  )
}

.mednova_mr_context <- function(data, data_name, spec, data_profile) {
  if (is.list(data) && all(c("exposure_dat", "outcome_dat") %in% names(data))) {
    exposure_data_name <- paste0(data_name, "$exposure_dat")
    outcome_data_name <- paste0(data_name, "$outcome_dat")
  } else {
    exposure_data_name <- .mednova_first_value(spec$exposure_data_name, data_name)
    outcome_data_name <- .mednova_first_value(spec$outcome_data_name, "outcome_data")
  }

  exposure_name <- .mednova_first_value(spec$exposure_name, "Exposure")
  col_map <- list(
    snp = .mednova_first_value(spec$snp, "SNP"),
    beta = .mednova_first_value(spec$beta, "BETA"),
    se = .mednova_first_value(spec$se, "SE"),
    eaf = .mednova_first_value(spec$eaf, "EAF"),
    effect_allele = .mednova_first_value(spec$effect_allele, "A1"),
    other_allele = .mednova_first_value(spec$other_allele, "A2"),
    pval = .mednova_first_value(spec$pval, "P"),
    n = .mednova_first_value(spec$n, "N"),
    chr = .mednova_first_value(spec$chr, "CHR"),
    pos = .mednova_first_value(spec$pos, "POS")
  )

  assumptions <- c(
    .mednova_base_assumptions(data_profile),
    paste0("`", exposure_data_name, "` will be used as the exposure dataset."),
    paste0("`", outcome_data_name, "` will be used as the outcome dataset.")
  )

  list(
    replacements = list(
      EXPOSURE_DATA_NAME = exposure_data_name,
      OUTCOME_DATA_NAME = outcome_data_name
    ),
    values = list(
      exposure_data_name = exposure_data_name,
      outcome_data_name = outcome_data_name,
      exposure_name = exposure_name,
      col_map = col_map
    ),
    assumptions = assumptions
  )
}

.mednova_diagnostic_context <- function(data_name, column_names, spec, data_profile) {
  outcome <- .mednova_first_value(
    spec$outcome,
    spec$case_control,
    spec$event,
    "outcome"
  )
  covariates <- .mednova_covariates(column_names, spec$covariates, exclude = outcome)
  sample_id <- .mednova_optional_first_value(spec$sample_id)
  formula_text <- .mednova_formula_text(outcome, covariates)
  assumptions <- c(
    .mednova_base_assumptions(data_profile),
    paste0("`", outcome, "` is a binary diagnostic outcome column."),
    .mednova_covariate_assumption(covariates)
  )

  list(
    replacements = list(
      DATA_NAME = data_name,
      MODEL_FORMULA = .mednova_string_literal(formula_text),
      OUTCOME_COLUMN = .mednova_string_literal(outcome)
    ),
    values = list(
      data_name = data_name,
      outcome = outcome,
      sample_id = sample_id,
      covariates = covariates,
      formula_text = formula_text
    ),
    assumptions = assumptions
  )
}

.mednova_build_reference_script <- function(task_match, context) {
  switch(
    task_match$domain,
    propensity_score_matching = .mednova_reference_psm_script(context),
    logistic_regression = .mednova_reference_logistic_script(context),
    cox_regression = .mednova_reference_cox_script(context),
    bulk_rna_differential_expression = .mednova_reference_bulk_script(context),
    mendelian_randomization = .mednova_reference_mr_script(context),
    diagnostic_modeling = .mednova_reference_diagnostic_script(context),
    NULL
  )
}

.mednova_reference_psm_script <- function(context) {
  values <- context$values

  body <- c(
    paste0("analysis_data <- ", values$data_name),
    "",
    "psm_res <- med_psm(",
    "  data = analysis_data,",
    paste0("  treat_col = ", .mednova_string_literal(values$treatment), ","),
    paste0("  covariates = ", .mednova_character_vector_code(values$covariates), ","),
    "  return_baseline = TRUE,",
    "  return_love_plot = TRUE",
    ")",
    "",
    "utils::head(psm_res$matched_data)",
    "psm_res$balance_table",
    "psm_res$baseline_tables"
  )

  body <- c(
    body,
    "",
    "if (!is.null(psm_res$love_plot)) {",
    "  print(psm_res$love_plot)",
    "}"
  )

  list(
    packages = c("MedNova"),
    body = paste(body, collapse = "\n")
  )
}

.mednova_reference_logistic_script <- function(context) {
  values <- context$values

  body <- c(
    paste0("analysis_data <- ", values$data_name),
    "",
    "logistic_res <- med_logistic(",
    "  data = analysis_data,",
    paste0("  outcome_col = ", .mednova_string_literal(values$outcome), ","),
    paste0("  predictors = ", .mednova_character_vector_code(values$covariates), ","),
    "  mode = \"both\"",
    ")",
    "",
    "logistic_res$multiv$results",
    "logistic_res$univ$results"
  )

  list(
    packages = c("MedNova"),
    body = paste(body, collapse = "\n")
  )
}

.mednova_reference_cox_script <- function(context) {
  values <- context$values

  body <- c(
    paste0("analysis_data <- ", values$data_name),
    "",
    "cox_res <- med_cox(",
    "  data = analysis_data,",
    paste0("  time_col = ", .mednova_string_literal(values$time_col), ","),
    paste0("  status_col = ", .mednova_string_literal(values$event_col), ","),
    paste0("  predictors = ", .mednova_character_vector_code(values$covariates)),
    ")",
    "",
    "cox_res$multiv$results"
  )

  list(
    packages = c("MedNova"),
    body = paste(body, collapse = "\n")
  )
}

.mednova_reference_bulk_script <- function(context) {
  values <- context$values
  body <- switch(
    values$input_style,
    sample_table = c(
      paste0("analysis_data <- ", values$data_name),
      "",
      if (!is.null(values$sample_id)) {
        paste0("sample_ids <- as.character(analysis_data[[", .mednova_string_literal(values$sample_id), "]])")
      } else {
        "sample_ids <- rownames(analysis_data)"
      },
      "if (is.null(sample_ids)) {",
      "  sample_ids <- paste0(\"Sample\", seq_len(nrow(analysis_data)))",
      "}",
      paste0("feature_columns <- ", .mednova_character_vector_code(values$feature_columns)),
      "count_matrix <- t(as.matrix(analysis_data[, feature_columns, drop = FALSE]))",
      "colnames(count_matrix) <- sample_ids",
      "rownames(count_matrix) <- feature_columns",
      "group_data <- data.frame(",
      "  sample_id = sample_ids,",
      paste0("  group = analysis_data[[", .mednova_string_literal(values$group_column), "]],"),
      "  stringsAsFactors = FALSE,",
      "  check.names = FALSE",
      ")",
      "",
      "bulk_res <- med_bulk_deg(",
      "  counts = count_matrix,",
      "  group = group_data,",
      "  group_col = \"group\",",
      "  sample_col = \"sample_id\",",
      paste0("  case_name = ", .mednova_string_literal(values$case_level), ","),
      paste0("  control_name = ", .mednova_string_literal(values$control_level)),
      ")",
      "",
      "utils::head(bulk_res$deg$results)"
    ),
    matrix = c(
      paste0("count_matrix <- ", values$count_matrix_name),
      paste0("group_data <- ", values$group_data_name),
      "",
      "bulk_res <- med_bulk_deg(",
      "  counts = count_matrix,",
      "  group = group_data,",
      paste0("  group_col = ", .mednova_string_literal(values$group_column), ","),
      if (!is.null(values$sample_id)) paste0("  sample_col = ", .mednova_string_literal(values$sample_id), ","),
      paste0("  case_name = ", .mednova_string_literal(values$case_level), ","),
      paste0("  control_name = ", .mednova_string_literal(values$control_level)),
      ")",
      "",
      "utils::head(bulk_res$deg$results)"
    ),
    c(
      paste0("count_matrix <- ", values$count_matrix_name),
      paste0("group_data <- ", values$group_data_name),
      "",
      "bulk_res <- med_bulk_deg(",
      "  counts = count_matrix,",
      "  group = group_data,",
      paste0("  group_col = ", .mednova_string_literal(values$group_column), ","),
      if (!is.null(values$sample_id)) paste0("  sample_col = ", .mednova_string_literal(values$sample_id), ","),
      paste0("  case_name = ", .mednova_string_literal(values$case_level), ","),
      paste0("  control_name = ", .mednova_string_literal(values$control_level)),
      ")",
      "",
      "utils::head(bulk_res$deg$results)"
    )
  )

  list(
    packages = c("MedNova"),
    body = paste(body, collapse = "\n")
  )
}

.mednova_reference_mr_script <- function(context) {
  values <- context$values
  col_map_code <- paste0(
    "list(\n",
    paste0(
      "    ",
      names(values$col_map),
      " = ",
      vapply(unname(values$col_map), .mednova_string_literal, character(1L)),
      collapse = ",\n"
    ),
    "\n  )"
  )

  body <- c(
    paste0("exposure_data <- ", values$exposure_data_name),
    "",
    "mr_res <- med_mr(",
    "  exposure_data_list = list(exposure_data),",
    paste0("  exposure_names = c(", .mednova_string_literal(values$exposure_name), "),"),
    paste0("  outcome_data = ", .mednova_name(values$outcome_data_name), ","),
    paste0("  exposure_col_map = ", col_map_code, ","),
    "  outcome_col_map = list(",
    "    snp = \"SNP\",",
    "    beta = \"BETA\",",
    "    se = \"SE\",",
    "    eaf = \"EAF\",",
    "    effect_allele = \"A1\",",
    "    other_allele = \"A2\",",
    "    pval = \"P\",",
    "    n = \"N\",",
    "    chr = \"CHR\",",
    "    pos = \"POS\"",
    "  ),",
    "  clump_params = list(",
    "    perform = FALSE,",
    "    kb = 10000,",
    "    r2 = 0.001,",
    "    pval = 5e-8,",
    "    local = FALSE,",
    "    plink_bin = NULL,",
    "    bfile = NULL",
    "  ),",
    "  cores = 1,",
    "  out_dir = \"MR_results\"",
    ")",
    "",
    "mr_res$completed"
  )

  list(
    packages = c("MedNova"),
    body = paste(body, collapse = "\n")
  )
}

.mednova_reference_diagnostic_script <- function(context) {
  values <- context$values

  body <- c(
    paste0("analysis_data <- ", values$data_name),
    "",
    if (!is.null(values$sample_id)) {
      paste0("sample_ids <- as.character(analysis_data[[", .mednova_string_literal(values$sample_id), "]])")
    } else {
      "sample_ids <- rownames(analysis_data)"
    },
    "if (is.null(sample_ids)) {",
    "  sample_ids <- paste0(\"Sample\", seq_len(nrow(analysis_data)))",
    "}",
    paste0("target_genes <- ", .mednova_character_vector_code(values$covariates)),
    "expr_matrix <- t(as.matrix(analysis_data[, target_genes, drop = FALSE]))",
    "colnames(expr_matrix) <- sample_ids",
    "rownames(expr_matrix) <- target_genes",
    "group_data <- data.frame(",
    "  sample_id = sample_ids,",
    paste0("  group = analysis_data[[", .mednova_string_literal(values$outcome), "]],"),
    "  stringsAsFactors = FALSE,",
    "  check.names = FALSE",
    ")",
    "case_label <- unique(group_data$group)[length(unique(group_data$group))]",
    "",
    "diagnostic_res <- med_diagnostic_model(",
    "  expr = expr_matrix,",
    "  group_df = group_data,",
    "  target_genes = target_genes,",
    "  sample_col = \"sample_id\",",
    "  group_col = \"group\",",
    "  case_label = case_label,",
    "  include_roc = TRUE",
    ")",
    "",
    "diagnostic_res$model"
  )

  list(
    packages = c("MedNova"),
    body = paste(body, collapse = "\n")
  )
}

.mednova_has_migrated_functions <- function(selected_functions, function_names) {
  if (is.null(selected_functions) || !nrow(selected_functions)) {
    return(FALSE)
  }

  all(function_names %in% selected_functions$function_name) &&
    all(function_names %in% .mednova_migrated_function_names())
}

.mednova_migrated_function_names <- function() {
  c(
    "Epi_PSM_match",
    "Epi_PSM_baseline_tables",
    "Epi_logistic_univ",
    "Epi_logistic_multiv",
    "Epi_cox_multiv",
    "Bio_Bulk_align_group",
    "Bio_Bulk_filter_low_expression",
    "Bio_Bulk_limma_analysis",
    "Bio_MR_process_exposure",
    "Bio_MR_prepare_outcome",
    "Bio_MR_pipeline",
    "Bio_build_diagnostic_model",
    "plot_love",
    "plot_logistic_forest",
    "plot_diagnostic_roc",
    "plot_volcano_advanced",
    "med_psm",
    "med_logistic",
    "med_cox",
    "med_bulk_deg",
    "med_mr",
    "med_diagnostic_model"
  )
}

.mednova_base_assumptions <- function(data_profile) {
  assumptions <- c(
    paste0("The object `", data_profile$object_name, "` already exists in the current R session.")
  )

  if (!is.null(data_profile$structure_guess) && nzchar(data_profile$structure_guess)) {
    assumptions <- c(
      assumptions,
      paste0(
        "The data look most like `",
        data_profile$structure_guess,
        "` based on the current lightweight heuristics."
      )
    )
  }

  assumptions
}

.mednova_covariate_assumption <- function(covariates) {
  if (!length(covariates)) {
    return("No predictor columns were confidently identified, so you should update the formula before running the script.")
  }

  paste0("Predictor columns used by default: ", paste(covariates, collapse = ", "), ".")
}

.mednova_build_assumptions <- function(domain, data, spec, data_profile) {
  .mednova_build_script_context(
    domain = domain,
    data = data,
    spec = spec,
    data_profile = data_profile
  )$assumptions
}

.mednova_read_template_text <- function(template_id) {
  template_file <- paste0(template_id, ".R")
  paste(
    readLines(.mednova_resource_path("templates", template_file), warn = FALSE, encoding = "UTF-8"),
    collapse = "\n"
  )
}

.mednova_script_header <- function(task_match, data_profile, assumptions, packages) {
  domain_label <- task_match$task_label %||% task_match$domain
  header <- c(
    "# MedNova generated script",
    paste0("# Task domain: ", domain_label),
    paste0("# Data object: ", data_profile$object_name)
  )

  if (length(assumptions)) {
    header <- c(
      header,
      "# Assumptions:",
      paste0("# - ", assumptions)
    )
  }

  c(
    header,
    "",
    paste0("library(", packages, ")"),
    ""
  )
}

.mednova_template_libraries <- function(template_id) {
  switch(
    template_id,
    psm_basic = c("MatchIt", "stats"),
    logistic_basic = c("stats"),
    cox_basic = c("survival", "stats"),
    bulk_deg = c("DESeq2", "stats"),
    mr_basic = c("TwoSampleMR"),
    diagnostic_basic = c("pROC", "stats"),
    "stats"
  )
}

.mednova_fill_template <- function(template_text, replacements) {
  for (name in names(replacements)) {
    template_text <- gsub(
      paste0("{{", name, "}}"),
      replacements[[name]],
      template_text,
      fixed = TRUE
    )
  }

  template_text
}

.mednova_resolve_data_name <- function(data, expr) {
  if (is.character(data) && length(data) == 1L && nzchar(trimws(data))) {
    return(data)
  }

  paste(deparse(expr), collapse = "")
}

.mednova_covariates <- function(column_names,
                                requested,
                                exclude = character(),
                                max_default = 3L) {
  if (!is.null(requested) && length(requested)) {
    return(as.character(requested))
  }

  candidates <- setdiff(column_names, exclude)

  if (!length(candidates)) {
    return(character())
  }

  head(candidates, max_default)
}

.mednova_formula_text <- function(lhs, rhs_terms) {
  paste(.mednova_name(lhs), .mednova_rhs_text(rhs_terms), sep = " ~ ")
}

.mednova_rhs_text <- function(rhs_terms) {
  if (!length(rhs_terms)) {
    return("1")
  }

  paste(vapply(rhs_terms, .mednova_name, character(1L)), collapse = " + ")
}

.mednova_name <- function(x) {
  if (!is.character(x) || length(x) != 1L || !nzchar(x)) {
    return(x)
  }

  if (grepl("^[.A-Za-z][.A-Za-z0-9_]*$", x)) {
    return(x)
  }

  paste0("`", gsub("`", "\\\\`", x), "`")
}

.mednova_string_literal <- function(x) {
  paste0("\"", gsub("\"", "\\\\\"", x), "\"")
}

.mednova_character_vector_code <- function(x) {
  if (!length(x)) {
    return("character()")
  }

  paste0("c(", paste(vapply(x, .mednova_string_literal, character(1L)), collapse = ", "), ")")
}

.mednova_first_value <- function(...) {
  values <- list(...)

  for (value in values) {
    if (is.null(value)) {
      next
    }

    if (length(value) == 1L && is.character(value) && nzchar(trimws(value))) {
      return(value)
    }
  }

  stop("A required script placeholder could not be resolved.", call. = FALSE)
}

.mednova_optional_first_value <- function(...) {
  values <- list(...)

  for (value in values) {
    if (is.null(value)) {
      next
    }

    if (length(value) == 1L && is.character(value) && nzchar(trimws(value))) {
      return(value)
    }
  }

  NULL
}

.mednova_detect_two_levels <- function(data,
                                       column_name,
                                       defaults = c("Control", "Case")) {
  if (is.data.frame(data) &&
      is.character(column_name) &&
      length(column_name) == 1L &&
      nzchar(column_name) &&
      column_name %in% names(data)) {
    values <- unique(stats::na.omit(as.character(data[[column_name]])))

    if (length(values) >= 2L) {
      return(values[1:2])
    }
  }

  defaults
}
