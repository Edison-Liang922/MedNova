#' 为 MedNova 任务生成最小格式建议
#'
#' 根据数据概况与匹配到的任务域，生成轻量级格式建议。这个辅助函数主要面向
#' MedNova Studio 等说明层使用场景，在不改变核心任务助手流程的前提下提供
#' 简洁提示。
#'
#' @param data_profile [mednova_inspect_data()] 返回的 list。
#' @param matched_domain MedNova 支持的任务域之一。
#' @param selected_functions 可选的数据框，表示当前选中的推荐函数。
#'
#' @return 返回一个 list，包含 `structure_guess`、`suggested_column_map`、
#'   `missing_roles` 与 `notes`。
#' @export
mednova_format_suggestions <- function(data_profile,
                                       matched_domain = NULL,
                                       selected_functions = NULL) {
  if (!is.list(data_profile) || is.null(data_profile$structure_guess)) {
    stop("`data_profile` must be the result of `mednova_inspect_data()`.", call. = FALSE)
  }

  guessed_columns <- data_profile$guessed_columns %||% list()
  suggested_column_map <- if (length(guessed_columns)) {
    data.frame(
      canonical_role = names(guessed_columns),
      detected_column = unname(unlist(guessed_columns)),
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      canonical_role = character(),
      detected_column = character(),
      stringsAsFactors = FALSE
    )
  }

  expected_roles <- .mednova_expected_roles_for_domain(matched_domain)
  present_roles <- intersect(expected_roles, names(guessed_columns))
  missing_roles <- setdiff(expected_roles, names(guessed_columns))
  notes <- c(
    paste0("Current data look most like `", data_profile$structure_guess, "`.")
  )

  if (!is.null(data_profile$structure_reason) && nzchar(data_profile$structure_reason)) {
    notes <- c(notes, data_profile$structure_reason)
  }

  if (length(present_roles)) {
    mapped_text <- paste(
      paste0(
        present_roles,
        " -> ",
        vapply(present_roles, function(role) guessed_columns[[role]], character(1L))
      ),
      collapse = "; "
    )
    notes <- c(notes, paste0("Likely column mappings: ", mapped_text, "."))
  }

  if (length(missing_roles)) {
    notes <- c(
      notes,
      paste0("Likely missing or unidentified task roles: ", paste(missing_roles, collapse = ", "), ".")
    )
  }

  mismatch_note <- .mednova_structure_mismatch_note(
    structure_guess = data_profile$structure_guess,
    matched_domain = matched_domain
  )
  if (!is.null(mismatch_note)) {
    notes <- c(notes, mismatch_note)
  }

  if (length(data_profile$column_names) &&
      any(grepl("[^A-Za-z0-9_.]", data_profile$column_names))) {
    notes <- c(
      notes,
      "Some column names contain spaces or punctuation; consider simplifying them before running the generated script."
    )
  }

  if (is.data.frame(selected_functions) && nrow(selected_functions) > 0) {
    notes <- c(
      notes,
      paste0(
        "Recommended functions currently start with: ",
        paste(utils::head(selected_functions$function_name, 3L), collapse = ", "),
        "."
      )
    )
  }

  list(
    structure_guess = data_profile$structure_guess,
    suggested_column_map = suggested_column_map,
    missing_roles = missing_roles,
    notes = unique(notes)
  )
}

.mednova_expected_roles_for_domain <- function(matched_domain) {
  if (is.null(matched_domain) || !nzchar(matched_domain)) {
    return(character())
  }

  switch(
    matched_domain,
    propensity_score_matching = c("treatment"),
    logistic_regression = c("outcome"),
    cox_regression = c("time", "event"),
    bulk_rna_differential_expression = c("group"),
    mendelian_randomization = c("snp", "beta", "se", "pval"),
    diagnostic_modeling = c("outcome"),
    character()
  )
}

.mednova_structure_mismatch_note <- function(structure_guess, matched_domain) {
  if (is.null(matched_domain) || !nzchar(matched_domain)) {
    return(NULL)
  }

  if (matched_domain %in% c(
    "propensity_score_matching",
    "logistic_regression",
    "cox_regression",
    "diagnostic_modeling"
  ) && identical(structure_guess, "expression_data")) {
    return("This task usually expects a clinical-style table, but the uploaded object looks more like expression data.")
  }

  if (identical(matched_domain, "bulk_rna_differential_expression") &&
      !identical(structure_guess, "expression_data")) {
    return("Bulk RNA differential expression usually expects an expression matrix or a list with counts and sample metadata.")
  }

  if (identical(matched_domain, "mendelian_randomization") &&
      !identical(structure_guess, "gwas_summary")) {
    return("Mendelian randomization usually expects GWAS summary statistics with SNP, allele, effect size, and P-value columns.")
  }

  NULL
}
