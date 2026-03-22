#' 为支持的任务创建最小流程方案
#'
#' @param task 任务描述，或者 `mednova_match_task()` 的返回结果。
#' @param data_profile 可选的 `mednova_inspect_data()` 输出结果。
#'
#' @return 返回一个 list，包含 `workflow_steps`、`data_requirements`
#'   与 `selected_functions`。
#' @export
mednova_plan_workflow <- function(task, data_profile = NULL) {
  task_match <- .mednova_resolve_task(task)
  selected_functions <- task_match$selected_functions %||%
    .mednova_catalog_for_domain(task_match$domain)

  list(
    domain = task_match$domain,
    template_id = task_match$template_id,
    task_label = task_match$task_label,
    workflow_steps = .mednova_workflow_steps(task_match$domain),
    data_requirements = .mednova_data_requirements(task_match$domain),
    data_structure = if (is.null(data_profile)) NULL else data_profile$structure_guess,
    selected_functions = selected_functions,
    recommended_functions = selected_functions
  )
}

.mednova_workflow_steps <- function(domain) {
  switch(
    domain,
    propensity_score_matching = c(
      "Confirm the treatment column and baseline covariates.",
      "Estimate propensity scores and perform matching.",
      "Review balance after matching.",
      "Use the matched dataset for downstream analysis."
    ),
    logistic_regression = c(
      "Confirm the binary outcome and candidate predictors.",
      "Optionally screen predictors and check collinearity.",
      "Fit a multivariable logistic regression model.",
      "Review odds ratios and predicted probabilities."
    ),
    cox_regression = c(
      "Confirm time, event, and predictor columns.",
      "Fit a Cox proportional hazards model.",
      "Check the proportional hazards assumption.",
      "Review hazard ratios and model discrimination."
    ),
    bulk_rna_differential_expression = c(
      "Prepare the count matrix and sample metadata.",
      "Align samples and confirm the case-control grouping column.",
      "Fit a differential expression model for the target contrast.",
      "Review the results table and volcano-style summaries."
    ),
    mendelian_randomization = c(
      "Prepare exposure and outcome summary statistics.",
      "Format and harmonize the two datasets.",
      "Run the core Mendelian randomization methods.",
      "Review sensitivity analyses such as heterogeneity or pleiotropy."
    ),
    diagnostic_modeling = c(
      "Confirm the outcome column and candidate features.",
      "Fit a diagnostic classification model.",
      "Generate predicted probabilities or scores.",
      "Evaluate ROC AUC and supporting diagnostic plots."
    ),
    stop("Unsupported domain: ", domain, call. = FALSE)
  )
}

.mednova_data_requirements <- function(domain) {
  switch(
    domain,
    propensity_score_matching = c("treatment", "covariates"),
    logistic_regression = c("outcome", "predictors"),
    cox_regression = c("time", "event", "predictors"),
    bulk_rna_differential_expression = c("count matrix", "sample metadata", "group"),
    mendelian_randomization = c("exposure summary table", "outcome summary table"),
    diagnostic_modeling = c("outcome", "predictors or feature matrix"),
    character()
  )
}
