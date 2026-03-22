#' 将任务描述匹配到 MedNova 支持的任务域
#'
#' @param task 自然语言任务描述。
#'
#' @return 返回一个 list，包含 `domain`、`template_id`、
#'   `selected_functions`、`task_label`、`matched_terms` 与 `confidence`。
#' @export
mednova_match_task <- function(task) {
  if (!is.character(task) || length(task) != 1L || !nzchar(trimws(task))) {
    stop("`task` must be a non-empty character string.", call. = FALSE)
  }

  task_text <- tolower(task)
  patterns <- .mednova_task_patterns()
  scores <- vapply(
    patterns,
    function(task_patterns) {
      sum(vapply(unname(task_patterns), function(pattern) grepl(pattern, task_text, perl = TRUE), logical(1L)))
    },
    integer(1L)
  )

  if (!any(scores > 0L)) {
    supported <- paste(unname(.mednova_task_labels()), collapse = ", ")
    stop(
      "Unsupported task description. Supported tasks are: ",
      supported,
      call. = FALSE
    )
  }

  best_key <- names(scores)[which.max(scores)]
  matched_terms <- names(patterns[[best_key]])[vapply(
    unname(patterns[[best_key]]),
    function(pattern) grepl(pattern, task_text, perl = TRUE),
    logical(1L)
  )]
  selected_functions <- .mednova_catalog_for_domain(best_key)

  list(
    domain = best_key,
    task_key = best_key,
    task_label = .mednova_task_labels()[[best_key]],
    template_id = .mednova_template_id_for_domain(best_key),
    selected_functions = selected_functions,
    matched_terms = matched_terms,
    confidence = unname(scores[[best_key]] / length(patterns[[best_key]]))
  )
}

.mednova_resolve_task <- function(task) {
  if (is.list(task) && !is.null(task$domain)) {
    if (is.null(task$template_id)) {
      task$template_id <- .mednova_template_id_for_domain(task$domain)
    }
    if (is.null(task$task_label)) {
      task$task_label <- .mednova_task_labels()[[task$domain]]
    }
    if (is.null(task$selected_functions)) {
      task$selected_functions <- .mednova_catalog_for_domain(task$domain)
    }
    if (is.null(task$task_key)) {
      task$task_key <- task$domain
    }
    return(task)
  }

  if (is.list(task) && !is.null(task$task_key)) {
    task$domain <- task$task_key
    return(.mednova_resolve_task(task))
  }

  mednova_match_task(task)
}

.mednova_task_labels <- function() {
  c(
    propensity_score_matching = "propensity score matching",
    logistic_regression = "logistic regression",
    cox_regression = "Cox regression",
    bulk_rna_differential_expression = "bulk RNA differential expression",
    mendelian_randomization = "Mendelian randomization",
    diagnostic_modeling = "diagnostic modeling"
  )
}

.mednova_task_patterns <- function() {
  list(
    propensity_score_matching = c(
      psm = "\\bpsm\\b",
      matching = "matching",
      propensity = "propensity",
      love_plot = "love plot",
      matchit = "matchit"
    ),
    logistic_regression = c(
      logistic = "logistic",
      logit = "logit",
      odds_ratio = "odds ratio",
      odds_ratios = "odds ratios"
    ),
    cox_regression = c(
      cox = "\\bcox\\b",
      survival = "survival",
      hazard = "hazard",
      hazard_ratio = "hazard ratio"
    ),
    bulk_rna_differential_expression = c(
      limma = "limma",
      differential_expression = "differential expression",
      volcano = "volcano",
      deg = "\\bdeg\\b",
      bulk_rna = "bulk rna"
    ),
    mendelian_randomization = c(
      mr = "\\bmr\\b",
      mendelian_randomization = "mendelian randomization",
      two_sample_mr = "two[- ]sample mr"
    ),
    diagnostic_modeling = c(
      diagnostic = "diagnostic",
      roc = "\\broc\\b",
      auc = "\\bauc\\b",
      classifier = "classifier",
      nomogram = "nomogram"
    )
  )
}

.mednova_template_id_for_domain <- function(domain) {
  switch(
    domain,
    propensity_score_matching = "psm_basic",
    logistic_regression = "logistic_basic",
    cox_regression = "cox_basic",
    bulk_rna_differential_expression = "bulk_deg",
    mendelian_randomization = "mr_basic",
    diagnostic_modeling = "diagnostic_basic",
    stop("Unsupported domain: ", domain, call. = FALSE)
  )
}

.mednova_catalog_for_domain <- function(domain) {
  function_catalog <- .mednova_read_csv_resource("catalog", "function_catalog.csv")
  task_column <- if ("task_type" %in% names(function_catalog)) "task_type" else "task_key"
  selected <- function_catalog[function_catalog[[task_column]] == domain, , drop = FALSE]

  if (nrow(selected) == 0L) {
    return(selected)
  }

  if ("priority" %in% names(selected)) {
    priority_num <- suppressWarnings(as.numeric(selected$priority))
    priority_num[is.na(priority_num)] <- Inf
    selected <- selected[order(priority_num, selected$function_name), , drop = FALSE]
    rownames(selected) <- NULL
  }

  selected
}
