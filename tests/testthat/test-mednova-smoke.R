testthat::test_that("task assistant returns the required result structure", {
  example_data <- data.frame(
    outcome = c(1, 0, 1),
    age = c(65, 52, 71),
    sex = c("F", "M", "F")
  )

  result <- mednova_task_assistant(
    data = example_data,
    task = "Fit a logistic regression model with odds ratio output",
    spec = list(outcome = "outcome", covariates = c("age", "sex"))
  )

  testthat::expect_s3_class(result, "mednova_result")
  testthat::expect_true(all(c(
    "data_profile",
    "matched_domain",
    "selected_functions",
    "workflow_steps",
    "assumptions",
    "script"
  ) %in% names(result)))
  testthat::expect_equal(result$matched_domain, "logistic_regression")
  testthat::expect_true(is.data.frame(result$selected_functions))
  testthat::expect_true(length(result$workflow_steps) >= 3L)
  testthat::expect_true(length(result$assumptions) >= 2L)
  testthat::expect_match(result$script, "library\\(MedNova\\)")
  testthat::expect_match(result$script, "analysis_data <- input_data", fixed = TRUE)
  testthat::expect_match(result$script, "med_logistic\\(")
})

testthat::test_that("data inspection returns a minimal profile", {
  example_data <- data.frame(
    treatment = c(1, 0, 1),
    outcome = c(1, 0, 1),
    age = c(65, 52, 71),
    sex = c("F", "M", "F")
  )

  profile <- mednova_inspect_data(example_data)

  testthat::expect_equal(profile$object_type, "data.frame")
  testthat::expect_true("age" %in% profile$numeric_columns)
  testthat::expect_true("sex" %in% profile$character_columns)
  testthat::expect_true(is.character(profile$structure_guess))
})

testthat::test_that("task matching returns template and selected functions", {
  match <- mednova_match_task("Build a Cox survival model and review hazard ratios")

  testthat::expect_equal(match$domain, "cox_regression")
  testthat::expect_equal(match$template_id, "cox_basic")
  testthat::expect_true(is.data.frame(match$selected_functions))
  testthat::expect_true(nrow(match$selected_functions) >= 1L)
})

testthat::test_that("workflow planning returns concise steps and catalog fields", {
  workflow <- mednova_plan_workflow("propensity score matching")

  testthat::expect_equal(workflow$domain, "propensity_score_matching")
  testthat::expect_true(length(workflow$workflow_steps) >= 3L)
  testthat::expect_true(all(c(
    "function_name",
    "module",
    "task_type",
    "input_mode",
    "required_cols",
    "output_type",
    "runtime_class",
    "side_effect",
    "keywords",
    "template_id",
    "priority"
  ) %in% names(workflow$selected_functions)))
})

testthat::test_that("psm script prefers MedNova migrated wrappers", {
  example_data <- data.frame(
    treatment = c(1, 0, 1),
    age = c(65, 52, 71),
    sex = c("F", "M", "F")
  )

  script <- mednova_task_assistant(
    data = example_data,
    task = "psm matching love plot",
    spec = list(treatment = "treatment", covariates = c("age", "sex")),
    output = "script"
  )

  testthat::expect_match(script, "library\\(MedNova\\)")
  testthat::expect_match(script, "med_psm\\(")
  testthat::expect_match(script, "return_baseline = TRUE", fixed = TRUE)
  testthat::expect_match(script, "return_love_plot = TRUE", fixed = TRUE)
  testthat::expect_match(script, "psm_res\\$balance_table")
})

testthat::test_that("assistant can return and save the script only", {
  example_data <- data.frame(
    outcome = c(1, 0, 1),
    age = c(65, 52, 71),
    sex = c("F", "M", "F")
  )
  out_file <- tempfile(fileext = ".R")

  script <- mednova_task_assistant(
    data = example_data,
    task = "diagnostic roc classifier",
    spec = list(outcome = "outcome", covariates = c("age", "sex")),
    output = "script",
    save_to = out_file
  )

  testthat::expect_true(file.exists(out_file))
  testthat::expect_true(is.character(script))
  testthat::expect_match(script, "library\\(MedNova\\)")
  testthat::expect_match(script, "med_diagnostic_model\\(")
})

testthat::test_that("bulk DEG script generation does not require DESeq2", {
  counts <- matrix(
    c(
      10, 12, 20, 22,
      50, 48, 46, 45,
      100, 102, 80, 78
    ),
    nrow = 3,
    byrow = TRUE
  )
  rownames(counts) <- c("Gene1", "Gene2", "Gene3")
  colnames(counts) <- c("S1", "S2", "S3", "S4")

  group <- data.frame(group = c("Control", "Control", "Case", "Case"))
  rownames(group) <- colnames(counts)

  script <- mednova_generate_script(
    task = list(
      domain = "bulk_rna_differential_expression",
      task_label = "bulk RNA differential expression",
      template_id = "bulk_deg"
    ),
    data = counts,
    spec = list(
      group = "group",
      group_data_name = "group_data",
      case_level = "Case",
      control_level = "Control"
    ),
    data_profile = mednova_inspect_data(counts, data_name = "count_matrix"),
    assumptions = character()
  )

  testthat::expect_match(script, "library\\(MedNova\\)")
  testthat::expect_match(script, "med_bulk_deg\\(")
  testthat::expect_false(grepl("DESeq2", script, fixed = TRUE))
})

testthat::test_that("explicit data_name overrides spec data_name", {
  example_data <- data.frame(
    outcome = c(1, 0, 1),
    age = c(65, 52, 71),
    sex = c("F", "M", "F")
  )

  script <- mednova_task_assistant(
    data = example_data,
    task = "Fit a logistic regression model",
    spec = list(
      outcome = "outcome",
      covariates = c("age", "sex"),
      data_name = "from_spec"
    ),
    data_name = "from_arg",
    output = "script"
  )

  testthat::expect_match(script, "analysis_data <- from_arg", fixed = TRUE)
  testthat::expect_false(grepl("analysis_data <- from_spec", script, fixed = TRUE))
})

testthat::test_that("spec data_name is used when explicit data_name is not provided", {
  example_data <- data.frame(
    outcome = c(1, 0, 1),
    age = c(65, 52, 71),
    sex = c("F", "M", "F")
  )

  script <- mednova_task_assistant(
    data = example_data,
    task = "Fit a logistic regression model",
    spec = list(
      outcome = "outcome",
      covariates = c("age", "sex"),
      data_name = "from_spec"
    ),
    output = "script"
  )

  testthat::expect_match(script, "analysis_data <- from_spec", fixed = TRUE)
})
