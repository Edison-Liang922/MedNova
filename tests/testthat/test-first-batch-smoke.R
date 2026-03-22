testthat::test_that("med_psm smoke test returns unified PSM outputs", {
  testthat::skip_if_not_installed("MatchIt")

  dat <- data.frame(
    treat = c(1, 1, 1, 1, 0, 0, 0, 0, 1, 0, 1, 0),
    age = c(60, 62, 58, 57, 59, 61, 56, 55, 63, 64, 54, 60),
    sex = c("F", "M", "F", "M", "F", "M", "F", "M", "F", "M", "M", "F"),
    bmi = c(24, 27, 23, 26, 25, 28, 22, 24, 29, 27, 23, 26)
  )

  res <- med_psm(
    data = dat,
    treat_col = "treat",
    covariates = c("age", "sex", "bmi"),
    return_baseline = FALSE,
    return_love_plot = FALSE,
    caliper = 1
  )

  testthat::expect_true(is.list(res))
  testthat::expect_s3_class(res, "mednova_psm_result")
  testthat::expect_true(inherits(res$matched_object, "matchit"))
  testthat::expect_true(is.data.frame(res$matched_data))
  testthat::expect_true(is.data.frame(res$balance_table))
  testthat::expect_true(is.data.frame(res$love_plot_data))
  testthat::expect_true(all(c("Covariate", "AbsSMD_Before_max", "AbsSMD_After_max") %in% names(res$love_plot_data)))
  testthat::expect_null(res$baseline_tables)
  testthat::expect_null(res$love_plot)
})

testthat::test_that("med_logistic smoke test returns model outputs", {
  dat <- data.frame(
    outcome = c(1, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 1),
    age = c(61, 55, 68, 59, 64, 51, 70, 53, 58, 62, 57, 66),
    sex = c("F", "M", "F", "F", "M", "M", "F", "M", "F", "M", "M", "F"),
    bmi = c(25, 27, 24, 26, 28, 23, 29, 22, 27, 25, 24, 26)
  )

  res <- med_logistic(
    data = dat,
    outcome_col = "outcome",
    predictors = c("age", "sex", "bmi"),
    mode = "both"
  )

  testthat::expect_true(is.list(res))
  testthat::expect_true("multiv" %in% names(res))
  testthat::expect_true("univ" %in% names(res))
  testthat::expect_true(is.data.frame(res$multiv$results))
})

testthat::test_that("med_cox smoke test returns cox results", {
  testthat::skip_if_not_installed("survival")

  dat <- data.frame(
    time = c(5, 8, 12, 4, 9, 11, 6, 10),
    status = c(1, 0, 1, 1, 0, 1, 0, 1),
    age = c(61, 55, 68, 59, 64, 51, 70, 53),
    sex = c("F", "M", "F", "F", "M", "M", "F", "M")
  )

  res <- med_cox(
    data = dat,
    time_col = "time",
    status_col = "status",
    predictors = c("age", "sex")
  )

  testthat::expect_true(is.list(res))
  testthat::expect_true(is.data.frame(res$multiv$results))
})

testthat::test_that("med_bulk_deg smoke test returns deg results", {
  testthat::skip_if_not_installed("limma")

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

  res <- med_bulk_deg(
    counts = counts,
    group = group,
    group_col = "group",
    case_name = "Case",
    control_name = "Control"
  )

  testthat::expect_true(is.list(res))
  testthat::expect_true(is.data.frame(res$deg$results))
})

testthat::test_that("med_mr smoke test returns pipeline structure", {
  if (!identical(tolower(Sys.getenv("MEDNOVA_RUN_HEAVY_SMOKE", "false")), "true")) {
    testthat::skip("Set MEDNOVA_RUN_HEAVY_SMOKE=true to run heavy MR smoke tests.")
  }
  testthat::skip_if_not_installed("TwoSampleMR")
  testthat::skip_if_not_installed("openxlsx")
  testthat::skip_if_not_installed("dplyr")
  testthat::skip_if_not_installed("tidyr")

  exposure_raw <- data.frame(
    SNP = c("rs1", "rs2", "rs3"),
    BETA = c(0.25, 0.20, 0.18),
    SE = c(0.05, 0.04, 0.03),
    EAF = c(0.30, 0.40, 0.45),
    A1 = c("A", "C", "G"),
    A2 = c("G", "T", "A"),
    P = c(1e-8, 3e-7, 8e-6),
    N = c(10000, 10000, 10000),
    CHR = c(1, 1, 2),
    POS = c(101, 202, 303)
  )

  outcome_raw <- data.frame(
    SNP = c("rs1", "rs2", "rs3"),
    BETA = c(0.10, 0.08, 0.06),
    SE = c(0.03, 0.03, 0.02),
    EAF = c(0.30, 0.40, 0.45),
    A1 = c("A", "C", "G"),
    A2 = c("G", "T", "A"),
    P = c(0.01, 0.02, 0.03),
    N = c(12000, 12000, 12000),
    CHR = c(1, 1, 2),
    POS = c(101, 202, 303)
  )

  out_dir <- tempfile(pattern = "mednova-mr-")

  res <- med_mr(
    exposure_data_list = list(exposure_raw),
    exposure_names = c("Exposure1"),
    outcome_data = outcome_raw,
    clump_params = list(
      perform = FALSE,
      kb = 10000,
      r2 = 0.001,
      pval = 5e-8,
      local = FALSE,
      plink_bin = NULL,
      bfile = NULL
    ),
    cores = 1,
    out_dir = out_dir
  )

  testthat::expect_true(is.list(res))
  testthat::expect_true(all(c("exposure_list", "outcome_dat", "harmonized_list", "completed") %in% names(res)))
  testthat::expect_true(dir.exists(out_dir))
})

testthat::test_that("med_diagnostic_model smoke test returns model bundle", {
  if (!identical(tolower(Sys.getenv("MEDNOVA_RUN_HEAVY_SMOKE", "false")), "true")) {
    testthat::skip("Set MEDNOVA_RUN_HEAVY_SMOKE=true to run heavy diagnostic smoke tests.")
  }
  testthat::skip_if_not_installed("rms")
  testthat::skip_if_not_installed("dcurves")
  testthat::skip_if_not_installed("ggplot2")
  testthat::skip_if_not_installed("dplyr")

  expr <- matrix(
    c(
      1.2, 1.1, 1.4, 2.1, 2.0, 2.2,
      3.1, 3.0, 3.2, 4.0, 4.2, 4.1
    ),
    nrow = 2,
    byrow = TRUE
  )
  rownames(expr) <- c("GeneA", "GeneB")
  colnames(expr) <- paste0("S", 1:6)

  group_df <- data.frame(group = c("Control", "Control", "Control", "Case", "Case", "Case"))
  rownames(group_df) <- colnames(expr)

  project_name <- file.path(tempdir(), "mednova_diag")

  res <- med_diagnostic_model(
    expr = expr,
    group_df = group_df,
    target_genes = c("GeneA", "GeneB"),
    project_name = project_name,
    include_roc = FALSE
  )

  testthat::expect_true(is.list(res))
  testthat::expect_true(is.list(res$model))
  testthat::expect_true(file.exists(paste0(project_name, "_Nomogram.pdf")))
  testthat::expect_true(file.exists(paste0(project_name, "_DCA.pdf")))
})
