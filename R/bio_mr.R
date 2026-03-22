#' Process Exposure Data for Mendelian Randomization
#'
#' A lightweight MedNova migration of the reference exposure-processing
#' pipeline: format, instrument-strength filtering, and optional clumping.
#'
#' @param raw_data Raw GWAS exposure summary statistics.
#' @param phenotype_name Exposure phenotype name.
#' @param col_map Named list mapping standard MR fields to columns in
#'   `raw_data`.
#' @param filter_pval P-value threshold applied before formatting.
#' @param filter_f F-statistic threshold used after formatting.
#' @param clump_params Named list of clumping parameters. Supported keys are
#'   `perform`, `kb`, `r2`, `pval`, `local`, `plink_bin`, and `bfile`.
#'
#' @return A processed exposure data frame, or `NULL` if no SNPs remain.
#' @export
Bio_MR_process_exposure <- function(raw_data,
                                    phenotype_name,
                                    col_map = .mednova_mr_default_col_map(),
                                    filter_pval = 1e-5,
                                    filter_f = 10,
                                    clump_params = list(
                                      perform = TRUE,
                                      kb = 10000,
                                      r2 = 0.001,
                                      pval = 5e-8,
                                      local = FALSE,
                                      plink_bin = NULL,
                                      bfile = NULL
                                    )) {
  .mednova_mr_require("TwoSampleMR")

  if (is.null(raw_data) || nrow(raw_data) == 0) {
    return(NULL)
  }

  exposure_dat <- .mednova_mr_format_exposure(
    raw_data = raw_data,
    phenotype_name = phenotype_name,
    col_map = col_map,
    pval_filter = filter_pval
  )

  if (is.null(exposure_dat) || nrow(exposure_dat) == 0) {
    return(NULL)
  }

  exposure_dat <- .mednova_mr_calc_r2(exposure_dat)
  exposure_dat <- .mednova_mr_calc_f(exposure_dat)
  exposure_dat <- .mednova_mr_filter_f(exposure_dat, f_thresh = filter_f)

  if (is.null(exposure_dat) || nrow(exposure_dat) == 0) {
    return(NULL)
  }

  clump_params <- utils::modifyList(
    list(
      perform = TRUE,
      kb = 10000,
      r2 = 0.001,
      pval = 5e-8,
      local = FALSE,
      plink_bin = NULL,
      bfile = NULL
    ),
    clump_params
  )

  if (isTRUE(clump_params$perform)) {
    exposure_dat <- .mednova_mr_clump_dispatch(
      exposure_dat = exposure_dat,
      perform = clump_params$perform,
      kb = clump_params$kb,
      r2 = clump_params$r2,
      pval_thresh = clump_params$pval,
      local = clump_params$local,
      plink_bin = clump_params$plink_bin,
      bfile = clump_params$bfile
    )
  }

  if (is.null(exposure_dat) || nrow(exposure_dat) == 0) {
    return(NULL)
  }

  exposure_dat
}

#' Prepare Outcome Data for Mendelian Randomization
#'
#' Format a raw GWAS outcome table into a TwoSampleMR-compatible outcome
#' dataset.
#'
#' @param raw_data Raw GWAS outcome summary statistics.
#' @param phenotype_name Outcome phenotype name.
#' @param col_map Named list mapping standard MR fields to columns in
#'   `raw_data`.
#' @param phenotype_col Column name used as the phenotype label during
#'   formatting.
#'
#' @return A data frame in TwoSampleMR outcome format.
#' @export
Bio_MR_prepare_outcome <- function(raw_data,
                                   phenotype_name = "Outcome",
                                   col_map = .mednova_mr_default_col_map(),
                                   phenotype_col = "trait") {
  .mednova_mr_require("TwoSampleMR")

  .mednova_mr_format_outcome(
    raw_data = raw_data,
    phenotype_name = phenotype_name,
    col_map = col_map,
    phenotype_col = phenotype_col
  )
}

#' Run the First-Batch Mendelian Randomization Pipeline
#'
#' End-to-end MR workflow: process exposure datasets, prepare one outcome,
#' harmonize them, and run the reference-style batch analysis with output files.
#'
#' @param exposure_data_list List of raw exposure data frames.
#' @param exposure_names Character vector of exposure names.
#' @param outcome_data Raw outcome GWAS data frame.
#' @param exposure_col_map Named list mapping exposure columns.
#' @param outcome_col_map Named list mapping outcome columns.
#' @param outcome_name Outcome phenotype name.
#' @param filter_pval Exposure pre-filter P-value threshold.
#' @param filter_f Exposure F-statistic threshold.
#' @param clump_params Named list of clumping parameters.
#' @param cores Number of cores for harmonization and batch MR. When `cores <= 1`,
#'   MedNova falls back to a sequential path.
#' @param out_dir Output directory for MR result files.
#'
#' @return A list with `exposure_list`, `outcome_dat`, `harmonized_list`, and
#'   `completed`.
#' @export
Bio_MR_pipeline <- function(exposure_data_list,
                            exposure_names,
                            outcome_data,
                            exposure_col_map = .mednova_mr_default_col_map(),
                            outcome_col_map = .mednova_mr_default_col_map(),
                            outcome_name = "Outcome",
                            filter_pval = 1e-5,
                            filter_f = 10,
                            clump_params = list(
                              perform = TRUE,
                              kb = 10000,
                              r2 = 0.001,
                              pval = 5e-8,
                              local = FALSE,
                              plink_bin = NULL,
                              bfile = NULL
                            ),
                            cores = 10,
                            out_dir = "MR_results") {
  .mednova_mr_require(c("TwoSampleMR", "openxlsx", "dplyr", "tidyr"))

  if (!is.list(exposure_data_list)) {
    stop("`exposure_data_list` must be a list.", call. = FALSE)
  }
  if (!is.character(exposure_names) || length(exposure_names) != length(exposure_data_list)) {
    stop("`exposure_names` must match length of `exposure_data_list`.", call. = FALSE)
  }

  exposure_list <- vector("list", length(exposure_data_list))
  for (i in seq_along(exposure_data_list)) {
    exposure_list[[i]] <- Bio_MR_process_exposure(
      raw_data = exposure_data_list[[i]],
      phenotype_name = exposure_names[i],
      col_map = exposure_col_map,
      filter_pval = filter_pval,
      filter_f = filter_f,
      clump_params = clump_params
    )
  }
  exposure_list <- Filter(function(x) is.data.frame(x) && nrow(x) > 0, exposure_list)

  outcome_dat <- Bio_MR_prepare_outcome(
    raw_data = outcome_data,
    phenotype_name = outcome_name,
    col_map = outcome_col_map
  )

  harmonized_list <- .mednova_mr_harmonize_list(
    exposure_list = exposure_list,
    outcome_dat = outcome_dat,
    cores = cores
  )
  harmonized_list <- .mednova_mr_harmonize_post(harmonized_list)

  completed <- .mednova_mr_run_batch(
    harmonized_list = harmonized_list,
    out_dir = out_dir,
    cores = cores
  )

  list(
    exposure_list = exposure_list,
    outcome_dat = outcome_dat,
    harmonized_list = harmonized_list,
    completed = completed
  )
}

#' Unified MedNova Entry for Mendelian Randomization
#'
#' @param exposure_data_list List of raw exposure data frames, or a single raw
#'   exposure data frame.
#' @param exposure_names Character vector of exposure names.
#' @param outcome_data Raw outcome GWAS data frame.
#' @param ... Additional arguments passed to `Bio_MR_pipeline()`.
#'
#' @return The output of `Bio_MR_pipeline()`.
#' @export
med_mr <- function(exposure_data_list,
                   exposure_names,
                   outcome_data,
                   ...) {
  if (is.data.frame(exposure_data_list)) {
    exposure_data_list <- list(exposure_data_list)
  }

  Bio_MR_pipeline(
    exposure_data_list = exposure_data_list,
    exposure_names = exposure_names,
    outcome_data = outcome_data,
    ...
  )
}

.mednova_mr_default_col_map <- function() {
  list(
    snp = "SNP",
    beta = "BETA",
    se = "SE",
    eaf = "EAF",
    effect_allele = "A1",
    other_allele = "A2",
    pval = "P",
    n = "N",
    chr = "CHR",
    pos = "POS"
  )
}

.mednova_mr_require <- function(packages) {
  for (pkg in packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package '", pkg, "' is required.", call. = FALSE)
    }
  }
}

.mednova_mr_check_cols <- function(data,
                                   required_cols,
                                   data_name = "data",
                                   strict = TRUE) {
  if (!is.data.frame(data)) {
    stop("`", data_name, "` must be a data.frame.", call. = FALSE)
  }
  if (!is.character(required_cols)) {
    stop("`required_cols` must be a character vector.", call. = FALSE)
  }

  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) == 0) {
    return(invisible(TRUE))
  }

  if (isTRUE(strict)) {
    stop(
      data_name,
      " is missing required columns: ",
      paste(missing_cols, collapse = ", "),
      call. = FALSE
    )
  }

  missing_cols
}

.mednova_mr_cast_numeric <- function(data, cols) {
  .mednova_mr_check_cols(data, cols, data_name = "data")

  for (col in cols) {
    data[[col]] <- as.numeric(data[[col]])
  }

  data
}

.mednova_mr_filter_p <- function(data, p_col, p_thresh = 1e-5) {
  if (!is.character(p_col) || length(p_col) != 1L) {
    stop("`p_col` must be a single column name.", call. = FALSE)
  }
  if (!is.numeric(p_thresh) || length(p_thresh) != 1L) {
    stop("`p_thresh` must be a single numeric value.", call. = FALSE)
  }

  .mednova_mr_check_cols(data, p_col, data_name = "data")
  data <- .mednova_mr_cast_numeric(data, p_col)
  data[data[[p_col]] < p_thresh, , drop = FALSE]
}

.mednova_mr_add_pheno <- function(data, pheno_name, pheno_col = "phenotype") {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (!is.character(pheno_name) || length(pheno_name) != 1L) {
    stop("`pheno_name` must be a single character value.", call. = FALSE)
  }
  if (!is.character(pheno_col) || length(pheno_col) != 1L) {
    stop("`pheno_col` must be a single column name.", call. = FALSE)
  }

  data[[pheno_col]] <- pheno_name
  data
}

.mednova_mr_select_keep <- function(data,
                                    keep_col = "mr_keep",
                                    keep_value = TRUE) {
  .mednova_mr_check_cols(data, keep_col, data_name = "data")
  data[data[[keep_col]] == keep_value, , drop = FALSE]
}

.mednova_mr_safe_name <- function(x, replace_with = "_") {
  if (!is.character(x)) {
    stop("`x` must be a character vector.", call. = FALSE)
  }
  if (!is.character(replace_with) || length(replace_with) != 1L) {
    stop("`replace_with` must be a single character string.", call. = FALSE)
  }

  gsub("[/\\\\:;\\s]+", replace_with, x)
}

.mednova_mr_format_exposure <- function(raw_data,
                                        phenotype_name,
                                        col_map = .mednova_mr_default_col_map(),
                                        pval_filter = NULL,
                                        pval_col = NULL) {
  if (!is.data.frame(raw_data)) {
    raw_data <- as.data.frame(raw_data)
  }
  if (!is.character(phenotype_name) || length(phenotype_name) != 1L) {
    stop("`phenotype_name` must be a single character value.", call. = FALSE)
  }
  if (!is.list(col_map)) {
    stop("`col_map` must be a list.", call. = FALSE)
  }

  required <- c(
    "snp", "beta", "se", "eaf", "effect_allele", "other_allele",
    "pval", "n", "chr", "pos"
  )
  miss_map <- setdiff(required, names(col_map))
  if (length(miss_map) > 0) {
    stop("`col_map` is missing keys: ", paste(miss_map, collapse = ", "), call. = FALSE)
  }
  .mednova_mr_check_cols(raw_data, unlist(col_map), data_name = "raw_data")

  if (!is.null(pval_filter)) {
    p_col_use <- if (is.null(pval_col)) col_map$pval else pval_col
    raw_data <- .mednova_mr_filter_p(raw_data, p_col = p_col_use, p_thresh = pval_filter)
  }

  raw_data <- .mednova_mr_add_pheno(
    raw_data,
    pheno_name = phenotype_name,
    pheno_col = "internal_pheno"
  )

  exposure_dat <- TwoSampleMR::format_data(
    raw_data,
    type = "exposure",
    snps = NULL,
    header = TRUE,
    phenotype_col = "internal_pheno",
    snp_col = col_map$snp,
    beta_col = col_map$beta,
    se_col = col_map$se,
    eaf_col = col_map$eaf,
    effect_allele_col = col_map$effect_allele,
    other_allele_col = col_map$other_allele,
    pval_col = col_map$pval,
    samplesize_col = col_map$n,
    chr_col = col_map$chr,
    pos_col = col_map$pos
  )

  .mednova_mr_select_keep(
    exposure_dat,
    keep_col = "mr_keep.exposure",
    keep_value = "TRUE"
  )
}

.mednova_mr_format_outcome <- function(raw_data,
                                       phenotype_name = "Outcome",
                                       col_map = .mednova_mr_default_col_map(),
                                       phenotype_col = "trait") {
  if (!is.data.frame(raw_data)) {
    raw_data <- as.data.frame(raw_data)
  }
  if (!is.character(phenotype_name) || length(phenotype_name) != 1L) {
    stop("`phenotype_name` must be a single character value.", call. = FALSE)
  }
  if (!is.list(col_map)) {
    stop("`col_map` must be a list.", call. = FALSE)
  }

  required <- c(
    "snp", "beta", "se", "eaf", "effect_allele", "other_allele",
    "pval", "n", "chr", "pos"
  )
  miss_map <- setdiff(required, names(col_map))
  if (length(miss_map) > 0) {
    stop("`col_map` is missing keys: ", paste(miss_map, collapse = ", "), call. = FALSE)
  }
  .mednova_mr_check_cols(raw_data, unlist(col_map), data_name = "raw_data")

  if (!phenotype_col %in% names(raw_data)) {
    raw_data[[phenotype_col]] <- phenotype_name
  }

  TwoSampleMR::format_data(
    raw_data,
    type = "outcome",
    snps = NULL,
    header = TRUE,
    phenotype_col = phenotype_col,
    snp_col = col_map$snp,
    beta_col = col_map$beta,
    se_col = col_map$se,
    eaf_col = col_map$eaf,
    effect_allele_col = col_map$effect_allele,
    other_allele_col = col_map$other_allele,
    pval_col = col_map$pval,
    samplesize_col = col_map$n,
    chr_col = col_map$chr,
    pos_col = col_map$pos
  )
}

.mednova_mr_calc_r2 <- function(exposure_dat,
                                beta_col = "beta.exposure",
                                se_col = "se.exposure",
                                n_col = "samplesize.exposure",
                                eaf_col = "eaf.exposure") {
  need <- c(beta_col, se_col, n_col, eaf_col)
  .mednova_mr_check_cols(exposure_dat, need, data_name = "exposure_dat")

  dat <- exposure_dat
  numerator <- 2 * (1 - dat[[eaf_col]]) * dat[[eaf_col]] * (dat[[beta_col]]^2)
  denominator <- numerator +
    (2 * (1 - dat[[eaf_col]]) * dat[[eaf_col]] * (dat[[se_col]]^2) * dat[[n_col]])
  dat$R2 <- ifelse(denominator == 0 | is.na(denominator), NA, numerator / denominator)
  dat
}

.mednova_mr_calc_f <- function(exposure_dat,
                               r2_col = "R2",
                               n_col = "samplesize.exposure") {
  need <- c(r2_col, n_col)
  .mednova_mr_check_cols(exposure_dat, need, data_name = "exposure_dat")

  dat <- exposure_dat
  dat$F_statistic <- (dat[[n_col]] - 2) * dat[[r2_col]] / (1 - dat[[r2_col]])
  dat
}

.mednova_mr_filter_f <- function(exposure_dat,
                                 f_col = "F_statistic",
                                 f_thresh = 10) {
  if (!is.numeric(f_thresh) || length(f_thresh) != 1L) {
    stop("`f_thresh` must be a single numeric value.", call. = FALSE)
  }

  .mednova_mr_check_cols(exposure_dat, f_col, data_name = "exposure_dat")
  exposure_dat[
    !is.na(exposure_dat[[f_col]]) & exposure_dat[[f_col]] > f_thresh,
    ,
    drop = FALSE
  ]
}

.mednova_mr_clump_local <- function(exposure_dat,
                                    kb = 10000,
                                    r2 = 0.001,
                                    plink_bin,
                                    bfile) {
  .mednova_mr_require("ieugwasr")
  need <- c("SNP", "pval.exposure", "id.exposure")
  .mednova_mr_check_cols(exposure_dat, need, data_name = "exposure_dat")

  if (is.null(plink_bin) || is.null(bfile)) {
    stop("`plink_bin` and `bfile` are required for local clumping.", call. = FALSE)
  }

  snp_df <- data.frame(
    rsid = exposure_dat$SNP,
    pval = exposure_dat$pval.exposure,
    id = exposure_dat$id.exposure
  )

  clumped_res <- tryCatch(
    ieugwasr::ld_clump(
      dat = snp_df,
      clump_kb = kb,
      clump_r2 = r2,
      plink_bin = plink_bin,
      bfile = bfile
    ),
    error = function(e) {
      message("Local PLINK clumping failed: ", e$message)
      NULL
    }
  )

  if (is.null(clumped_res)) {
    return(NULL)
  }

  exposure_dat[exposure_dat$SNP %in% clumped_res$rsid, , drop = FALSE]
}

.mednova_mr_clump_remote <- function(exposure_dat,
                                     kb = 10000,
                                     r2 = 0.001) {
  TwoSampleMR::clump_data(exposure_dat, clump_kb = kb, clump_r2 = r2)
}

.mednova_mr_clump_dispatch <- function(exposure_dat,
                                       perform = TRUE,
                                       kb = 10000,
                                       r2 = 0.001,
                                       pval_thresh = NULL,
                                       local = FALSE,
                                       plink_bin = NULL,
                                       bfile = NULL) {
  if (!isTRUE(perform)) {
    return(exposure_dat)
  }

  if (!is.null(pval_thresh)) {
    exposure_dat <- exposure_dat[exposure_dat$pval.exposure <= pval_thresh, , drop = FALSE]
  }

  if (nrow(exposure_dat) == 0) {
    return(exposure_dat)
  }

  if (isTRUE(local)) {
    return(.mednova_mr_clump_local(
      exposure_dat = exposure_dat,
      kb = kb,
      r2 = r2,
      plink_bin = plink_bin,
      bfile = bfile
    ))
  }

  .mednova_mr_clump_remote(exposure_dat, kb = kb, r2 = r2)
}

.mednova_mr_harmonize_one <- function(exposure_dat, outcome_dat) {
  if (is.null(exposure_dat) || nrow(exposure_dat) == 0) {
    return(NULL)
  }
  if (is.null(outcome_dat) || nrow(outcome_dat) == 0) {
    return(NULL)
  }

  res <- TwoSampleMR::harmonise_data(exposure_dat = exposure_dat, outcome_dat = outcome_dat)
  if ("mr_keep" %in% colnames(res)) {
    res <- .mednova_mr_select_keep(res, keep_col = "mr_keep", keep_value = TRUE)
    res <- res[!duplicated(res$SNP), , drop = FALSE]
  }
  res
}

.mednova_mr_harmonize_list <- function(exposure_list, outcome_dat, cores = 10) {
  if (!is.list(exposure_list)) {
    stop("`exposure_list` must be a list.", call. = FALSE)
  }
  if (is.null(outcome_dat) || nrow(outcome_dat) == 0) {
    stop("`outcome_dat` is empty.", call. = FALSE)
  }
  if (!is.numeric(cores) || length(cores) != 1L) {
    stop("`cores` must be a single numeric value.", call. = FALSE)
  }

  if (length(exposure_list) == 0) {
    return(list())
  }

  if (cores <= 1L || length(exposure_list) <= 1L) {
    harmonized_list <- lapply(
      exposure_list,
      function(exposure_dat) .mednova_mr_harmonize_one(exposure_dat, outcome_dat)
    )
    return(Filter(function(x) is.data.frame(x) && nrow(x) > 0, harmonized_list))
  }

  .mednova_mr_require(c("doParallel", "foreach"))
  cl <- parallel::makeCluster(cores)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  doParallel::registerDoParallel(cl)

  harmonized_list <- foreach::foreach(
    i = seq_along(exposure_list),
    .packages = c("TwoSampleMR")
  ) %dopar% {
    .mednova_mr_harmonize_one(exposure_list[[i]], outcome_dat)
  }

  Filter(function(x) is.data.frame(x) && nrow(x) > 0, harmonized_list)
}

.mednova_mr_harmonize_post <- function(harmonized_list) {
  if (!is.list(harmonized_list)) {
    stop("`harmonized_list` must be a list.", call. = FALSE)
  }

  harmonized_list <- Filter(function(x) is.data.frame(x) && nrow(x) > 0, harmonized_list)
  if (length(harmonized_list) == 0) {
    return(list())
  }

  harmonized_list <- lapply(harmonized_list, function(df) {
    if ("exposure" %in% colnames(df)) {
      df$exposure <- .mednova_mr_safe_name(df$exposure)
    }
    df
  })

  names(harmonized_list) <- vapply(
    harmonized_list,
    function(df) {
      if ("exposure" %in% colnames(df)) {
        unique(df$exposure)[1]
      } else {
        "exposure"
      }
    },
    character(1L)
  )

  harmonized_list
}

.mednova_mr_run_methods <- function(harmonized_dat,
                                    method_list = c(
                                      "mr_ivw",
                                      "mr_egger_regression",
                                      "mr_weighted_median",
                                      "mr_simple_mode",
                                      "mr_weighted_mode",
                                      "mr_wald_ratio"
                                    )) {
  if (is.null(harmonized_dat) || nrow(harmonized_dat) == 0) {
    return(data.frame())
  }

  TwoSampleMR::mr(harmonized_dat, method_list = method_list)
}

.mednova_mr_calc_or <- function(mr_res) {
  if (is.null(mr_res) || nrow(mr_res) == 0) {
    return(data.frame())
  }

  TwoSampleMR::generate_odds_ratios(mr_res)
}

.mednova_mr_heterogeneity <- function(harmonized_dat) {
  if (is.null(harmonized_dat) || nrow(harmonized_dat) < 2) {
    return(data.frame())
  }

  TwoSampleMR::mr_heterogeneity(harmonized_dat)
}

.mednova_mr_pleiotropy <- function(harmonized_dat) {
  if (is.null(harmonized_dat) || nrow(harmonized_dat) < 3) {
    return(data.frame())
  }

  TwoSampleMR::mr_pleiotropy_test(harmonized_dat)
}

.mednova_mr_steiger <- function(harmonized_dat) {
  if (is.null(harmonized_dat) || nrow(harmonized_dat) == 0) {
    return(data.frame())
  }
  if (!any(!is.na(harmonized_dat$samplesize.exposure))) {
    return(data.frame())
  }

  TwoSampleMR::directionality_test(harmonized_dat)
}

.mednova_mr_pivot_results <- function(mr_res) {
  if (is.null(mr_res) || nrow(mr_res) == 0) {
    return(data.frame())
  }

  res_clean <- mr_res[, !(names(mr_res) %in% c("lo_ci", "up_ci", "id.exposure", "id.outcome"))]
  tidyr::pivot_wider(
    res_clean,
    names_from = "method",
    names_vary = "slowest",
    values_from = c("b", "se", "pval", "or", "or_lci95", "or_uci95", "estimate")
  )
}

.mednova_mr_merge_sensitivity <- function(res_wide,
                                          hetero_res = data.frame(),
                                          pleio_res = data.frame(),
                                          steiger_res = data.frame()) {
  res_all <- res_wide

  if (nrow(hetero_res) > 0) {
    hetero_wide <- tidyr::pivot_wider(
      hetero_res,
      names_from = "method",
      names_vary = "slowest",
      values_from = c("Q", "Q_df", "Q_pval")
    )
    hetero_wide <- dplyr::select(
      hetero_wide,
      -dplyr::any_of(c("id.exposure", "id.outcome", "outcome", "exposure"))
    )
    res_all <- cbind(res_all, hetero_wide)
  }

  if (nrow(pleio_res) > 0) {
    pleio_wide <- dplyr::select(pleio_res, egger_intercept, se, pval)
    names(pleio_wide) <- c("egger_intercept", "egger_se", "egger_pval")
    res_all <- cbind(res_all, pleio_wide)
  }

  if (nrow(steiger_res) > 0) {
    steiger_wide <- dplyr::select(
      steiger_res,
      snp_r2.exposure,
      snp_r2.outcome,
      correct_causal_direction,
      steiger_pval
    )
    res_all <- cbind(res_all, steiger_wide)
  }

  res_all
}

.mednova_mr_run_single <- function(harmonized_dat,
                                   method_list = c(
                                     "mr_ivw",
                                     "mr_egger_regression",
                                     "mr_weighted_median",
                                     "mr_simple_mode",
                                     "mr_weighted_mode",
                                     "mr_wald_ratio"
                                   )) {
  if (is.null(harmonized_dat) || nrow(harmonized_dat) == 0) {
    return(list())
  }

  mr_res <- .mednova_mr_run_methods(harmonized_dat, method_list = method_list)
  if (nrow(mr_res) == 0) {
    return(list())
  }

  or_res <- .mednova_mr_calc_or(mr_res)
  res_wide <- .mednova_mr_pivot_results(or_res)

  hetero_res <- .mednova_mr_heterogeneity(harmonized_dat)
  pleio_res <- .mednova_mr_pleiotropy(harmonized_dat)
  steiger_res <- .mednova_mr_steiger(harmonized_dat)

  res_all <- .mednova_mr_merge_sensitivity(
    res_wide = res_wide,
    hetero_res = hetero_res,
    pleio_res = pleio_res,
    steiger_res = steiger_res
  )

  list(
    mr_res = mr_res,
    or_res = or_res,
    res_wide = res_wide,
    heterogeneity = hetero_res,
    pleiotropy = pleio_res,
    steiger = steiger_res,
    res_all = res_all
  )
}

.mednova_mr_write_harmonized <- function(harmonized_dat,
                                         out_file,
                                         overwrite = TRUE) {
  openxlsx::write.xlsx(
    harmonized_dat,
    file = out_file,
    rowNames = FALSE,
    overwrite = overwrite
  )
  out_file
}

.mednova_mr_write_results <- function(res_all,
                                      results_or,
                                      assumption,
                                      out_prefix,
                                      overwrite = TRUE) {
  res_xlsx <- paste0(out_prefix, "-res.xlsx")
  openxlsx::write.xlsx(
    list(main = results_or, Assumption = assumption),
    file = res_xlsx,
    overwrite = overwrite
  )

  res_csv <- paste0(out_prefix, ".csv")
  utils::write.csv(res_all, file = res_csv, row.names = FALSE)

  list(res_xlsx = res_xlsx, res_csv = res_csv)
}

.mednova_mr_run_batch <- function(harmonized_list,
                                  out_dir,
                                  cores = 10,
                                  method_list = c(
                                    "mr_ivw",
                                    "mr_egger_regression",
                                    "mr_weighted_median",
                                    "mr_simple_mode",
                                    "mr_weighted_mode",
                                    "mr_wald_ratio"
                                  )) {
  if (!is.list(harmonized_list)) {
    stop("`harmonized_list` must be a list.", call. = FALSE)
  }
  if (!is.character(out_dir) || length(out_dir) != 1L) {
    stop("`out_dir` must be a single directory path.", call. = FALSE)
  }

  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  original_wd <- getwd()
  setwd(out_dir)
  on.exit(setwd(original_wd), add = TRUE)

  .mednova_mr_batch_worker <- function(current_dat, exp_name, method_list) {
    if (is.null(exp_name) || exp_name == "") {
      exp_name <- "Exposure"
    }

    if (is.null(current_dat) || nrow(current_dat) == 0) {
      utils::write.csv("None of SNP", file = paste0(exp_name, "_None_of_SNP.csv"), row.names = FALSE)
      return(NULL)
    }

    res_list <- .mednova_mr_run_single(current_dat, method_list = method_list)
    if (length(res_list) == 0) {
      return(NULL)
    }

    nsnp <- nrow(current_dat)
    suffix <- ifelse(nsnp >= 3, "_sufficient_SNPs", ifelse(nsnp == 2, "_2_SNPs", "_1_SNP"))

    assumption <- subset(
      current_dat,
      mr_keep == TRUE,
      select = c("SNP", "pval.exposure", "pval.outcome", "mr_keep")
    )

    .mednova_mr_write_harmonized(current_dat, paste0(exp_name, "-dat.xlsx"))
    .mednova_mr_write_results(
      res_all = res_list$res_all,
      results_or = res_list$or_res,
      assumption = assumption,
      out_prefix = exp_name
    )

    utils::write.csv(res_list$res_all, file = paste0(exp_name, suffix, ".csv"), row.names = FALSE)
    exp_name
  }

  if (length(harmonized_list) == 0) {
    return(character())
  }

  if (cores <= 1L || length(harmonized_list) <= 1L) {
    completed <- vapply(
      seq_along(harmonized_list),
      function(i) {
        exp_name <- names(harmonized_list)[i]
        res <- .mednova_mr_batch_worker(harmonized_list[[i]], exp_name, method_list)
        if (is.null(res)) "" else res
      },
      character(1L)
    )
    return(completed[nzchar(completed)])
  }

  .mednova_mr_require(c("doParallel", "foreach"))
  cl <- parallel::makeCluster(cores)
  on.exit(parallel::stopCluster(cl), add = TRUE)
  doParallel::registerDoParallel(cl)

  completed <- foreach::foreach(
    i = seq_along(harmonized_list),
    .packages = c("TwoSampleMR", "dplyr", "tidyr", "openxlsx")
  ) %dopar% {
    current_dat <- harmonized_list[[i]]
    exp_name <- names(harmonized_list)[i]
    .mednova_mr_batch_worker(current_dat, exp_name, method_list)
  }

  unlist(completed)
}
