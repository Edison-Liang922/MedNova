#' Build a Diagnostic Model With Nomogram and DCA
#'
#' Build a multigene logistic model, generate a nomogram PDF, and save a DCA
#' plot in the current working directory.
#'
#' @param expr Expression matrix with genes in rows and samples in columns.
#' @param group_df Sample metadata with sample IDs and group labels.
#' @param target_genes Character vector of target genes.
#' @param project_name Project name used for output files.
#' @param sample_col Optional sample ID column in `group_df`.
#' @param group_col Group label column in `group_df`.
#' @param case_label Positive class label.
#'
#' @return A list with `model`, `dca_data`, and `plot`.
#' @export
Bio_build_diagnostic_model <- function(expr,
                                       group_df,
                                       target_genes,
                                       project_name = "Combined_Model",
                                       sample_col = NULL,
                                       group_col = "group",
                                       case_label = "Case") {
  if (!requireNamespace("rms", quietly = TRUE)) {
    stop("Package 'rms' is required.", call. = FALSE)
  }
  if (!requireNamespace("dcurves", quietly = TRUE)) {
    stop("Package 'dcurves' is required.", call. = FALSE)
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.", call. = FALSE)
  }
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required.", call. = FALSE)
  }

  group_df <- as.data.frame(group_df)
  if (!group_col %in% names(group_df)) {
    stop("`group_df` is missing column: ", group_col, call. = FALSE)
  }
  if (!is.null(sample_col)) {
    if (!sample_col %in% names(group_df)) {
      stop("`group_df` is missing column: ", sample_col, call. = FALSE)
    }
    group_df$Sample <- as.character(group_df[[sample_col]])
  } else {
    if (is.null(rownames(group_df))) {
      stop("`group_df` must have rownames or specify `sample_col`.", call. = FALSE)
    }
    group_df$Sample <- rownames(group_df)
  }
  group_df$group <- as.character(group_df[[group_col]])

  target_genes <- intersect(target_genes, rownames(expr))
  if (length(target_genes) == 0) {
    stop("No target genes were matched in `expr`.", call. = FALSE)
  }

  dat <- as.data.frame(t(expr[target_genes, , drop = FALSE])) |>
    dplyr::mutate(Sample = rownames(.)) |>
    dplyr::inner_join(group_df[, c("Sample", "group")], by = "Sample") |>
    dplyr::mutate(Status = ifelse(group == case_label, 1, 0))

  dd <- rms::datadist(dat)
  old_opt <- options(datadist = "dd")
  on.exit(options(old_opt), add = TRUE)

  formula_str <- stats::as.formula(
    paste("Status ~", paste0("`", target_genes, "`", collapse = "+"))
  )
  fit <- rms::lrm(formula_str, data = dat, x = TRUE, y = TRUE)

  grDevices::pdf(paste0(project_name, "_Nomogram.pdf"), width = 10, height = 8)
  prop_fun <- function(x) 1 / (1 + exp(-x))
  nom <- rms::nomogram(
    fit,
    fun = prop_fun,
    fun.at = c(0.1, 0.3, 0.5, 0.7, 0.9),
    funlabel = "Diagnostic Probability"
  )
  graphics::plot(nom)
  grDevices::dev.off()

  lp <- stats::predict(fit, type = "lp")
  dat$Model_Prob <- 1 / (1 + exp(-lp))

  dca_res <- dcurves::dca(
    Status ~ Model_Prob,
    data = dat,
    thresholds = seq(0, 0.99, by = 0.01)
  )

  dca_p <- plot(dca_res) +
    ggplot2::theme_classic() +
    ggplot2::labs(
      title = "Decision Curve Analysis",
      x = "Threshold Probability",
      y = "Net Benefit"
    )

  ggplot2::ggsave(paste0(project_name, "_DCA.pdf"), dca_p, width = 6, height = 5)

  message("Diagnostic model built successfully. Project: ", project_name)
  list(model = fit, dca_data = dca_res, plot = dca_p)
}

#' Unified MedNova Entry for Diagnostic Modeling
#'
#' @param expr Expression matrix with genes in rows and samples in columns.
#' @param group_df Sample metadata with sample IDs and group labels.
#' @param target_genes Character vector of target genes.
#' @param project_name Project name used for output files.
#' @param sample_col Optional sample ID column in `group_df`.
#' @param group_col Group label column in `group_df`.
#' @param case_label Positive class label.
#' @param include_roc Logical; whether to also run `plot_diagnostic_roc()`.
#'
#' @return A list with `model` and optional `roc`.
#' @export
med_diagnostic_model <- function(expr,
                                 group_df,
                                 target_genes,
                                 project_name = "Combined_Model",
                                 sample_col = NULL,
                                 group_col = "group",
                                 case_label = "Case",
                                 include_roc = TRUE) {
  model_res <- Bio_build_diagnostic_model(
    expr = expr,
    group_df = group_df,
    target_genes = target_genes,
    project_name = project_name,
    sample_col = sample_col,
    group_col = group_col,
    case_label = case_label
  )

  roc_res <- NULL
  if (isTRUE(include_roc)) {
    roc_res <- plot_diagnostic_roc(
      expr = expr,
      group_df = group_df,
      target_genes = target_genes,
      project_name = paste0(project_name, "_ROC"),
      sample_col = sample_col,
      group_col = group_col,
      case_label = case_label
    )
  }

  list(model = model_res, roc = roc_res)
}
