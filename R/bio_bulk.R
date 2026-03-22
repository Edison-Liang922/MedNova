#' Align Bulk Expression and Group Metadata
#'
#' @param expr Expression matrix/data frame with sample IDs in row names.
#' @param group Sample metadata data frame.
#' @param sample_col Column name in `group` containing sample IDs.
#' @param group_col Column name in `group` containing group labels.
#' @param dataset_col Column name to use for renamed sample IDs.
#' @param id_prefix Prefix for new sample IDs.
#' @param recode_map Named character vector for recoding group labels.
#' @param keep_group_cols Optional character vector of columns to keep in
#'   `group`.
#' @param reorder Logical; whether to reorder and subset to common samples.
#'
#' @return A list with `expr`, `group`, and `id_map`.
#' @export
Bio_Bulk_align_group <- function(expr,
                                 group,
                                 sample_col = "GSM",
                                 group_col = "group",
                                 dataset_col = "Dataset",
                                 id_prefix = "Data",
                                 recode_map = c(normal = "Control", cirrhosis = "Case"),
                                 keep_group_cols = NULL,
                                 reorder = TRUE) {
  expr_df <- as.data.frame(expr)
  if (is.null(rownames(expr_df))) {
    stop("`expr` must have row names for sample IDs.", call. = FALSE)
  }

  original_ids <- rownames(expr_df)
  new_id_labels <- paste0(id_prefix, seq_along(original_ids))
  id_map <- stats::setNames(new_id_labels, original_ids)
  rownames(expr_df) <- unname(id_map[rownames(expr_df)])

  group_df <- as.data.frame(group)
  if (!sample_col %in% names(group_df)) {
    if (is.null(rownames(group_df))) {
      stop("`group` must have row names or include `sample_col`.", call. = FALSE)
    }
    group_df[[dataset_col]] <- rownames(group_df)
  } else {
    group_df[[dataset_col]] <- as.character(group_df[[sample_col]])
  }

  group_df[[dataset_col]] <- unname(id_map[group_df[[dataset_col]]])
  if (any(is.na(group_df[[dataset_col]]))) {
    warning("Some sample IDs in `group` were not found in `expr`.", call. = FALSE)
  }

  if (!group_col %in% names(group_df)) {
    stop("`group` is missing column: ", group_col, call. = FALSE)
  }
  recode_key <- names(recode_map)
  group_df[[group_col]] <- ifelse(
    group_df[[group_col]] %in% recode_key,
    unname(recode_map[group_df[[group_col]]]),
    group_df[[group_col]]
  )

  rownames(group_df) <- group_df[[dataset_col]]

  if (isTRUE(reorder)) {
    common_ids <- intersect(rownames(expr_df), rownames(group_df))
    if (!length(common_ids)) {
      stop("No overlapping samples between `expr` and `group` after mapping.", call. = FALSE)
    }
    if (!identical(rownames(expr_df), rownames(group_df))) {
      expr_df <- expr_df[common_ids, , drop = FALSE]
      group_df <- group_df[common_ids, , drop = FALSE]
    }
  }

  if (!is.null(keep_group_cols)) {
    keep_group_cols <- intersect(keep_group_cols, colnames(group_df))
    group_df <- group_df[, keep_group_cols, drop = FALSE]
  }

  list(expr = expr_df, group = group_df, id_map = id_map)
}

#' Filter Low-Expression Genes
#'
#' @param expr A numeric matrix/data frame with genes in rows and samples in
#'   columns.
#' @param threshold Minimum mean/median expression to keep a gene.
#' @param method Statistic for filtering: `"mean"` or `"median"`.
#' @param logical_transpose Logical; whether to transpose output.
#'
#' @return A data frame of filtered expression data.
#' @export
Bio_Bulk_filter_low_expression <- function(expr,
                                           threshold = 1,
                                           method = c("mean", "median"),
                                           logical_transpose = TRUE) {
  method <- match.arg(method)
  expr_mat <- as.matrix(expr)

  if (method == "mean") {
    gene_stat <- rowMeans(expr_mat, na.rm = TRUE)
  } else {
    gene_stat <- apply(expr_mat, 1, stats::median, na.rm = TRUE)
  }

  keep_genes <- gene_stat >= threshold
  filtered_counts <- expr_mat[keep_genes, , drop = FALSE]

  message(paste0("原始基因数: ", nrow(expr_mat)))
  message(paste0("保留基因数: ", nrow(filtered_counts)))
  message(paste0("过滤掉的基因数: ", sum(!keep_genes)))

  if (isTRUE(logical_transpose)) {
    return(as.data.frame(t(filtered_counts)))
  }

  as.data.frame(filtered_counts)
}

#' Limma Differential Expression for Bulk RNA-seq
#'
#' @param counts Expression matrix or data frame.
#' @param group Sample metadata with sample IDs and a grouping column.
#' @param group_col Column name in `group` for case/control labels.
#' @param sample_col Optional column name in `group` for sample IDs.
#' @param case_name Case label in `group_col`.
#' @param control_name Control label in `group_col`.
#' @param logFC_threshold Log2 fold-change threshold for calling DE genes.
#' @param adj_P_threshold Adjusted P-value threshold for calling DE genes.
#'
#' @return A list with `results`, `deg_list`, and `contrast`.
#' @export
Bio_Bulk_limma_analysis <- function(counts,
                                    group,
                                    group_col = "group",
                                    sample_col = NULL,
                                    case_name = "Case",
                                    control_name = "Control",
                                    logFC_threshold = 0.5,
                                    adj_P_threshold = 0.05) {
  if (!requireNamespace("limma", quietly = TRUE)) {
    stop("Package 'limma' is required.", call. = FALSE)
  }

  counts_mat <- as.matrix(counts)
  if (is.null(rownames(counts_mat)) || is.null(colnames(counts_mat))) {
    stop("`counts` must have both rownames and colnames.", call. = FALSE)
  }

  group_df <- as.data.frame(group)
  if (!group_col %in% names(group_df)) {
    stop("`group` is missing column: ", group_col, call. = FALSE)
  }

  if (!is.null(sample_col)) {
    if (!sample_col %in% names(group_df)) {
      stop("`group` is missing column: ", sample_col, call. = FALSE)
    }
    rownames(group_df) <- as.character(group_df[[sample_col]])
  } else if (is.null(rownames(group_df))) {
    stop("`group` must have rownames or specify `sample_col`.", call. = FALSE)
  }

  sample_ids <- rownames(group_df)
  if (all(sample_ids %in% colnames(counts_mat))) {
    counts_use <- counts_mat[, sample_ids, drop = FALSE]
  } else if (all(sample_ids %in% rownames(counts_mat))) {
    counts_use <- t(counts_mat[sample_ids, , drop = FALSE])
  } else {
    stop("Sample IDs in `group` do not match rownames or colnames of `counts`.", call. = FALSE)
  }

  group_df <- group_df[colnames(counts_use), , drop = FALSE]
  group_factor <- factor(group_df[[group_col]], levels = c(control_name, case_name))
  if (any(is.na(group_factor))) {
    stop("`group_col` must contain only control/case labels.", call. = FALSE)
  }

  design <- stats::model.matrix(~0 + group_factor)
  colnames(design) <- levels(group_factor)
  rownames(design) <- colnames(counts_use)

  fit <- limma::lmFit(counts_use, design)
  contrast_str <- paste0(case_name, "-", control_name)
  contrast_matrix <- limma::makeContrasts(contrasts = contrast_str, levels = design)
  fit2 <- limma::contrasts.fit(fit, contrast_matrix)
  fit2 <- limma::eBayes(fit2)

  deg_limma <- limma::topTable(fit2, coef = 1, n = Inf, sort.by = "logFC")
  deg_limma$gene <- rownames(deg_limma)
  deg_limma$change <- ifelse(
    deg_limma$adj.P.Val > adj_P_threshold,
    "stable",
    ifelse(
      deg_limma$logFC > logFC_threshold,
      "up",
      ifelse(deg_limma$logFC < -logFC_threshold, "down", "stable")
    )
  )
  deg_limma$log.adj.p <- -log10(deg_limma$adj.P.Val)

  deg_gene <- deg_limma$gene[deg_limma$change %in% c("up", "down")]
  message("差异分析统计结果（", contrast_str, "）：")
  print(table(deg_limma$change))

  list(
    results = deg_limma,
    deg_list = deg_gene,
    contrast = contrast_str
  )
}

#' Unified MedNova Entry for Bulk Differential Expression
#'
#' @param counts Expression matrix or data frame.
#' @param group Sample metadata.
#' @param group_col Grouping column.
#' @param sample_col Optional sample ID column.
#' @param align Logical; whether to call `Bio_Bulk_align_group()` first.
#' @param align_args Optional list of extra arguments for
#'   `Bio_Bulk_align_group()`.
#' @param filter_low_expression Logical; whether to call
#'   `Bio_Bulk_filter_low_expression()` before limma.
#' @param filter_args Optional list of extra arguments for
#'   `Bio_Bulk_filter_low_expression()`.
#' @param ... Additional arguments passed to `Bio_Bulk_limma_analysis()`.
#'
#' @return A list with optional `aligned`, optional `filtered_counts`, and `deg`.
#' @export
med_bulk_deg <- function(counts,
                         group,
                         group_col = "group",
                         sample_col = NULL,
                         align = FALSE,
                         align_args = list(),
                         filter_low_expression = FALSE,
                         filter_args = list(),
                         ...) {
  counts_use <- counts
  group_use <- group
  aligned <- NULL
  filtered_counts <- NULL

  if (isTRUE(align)) {
    aligned <- do.call(
      Bio_Bulk_align_group,
      utils::modifyList(
        list(
          expr = counts_use,
          group = group_use,
          sample_col = sample_col %||% "GSM",
          group_col = group_col
        ),
        align_args
      )
    )
    counts_use <- aligned$expr
    group_use <- aligned$group
    sample_col <- NULL
  }

  if (isTRUE(filter_low_expression)) {
    filter_defaults <- list(
      expr = counts_use,
      logical_transpose = FALSE
    )
    filtered_counts <- do.call(
      Bio_Bulk_filter_low_expression,
      utils::modifyList(filter_defaults, filter_args)
    )
    counts_use <- filtered_counts
  }

  deg <- Bio_Bulk_limma_analysis(
    counts = counts_use,
    group = group_use,
    group_col = group_col,
    sample_col = sample_col,
    ...
  )

  list(
    aligned = aligned,
    filtered_counts = filtered_counts,
    deg = deg
  )
}
