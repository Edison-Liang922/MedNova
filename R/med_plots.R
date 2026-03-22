#' Love Plot for Absolute Standardized Mean Differences
#'
#' Create a love plot comparing absolute standardized mean differences before
#' and after matching or weighting.
#'
#' @param data Data frame containing covariate and SMD columns.
#' @param covariate_col Column name for covariates.
#' @param before_col Column name for the pre-adjustment absolute SMD.
#' @param after_col Column name for the post-adjustment absolute SMD.
#' @param threshold Vertical threshold line.
#' @param colors Named vector with entries `Before` and `After`.
#' @param connect_lines Logical; draw segments between before and after points.
#' @param x_breaks Numeric vector of x-axis breaks.
#' @param x_limit Optional x-axis limits.
#' @param base_size Base font size.
#'
#' @return A list with `plot` and `data`.
#' @export
plot_love <- function(data,
                      covariate_col = "Covariate",
                      before_col = "AbsSMD_Before_max",
                      after_col = "AbsSMD_After_max",
                      threshold = 0.10,
                      colors = c(Before = "#D62728", After = "#1F77B4"),
                      connect_lines = TRUE,
                      x_breaks = seq(0, 0.30, by = 0.05),
                      x_limit = NULL,
                      base_size = 12) {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("Package 'dplyr' is required.", call. = FALSE)
  }
  if (!requireNamespace("tidyr", quietly = TRUE)) {
    stop("Package 'tidyr' is required.", call. = FALSE)
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.", call. = FALSE)
  }
  if (!requireNamespace("forcats", quietly = TRUE)) {
    stop("Package 'forcats' is required.", call. = FALSE)
  }
  if (!requireNamespace("rlang", quietly = TRUE)) {
    stop("Package 'rlang' is required.", call. = FALSE)
  }

  df <- data |>
    dplyr::select(dplyr::all_of(c(covariate_col, before_col, after_col))) |>
    dplyr::rename(
      Covariate = !!rlang::sym(covariate_col),
      Before = !!rlang::sym(before_col),
      After = !!rlang::sym(after_col)
    ) |>
    dplyr::mutate(
      Before = abs(as.numeric(Before)),
      After = abs(as.numeric(After))
    ) |>
    dplyr::mutate(Covariate = forcats::fct_reorder(Covariate, Before, .desc = TRUE))

  plot_df <- df |>
    tidyr::pivot_longer(
      cols = c(Before, After),
      names_to = "Time",
      values_to = "AbsSMD"
    ) |>
    dplyr::mutate(Time = factor(Time, levels = c("Before", "After")))

  if (is.null(x_limit)) {
    max_val <- max(df$Before, df$After, na.rm = TRUE)
    x_limit <- c(0, max_val + 0.03)
  }

  p <- ggplot2::ggplot() +
    ggplot2::geom_vline(
      xintercept = threshold,
      linetype = "dashed",
      linewidth = 0.7
    )

  if (isTRUE(connect_lines)) {
    p <- p +
      ggplot2::geom_segment(
        data = df,
        ggplot2::aes(x = After, xend = Before, y = Covariate, yend = Covariate),
        linewidth = 0.45,
        color = "grey80"
      )
  }

  p <- p +
    ggplot2::geom_point(
      data = plot_df,
      ggplot2::aes(x = AbsSMD, y = Covariate, color = Time),
      size = 3.0
    ) +
    ggplot2::scale_color_manual(values = colors) +
    ggplot2::scale_x_continuous(
      limits = x_limit,
      breaks = x_breaks,
      expand = ggplot2::expansion(mult = c(0.01, 0.02))
    ) +
    ggplot2::labs(
      x = "Absolute standardized mean difference (|SMD|)",
      y = NULL
    ) +
    ggplot2::theme_classic(base_size = base_size) +
    ggplot2::theme(
      legend.title = ggplot2::element_blank(),
      legend.position = "top",
      legend.text = ggplot2::element_text(size = 10),
      axis.text.y = ggplot2::element_text(size = 10),
      axis.text.x = ggplot2::element_text(size = 10),
      axis.title.x = ggplot2::element_text(size = 11),
      axis.line = ggplot2::element_line(linewidth = 0.8),
      axis.ticks = ggplot2::element_line(linewidth = 0.8),
      plot.margin = ggplot2::margin(10, 14, 10, 10)
    )

  list(plot = p, data = list(summary = df, long = plot_df))
}

#' Forest Plot for Logistic Regression Results
#'
#' Create a forest plot from logistic regression OR estimates and confidence
#' intervals.
#'
#' @param result_df Data frame with columns `variable`, `level`, `OR`, and
#'   `OR_95CI`.
#' @param title Plot title.
#' @param color Point and interval color.
#' @param log_scale Logical; use a log10 x-axis.
#' @param breaks Numeric axis breaks when `log_scale = TRUE`.
#'
#' @return A ggplot object.
#' @export
plot_logistic_forest <- function(result_df,
                                 title = "Logistic Regression Forest Plot",
                                 color = "blue",
                                 log_scale = TRUE,
                                 breaks = c(0.5, 1, 2, 4, 8)) {
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.", call. = FALSE)
  }

  required_cols <- c("variable", "level", "OR", "OR_95CI")
  missing_cols <- setdiff(required_cols, names(result_df))
  if (length(missing_cols) > 0) {
    stop("`result_df` is missing columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  extract_ci <- function(ci_str) {
    parts <- strsplit(ci_str, "-")[[1]]
    as.numeric(parts)
  }

  df <- result_df
  df$var_level <- paste(df$variable, df$level, sep = ": ")
  df$OR_value <- df$OR
  ci_mat <- t(sapply(df$OR_95CI, extract_ci))
  df$lower <- ci_mat[, 1]
  df$upper <- ci_mat[, 2]

  p <- ggplot2::ggplot(df, ggplot2::aes(x = OR_value, y = stats::reorder(var_level, OR_value))) +
    ggplot2::geom_point(shape = 15, size = 3, color = color) +
    ggplot2::geom_errorbarh(
      ggplot2::aes(xmin = lower, xmax = upper),
      height = 0.2,
      color = color
    ) +
    ggplot2::geom_vline(xintercept = 1, linetype = "dashed", color = "red") +
    ggplot2::labs(title = title, x = "OR", y = "Variable") +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text.y = ggplot2::element_text(size = 10),
      axis.text.x = ggplot2::element_text(size = 10),
      panel.grid.major = ggplot2::element_line(color = "grey80"),
      panel.grid.minor = ggplot2::element_blank()
    )

  if (isTRUE(log_scale)) {
    p <- p + ggplot2::scale_x_continuous(trans = "log10", breaks = breaks)
  }

  p
}

#' Diagnostic ROC Analysis
#'
#' Plot single-gene ROC curves and save a combined ROC figure for a target gene
#' set.
#'
#' @param expr Expression matrix with genes in rows and samples in columns.
#' @param group_df Sample metadata with sample IDs and group labels.
#' @param target_genes Character vector of target genes.
#' @param project_name Project name for output files.
#' @param sample_col Optional sample ID column in `group_df`.
#' @param group_col Group label column in `group_df`.
#' @param case_label Positive class label.
#'
#' @return A named list of ROC objects.
#' @export
plot_diagnostic_roc <- function(expr,
                                group_df,
                                target_genes,
                                project_name = "ROC_Analysis",
                                sample_col = NULL,
                                group_col = "group",
                                case_label = "Case") {
  if (!requireNamespace("pROC", quietly = TRUE)) {
    stop("Package 'pROC' is required.", call. = FALSE)
  }
  if (!requireNamespace("RColorBrewer", quietly = TRUE)) {
    stop("Package 'RColorBrewer' is required.", call. = FALSE)
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

  matched_genes <- intersect(target_genes, rownames(expr))
  if (length(matched_genes) == 0) {
    stop("No target genes were matched in `expr`.", call. = FALSE)
  }

  out_dir <- paste0(project_name, "_ROC_Plots")
  if (!dir.exists(out_dir)) {
    dir.create(out_dir)
  }

  dat <- as.data.frame(t(expr[matched_genes, , drop = FALSE]))
  dat$Sample <- rownames(dat)
  dat <- dplyr::inner_join(dat, group_df[, c("Sample", "group")], by = "Sample")
  dat$group <- as.factor(dat$group)

  n_genes <- length(matched_genes)
  colors <- if (n_genes >= 3) {
    RColorBrewer::brewer.pal(min(n_genes, 8), "Set1")
  } else {
    c("#E41A1C", "#377EB8")[seq_len(n_genes)]
  }

  roc_list <- list()
  for (i in seq_along(matched_genes)) {
    gene <- matched_genes[i]
    fit <- stats::glm(
      stats::as.formula(paste("group ~", paste0("`", gene, "`"))),
      data = dat,
      family = stats::binomial
    )
    prob <- stats::predict(fit, type = "response")
    roc_obj <- pROC::roc(dat$group, prob, quiet = TRUE, levels = rev(levels(dat$group)))
    roc_list[[gene]] <- roc_obj

    grDevices::pdf(file.path(out_dir, paste0("Single_ROC_", gene, ".pdf")), width = 6, height = 6)
    graphics::par(pty = "s")
    graphics::plot(
      roc_obj,
      col = colors[i],
      lwd = 3,
      legacy.axes = TRUE,
      main = paste("ROC for", gene)
    )
    graphics::legend(
      "bottomright",
      legend = paste0(gene, "\nAUC: ", sprintf("%.3f", pROC::auc(roc_obj))),
      bty = "n",
      cex = 1.1,
      text.col = colors[i],
      adj = c(0, 0.5)
    )
    grDevices::dev.off()
  }

  grDevices::pdf(file.path(out_dir, paste0("Combined_ROC_", project_name, ".pdf")), width = 6, height = 6)
  graphics::par(pty = "s")
  for (i in seq_along(matched_genes)) {
    gene <- matched_genes[i]
    graphics::plot(
      roc_list[[gene]],
      col = colors[i],
      lwd = 2,
      add = (i != 1),
      legacy.axes = TRUE,
      main = "Combined ROC Curves"
    )
  }
  legend_labels <- vapply(
    matched_genes,
    function(gene) paste0(gene, " (AUC: ", sprintf("%.3f", pROC::auc(roc_list[[gene]])), ")"),
    character(1L)
  )
  graphics::legend(
    "bottomright",
    legend = legend_labels,
    col = colors,
    lwd = 2,
    bty = "n",
    cex = 0.9
  )
  grDevices::dev.off()

  message("ROC plots saved to: ", out_dir)
  roc_list
}

#' Advanced Volcano Plot
#'
#' Create a volcano plot with significance coloring, optional size mapping, and
#' optional gene labels.
#'
#' @param data Data frame containing differential-expression results.
#' @param logfc_col Column name for log2 fold-change.
#' @param p_col Column name for the P-value.
#' @param gene_col Column name for the gene label.
#' @param size_col Optional column name used for point sizes.
#' @param p_cutoff P-value cutoff for significance.
#' @param lfc_cutoff Absolute log2 fold-change cutoff.
#' @param label_genes Optional character vector of genes to label.
#' @param label_p_cutoff Optional P-value cutoff for labels.
#' @param label_top Optional number of top genes to label by P-value.
#' @param colors Named vector for `Up`, `Down`, and `NoSignifi`.
#' @param size_range Size range when `size_col` is used.
#' @param save_path Optional path passed to `ggplot2::ggsave()`.
#' @param save_width Saved plot width.
#' @param save_height Saved plot height.
#'
#' @return A list with `plot` and `data`.
#' @export
plot_volcano_advanced <- function(data,
                                  logfc_col = NULL,
                                  p_col = NULL,
                                  gene_col = NULL,
                                  size_col = "logCPM",
                                  p_cutoff = 0.05,
                                  lfc_cutoff = 0.25,
                                  label_genes = NULL,
                                  label_p_cutoff = NULL,
                                  label_top = 5,
                                  colors = c(
                                    Up = "#fe0000",
                                    Down = "#13fc00",
                                    NoSignifi = "#bdbdbd"
                                  ),
                                  size_range = c(2, 16),
                                  save_path = NULL,
                                  save_width = 5,
                                  save_height = 4) {
  if (!is.data.frame(data)) {
    stop("`data` must be a data.frame.", call. = FALSE)
  }
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Package 'ggplot2' is required.", call. = FALSE)
  }
  if (!requireNamespace("ggrepel", quietly = TRUE)) {
    stop("Package 'ggrepel' is required.", call. = FALSE)
  }

  pick_col <- function(primary, candidates) {
    if (!is.null(primary) && primary %in% names(data)) {
      return(primary)
    }
    for (nm in candidates) {
      if (nm %in% names(data)) {
        return(nm)
      }
    }
    NULL
  }

  logfc_col <- pick_col(logfc_col, c("logFC", "fd", "log2FC"))
  p_col <- pick_col(p_col, c("P_value", "pvalue", "P.Value", "p_val"))
  gene_col <- pick_col(gene_col, c("gene", "Gene", "SYMBOL", "symbol"))

  if (is.null(logfc_col) || is.null(p_col)) {
    stop("Cannot find logFC or p-value columns. Please set `logfc_col` and `p_col`.", call. = FALSE)
  }
  if (is.null(gene_col)) {
    gene_values <- rownames(data)
    if (is.null(gene_values)) {
      data$gene <- paste0("Gene", seq_len(nrow(data)))
      gene_col <- "gene"
    } else {
      data$gene <- gene_values
      gene_col <- "gene"
    }
  }

  df <- data
  df$logP <- -log10(df[[p_col]])
  df$threshold <- ifelse(
    df[[p_col]] < p_cutoff & abs(df[[logfc_col]]) >= lfc_cutoff,
    ifelse(df[[logfc_col]] >= lfc_cutoff, "Up", "Down"),
    "NoSignifi"
  )
  df$threshold <- factor(df$threshold, levels = c("Up", "Down", "NoSignifi"))

  label_df <- NULL
  if (!is.null(label_genes)) {
    label_df <- df[df[[gene_col]] %in% label_genes, , drop = FALSE]
  } else if (!is.null(label_p_cutoff)) {
    label_df <- df[df[[p_col]] <= label_p_cutoff, , drop = FALSE]
  } else if (!is.null(label_top) && label_top > 0) {
    ord <- order(df[[p_col]], decreasing = FALSE)
    label_df <- df[ord[seq_len(min(label_top, nrow(df)))], , drop = FALSE]
  }

  p <- ggplot2::ggplot(
    df,
    ggplot2::aes(
      x = rlang::.data[[logfc_col]],
      y = rlang::.data$logP,
      fill = rlang::.data$threshold
    )
  ) +
    ggplot2::geom_point(
      ggplot2::aes(
        size = if (!is.null(size_col) && size_col %in% names(df)) {
          rlang::.data[[size_col]]
        } else {
          NULL
        }
      ),
      colour = "black",
      shape = 21,
      stroke = 0.5
    ) +
    ggplot2::scale_fill_manual(values = colors) +
    ggplot2::geom_vline(
      xintercept = c(-lfc_cutoff, lfc_cutoff),
      linetype = 2,
      colour = "black",
      linewidth = 0.5
    ) +
    ggplot2::geom_hline(
      yintercept = -log10(p_cutoff),
      linetype = 2,
      colour = "black",
      linewidth = 0.5
    ) +
    ggplot2::xlab("log2 (FoldChange)") +
    ggplot2::ylab("-log10 (Pvalue)") +
    ggplot2::theme_classic(base_line_size = 1) +
    ggplot2::guides(fill = ggplot2::guide_legend(override.aes = list(size = 5))) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(size = 10, color = "black", face = "bold"),
      axis.title.y = ggplot2::element_text(
        size = 10,
        color = "black",
        face = "bold",
        vjust = 1.9,
        hjust = 0.5
      ),
      legend.text = ggplot2::element_text(color = "black", size = 7, face = "bold")
    )

  if (!is.null(size_col) && size_col %in% names(df)) {
    p <- p + ggplot2::scale_size(limits = size_range)
  }

  if (!is.null(label_df) && nrow(label_df) > 0) {
    p <- p + ggrepel::geom_text_repel(
      data = label_df,
      ggplot2::aes(label = rlang::.data[[gene_col]]),
      size = 4.5,
      color = "black",
      segment.color = "black",
      show.legend = FALSE
    )
  }

  if (!is.null(save_path)) {
    ggplot2::ggsave(save_path, plot = p, width = save_width, height = save_height)
  }

  list(plot = p, data = df)
}
