# Advanced Volcano Plot

Create a volcano plot with significance coloring, optional size mapping,
and optional gene labels.

## Usage

``` r
plot_volcano_advanced(
  data,
  logfc_col = NULL,
  p_col = NULL,
  gene_col = NULL,
  size_col = "logCPM",
  p_cutoff = 0.05,
  lfc_cutoff = 0.25,
  label_genes = NULL,
  label_p_cutoff = NULL,
  label_top = 5,
  colors = c(Up = "#fe0000", Down = "#13fc00", NoSignifi = "#bdbdbd"),
  size_range = c(2, 16),
  save_path = NULL,
  save_width = 5,
  save_height = 4
)
```

## Arguments

- data:

  Data frame containing differential-expression results.

- logfc_col:

  Column name for log2 fold-change.

- p_col:

  Column name for the P-value.

- gene_col:

  Column name for the gene label.

- size_col:

  Optional column name used for point sizes.

- p_cutoff:

  P-value cutoff for significance.

- lfc_cutoff:

  Absolute log2 fold-change cutoff.

- label_genes:

  Optional character vector of genes to label.

- label_p_cutoff:

  Optional P-value cutoff for labels.

- label_top:

  Optional number of top genes to label by P-value.

- colors:

  Named vector for `Up`, `Down`, and `NoSignifi`.

- size_range:

  Size range when `size_col` is used.

- save_path:

  Optional path passed to
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).

- save_width:

  Saved plot width.

- save_height:

  Saved plot height.

## Value

A list with `plot` and `data`.
