# Diagnostic ROC Analysis

Plot single-gene ROC curves and save a combined ROC figure for a target
gene set.

## Usage

``` r
plot_diagnostic_roc(
  expr,
  group_df,
  target_genes,
  project_name = "ROC_Analysis",
  sample_col = NULL,
  group_col = "group",
  case_label = "Case"
)
```

## Arguments

- expr:

  Expression matrix with genes in rows and samples in columns.

- group_df:

  Sample metadata with sample IDs and group labels.

- target_genes:

  Character vector of target genes.

- project_name:

  Project name for output files.

- sample_col:

  Optional sample ID column in `group_df`.

- group_col:

  Group label column in `group_df`.

- case_label:

  Positive class label.

## Value

A named list of ROC objects.
