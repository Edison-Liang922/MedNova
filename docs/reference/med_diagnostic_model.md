# Unified MedNova Entry for Diagnostic Modeling

Unified MedNova Entry for Diagnostic Modeling

## Usage

``` r
med_diagnostic_model(
  expr,
  group_df,
  target_genes,
  project_name = "Combined_Model",
  sample_col = NULL,
  group_col = "group",
  case_label = "Case",
  include_roc = TRUE
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

  Project name used for output files.

- sample_col:

  Optional sample ID column in `group_df`.

- group_col:

  Group label column in `group_df`.

- case_label:

  Positive class label.

- include_roc:

  Logical; whether to also run
  [`plot_diagnostic_roc()`](https://example.com/MedNova/reference/plot_diagnostic_roc.md).

## Value

A list with `model` and optional `roc`.
