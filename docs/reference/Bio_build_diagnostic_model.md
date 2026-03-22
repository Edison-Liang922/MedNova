# Build a Diagnostic Model With Nomogram and DCA

Build a multigene logistic model, generate a nomogram PDF, and save a
DCA plot in the current working directory.

## Usage

``` r
Bio_build_diagnostic_model(
  expr,
  group_df,
  target_genes,
  project_name = "Combined_Model",
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

  Project name used for output files.

- sample_col:

  Optional sample ID column in `group_df`.

- group_col:

  Group label column in `group_df`.

- case_label:

  Positive class label.

## Value

A list with `model`, `dca_data`, and `plot`.
