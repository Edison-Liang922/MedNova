# Unified MedNova Entry for Bulk Differential Expression

Unified MedNova Entry for Bulk Differential Expression

## Usage

``` r
med_bulk_deg(
  counts,
  group,
  group_col = "group",
  sample_col = NULL,
  align = FALSE,
  align_args = list(),
  filter_low_expression = FALSE,
  filter_args = list(),
  ...
)
```

## Arguments

- counts:

  Expression matrix or data frame.

- group:

  Sample metadata.

- group_col:

  Grouping column.

- sample_col:

  Optional sample ID column.

- align:

  Logical; whether to call
  [`Bio_Bulk_align_group()`](https://example.com/MedNova/reference/Bio_Bulk_align_group.md)
  first.

- align_args:

  Optional list of extra arguments for
  [`Bio_Bulk_align_group()`](https://example.com/MedNova/reference/Bio_Bulk_align_group.md).

- filter_low_expression:

  Logical; whether to call
  [`Bio_Bulk_filter_low_expression()`](https://example.com/MedNova/reference/Bio_Bulk_filter_low_expression.md)
  before limma.

- filter_args:

  Optional list of extra arguments for
  [`Bio_Bulk_filter_low_expression()`](https://example.com/MedNova/reference/Bio_Bulk_filter_low_expression.md).

- ...:

  Additional arguments passed to
  [`Bio_Bulk_limma_analysis()`](https://example.com/MedNova/reference/Bio_Bulk_limma_analysis.md).

## Value

A list with optional `aligned`, optional `filtered_counts`, and `deg`.
