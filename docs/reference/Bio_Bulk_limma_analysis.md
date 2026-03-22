# Limma Differential Expression for Bulk RNA-seq

Limma Differential Expression for Bulk RNA-seq

## Usage

``` r
Bio_Bulk_limma_analysis(
  counts,
  group,
  group_col = "group",
  sample_col = NULL,
  case_name = "Case",
  control_name = "Control",
  logFC_threshold = 0.5,
  adj_P_threshold = 0.05
)
```

## Arguments

- counts:

  Expression matrix or data frame.

- group:

  Sample metadata with sample IDs and a grouping column.

- group_col:

  Column name in `group` for case/control labels.

- sample_col:

  Optional column name in `group` for sample IDs.

- case_name:

  Case label in `group_col`.

- control_name:

  Control label in `group_col`.

- logFC_threshold:

  Log2 fold-change threshold for calling DE genes.

- adj_P_threshold:

  Adjusted P-value threshold for calling DE genes.

## Value

A list with `results`, `deg_list`, and `contrast`.
