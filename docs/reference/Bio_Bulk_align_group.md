# Align Bulk Expression and Group Metadata

Align Bulk Expression and Group Metadata

## Usage

``` r
Bio_Bulk_align_group(
  expr,
  group,
  sample_col = "GSM",
  group_col = "group",
  dataset_col = "Dataset",
  id_prefix = "Data",
  recode_map = c(normal = "Control", cirrhosis = "Case"),
  keep_group_cols = NULL,
  reorder = TRUE
)
```

## Arguments

- expr:

  Expression matrix/data frame with sample IDs in row names.

- group:

  Sample metadata data frame.

- sample_col:

  Column name in `group` containing sample IDs.

- group_col:

  Column name in `group` containing group labels.

- dataset_col:

  Column name to use for renamed sample IDs.

- id_prefix:

  Prefix for new sample IDs.

- recode_map:

  Named character vector for recoding group labels.

- keep_group_cols:

  Optional character vector of columns to keep in `group`.

- reorder:

  Logical; whether to reorder and subset to common samples.

## Value

A list with `expr`, `group`, and `id_map`.
