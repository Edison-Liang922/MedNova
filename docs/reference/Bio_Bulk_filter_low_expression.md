# Filter Low-Expression Genes

Filter Low-Expression Genes

## Usage

``` r
Bio_Bulk_filter_low_expression(
  expr,
  threshold = 1,
  method = c("mean", "median"),
  logical_transpose = TRUE
)
```

## Arguments

- expr:

  A numeric matrix/data frame with genes in rows and samples in columns.

- threshold:

  Minimum mean/median expression to keep a gene.

- method:

  Statistic for filtering: `"mean"` or `"median"`.

- logical_transpose:

  Logical; whether to transpose output.

## Value

A data frame of filtered expression data.
