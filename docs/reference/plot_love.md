# Love Plot for Absolute Standardized Mean Differences

Create a love plot comparing absolute standardized mean differences
before and after matching or weighting.

## Usage

``` r
plot_love(
  data,
  covariate_col = "Covariate",
  before_col = "AbsSMD_Before_max",
  after_col = "AbsSMD_After_max",
  threshold = 0.1,
  colors = c(Before = "#D62728", After = "#1F77B4"),
  connect_lines = TRUE,
  x_breaks = seq(0, 0.3, by = 0.05),
  x_limit = NULL,
  base_size = 12
)
```

## Arguments

- data:

  Data frame containing covariate and SMD columns.

- covariate_col:

  Column name for covariates.

- before_col:

  Column name for the pre-adjustment absolute SMD.

- after_col:

  Column name for the post-adjustment absolute SMD.

- threshold:

  Vertical threshold line.

- colors:

  Named vector with entries `Before` and `After`.

- connect_lines:

  Logical; draw segments between before and after points.

- x_breaks:

  Numeric vector of x-axis breaks.

- x_limit:

  Optional x-axis limits.

- base_size:

  Base font size.

## Value

A list with `plot` and `data`.
