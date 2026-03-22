# Forest Plot for Logistic Regression Results

Create a forest plot from logistic regression OR estimates and
confidence intervals.

## Usage

``` r
plot_logistic_forest(
  result_df,
  title = "Logistic Regression Forest Plot",
  color = "blue",
  log_scale = TRUE,
  breaks = c(0.5, 1, 2, 4, 8)
)
```

## Arguments

- result_df:

  Data frame with columns `variable`, `level`, `OR`, and `OR_95CI`.

- title:

  Plot title.

- color:

  Point and interval color.

- log_scale:

  Logical; use a log10 x-axis.

- breaks:

  Numeric axis breaks when `log_scale = TRUE`.

## Value

A ggplot object.
