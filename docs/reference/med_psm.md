# Unified MedNova Entry for Propensity Score Matching

Unified MedNova Entry for Propensity Score Matching

## Usage

``` r
med_psm(
  data,
  treat_col,
  covariates,
  baseline = FALSE,
  return_baseline = baseline,
  return_love_plot = FALSE,
  love_data = NULL,
  ...
)
```

## Arguments

- data:

  A data frame containing treatment and covariates.

- treat_col:

  Column name for treatment/exposure variable.

- covariates:

  Character vector of covariate column names.

- baseline:

  Deprecated compatibility alias for `return_baseline`.

- return_baseline:

  Logical; whether to also compute baseline tables.

- return_love_plot:

  Logical; whether to also generate a love plot.

- love_data:

  Optional data frame already prepared for
  [`plot_love()`](https://example.com/MedNova/reference/plot_love.md).
  If omitted, `med_psm()` will derive love plot data from the matching
  summary.

- ...:

  Additional arguments passed to
  [`Epi_PSM_match()`](https://example.com/MedNova/reference/Epi_PSM_match.md).

## Value

A structured list containing `matched_object`, `matchit_model`,
`matched_data`, optional `baseline_tables`, `balance_table`,
`love_plot_data`, and optional `love_plot`.
