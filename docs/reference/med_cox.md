# Unified MedNova Entry for Cox Regression

Unified MedNova Entry for Cox Regression

## Usage

``` r
med_cox(data, time_col, status_col, predictors, ...)
```

## Arguments

- data:

  A data frame containing survival inputs.

- time_col:

  Column name for survival time.

- status_col:

  Column name for event status.

- predictors:

  Character vector of predictor terms.

- ...:

  Additional arguments passed to
  [`Epi_cox_multiv()`](https://example.com/MedNova/reference/Epi_cox_multiv.md).

## Value

A list with a single `multiv` result.
