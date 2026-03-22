# Multivariable Logistic Regression

Multivariable Logistic Regression

## Usage

``` r
Epi_logistic_multiv(
  data,
  outcome_col,
  predictors,
  positive_label = NULL,
  negative_label = NULL,
  remove_na = TRUE,
  pvalue_accuracy = 0.001
)
```

## Arguments

- data:

  A data frame containing outcome and predictors.

- outcome_col:

  Column name for binary outcome.

- predictors:

  Character vector of predictor column names.

- positive_label:

  Optional value in `outcome_col` mapped to 1.

- negative_label:

  Optional value in `outcome_col` mapped to 0.

- remove_na:

  Logical; whether to drop rows with missing values.

- pvalue_accuracy:

  Accuracy for p-value formatting if `scales` is available.

## Value

A list with `results` and `model`.
