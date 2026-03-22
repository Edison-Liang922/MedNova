# Unified MedNova Entry for Logistic Regression

Unified MedNova Entry for Logistic Regression

## Usage

``` r
med_logistic(
  data,
  outcome_col,
  predictors,
  mode = c("both", "multiv", "univ"),
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

- mode:

  One of `"multiv"`, `"univ"`, or `"both"`.

- positive_label:

  Optional positive class label.

- negative_label:

  Optional negative class label.

- remove_na:

  Logical; whether to drop rows with missing values.

- pvalue_accuracy:

  Accuracy for p-value formatting.

## Value

A list with `mode`, optional `univ`, and optional `multiv`.
