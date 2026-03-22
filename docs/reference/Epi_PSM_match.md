# Propensity Score Matching (PSM)

Generic PSM wrapper using MatchIt. Returns matched data for downstream
analysis.

## Usage

``` r
Epi_PSM_match(
  data,
  treat_col,
  covariates,
  case_label = NULL,
  control_label = NULL,
  method = "nearest",
  distance = "glm",
  ratio = 1,
  caliper = 0.1,
  replace = FALSE,
  return_model = FALSE,
  ...
)
```

## Arguments

- data:

  A data frame containing treatment/exposure and covariates.

- treat_col:

  Column name for treatment/exposure variable (binary).

- covariates:

  Character vector of covariate column names used to estimate the
  propensity score.

- case_label:

  Optional value in `treat_col` representing the treated group.

- control_label:

  Optional value in `treat_col` representing the control group.

- method:

  Matching method.

- distance:

  Distance model.

- ratio:

  Matching ratio.

- caliper:

  Caliper width.

- replace:

  Whether to match with replacement.

- return_model:

  Logical; return the `matchit` object as well.

- ...:

  Additional arguments passed to
  [`MatchIt::matchit`](https://kosukeimai.github.io/MatchIt/reference/matchit.html).

## Value

If `return_model = FALSE`, a matched data frame. Otherwise a list with
`matched_data` and `matchit_model`.
