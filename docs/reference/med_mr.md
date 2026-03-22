# Unified MedNova Entry for Mendelian Randomization

Unified MedNova Entry for Mendelian Randomization

## Usage

``` r
med_mr(exposure_data_list, exposure_names, outcome_data, ...)
```

## Arguments

- exposure_data_list:

  List of raw exposure data frames, or a single raw exposure data frame.

- exposure_names:

  Character vector of exposure names.

- outcome_data:

  Raw outcome GWAS data frame.

- ...:

  Additional arguments passed to
  [`Bio_MR_pipeline()`](https://example.com/MedNova/reference/Bio_MR_pipeline.md).

## Value

The output of
[`Bio_MR_pipeline()`](https://example.com/MedNova/reference/Bio_MR_pipeline.md).
