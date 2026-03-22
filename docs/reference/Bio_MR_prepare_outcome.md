# Prepare Outcome Data for Mendelian Randomization

Format a raw GWAS outcome table into a TwoSampleMR-compatible outcome
dataset.

## Usage

``` r
Bio_MR_prepare_outcome(
  raw_data,
  phenotype_name = "Outcome",
  col_map = .mednova_mr_default_col_map(),
  phenotype_col = "trait"
)
```

## Arguments

- raw_data:

  Raw GWAS outcome summary statistics.

- phenotype_name:

  Outcome phenotype name.

- col_map:

  Named list mapping standard MR fields to columns in `raw_data`.

- phenotype_col:

  Column name used as the phenotype label during formatting.

## Value

A data frame in TwoSampleMR outcome format.
