# Process Exposure Data for Mendelian Randomization

A lightweight MedNova migration of the reference exposure-processing
pipeline: format, instrument-strength filtering, and optional clumping.

## Usage

``` r
Bio_MR_process_exposure(
  raw_data,
  phenotype_name,
  col_map = .mednova_mr_default_col_map(),
  filter_pval = 1e-05,
  filter_f = 10,
  clump_params = list(perform = TRUE, kb = 10000, r2 = 0.001, pval = 5e-08, local =
    FALSE, plink_bin = NULL, bfile = NULL)
)
```

## Arguments

- raw_data:

  Raw GWAS exposure summary statistics.

- phenotype_name:

  Exposure phenotype name.

- col_map:

  Named list mapping standard MR fields to columns in `raw_data`.

- filter_pval:

  P-value threshold applied before formatting.

- filter_f:

  F-statistic threshold used after formatting.

- clump_params:

  Named list of clumping parameters. Supported keys are `perform`, `kb`,
  `r2`, `pval`, `local`, `plink_bin`, and `bfile`.

## Value

A processed exposure data frame, or `NULL` if no SNPs remain.
