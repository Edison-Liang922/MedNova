# Run the First-Batch Mendelian Randomization Pipeline

End-to-end MR workflow: process exposure datasets, prepare one outcome,
harmonize them, and run the reference-style batch analysis with output
files.

## Usage

``` r
Bio_MR_pipeline(
  exposure_data_list,
  exposure_names,
  outcome_data,
  exposure_col_map = .mednova_mr_default_col_map(),
  outcome_col_map = .mednova_mr_default_col_map(),
  outcome_name = "Outcome",
  filter_pval = 1e-05,
  filter_f = 10,
  clump_params = list(perform = TRUE, kb = 10000, r2 = 0.001, pval = 5e-08, local =
    FALSE, plink_bin = NULL, bfile = NULL),
  cores = 10,
  out_dir = "MR_results"
)
```

## Arguments

- exposure_data_list:

  List of raw exposure data frames.

- exposure_names:

  Character vector of exposure names.

- outcome_data:

  Raw outcome GWAS data frame.

- exposure_col_map:

  Named list mapping exposure columns.

- outcome_col_map:

  Named list mapping outcome columns.

- outcome_name:

  Outcome phenotype name.

- filter_pval:

  Exposure pre-filter P-value threshold.

- filter_f:

  Exposure F-statistic threshold.

- clump_params:

  Named list of clumping parameters.

- cores:

  Number of cores for harmonization and batch MR. When `cores <= 1`,
  MedNova falls back to a sequential path.

- out_dir:

  Output directory for MR result files.

## Value

A list with `exposure_list`, `outcome_dat`, `harmonized_list`, and
`completed`.
