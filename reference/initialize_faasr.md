# Validate FaaSr / S3 / local-mode configuration at setup time.

The `flare_io.R` wrappers read `config$s3` and `config$run_config`
directly, so no global state needs initializing. This function exists to
fail fast on internally inconsistent configurations and to surface a
warning early when AWS credentials are missing under `mode="s3"`.

## Usage

``` r
initialize_faasr(config)
```

## Arguments

- config:

  FLAREr config list with `run_config$use_s3` and optionally
  `run_config$use_faasr`.

## Value

`NULL`, invisibly. Errors if `use_faasr=TRUE` while `use_s3=FALSE`.
Warns if `use_s3=TRUE` and AWS credentials are not set in the
environment.
