# Resolve the I/O backend for a FLARE run from `config$run_config`.

Returns one of `"faasr"`, `"s3"`, `"local"`. Errors if `use_faasr=TRUE`
while `use_s3=FALSE`; FaaSr mode is always cloud-backed. Falls back to
`"s3"` with a warning when `use_faasr=TRUE` but the FaaSr RPC stubs are
not loaded into
[`globalenv()`](https://rdrr.io/r/base/environment.html), i.e. when not
running inside a FaaSr container.

## Usage

``` r
flare_io_mode(config)
```

## Arguments

- config:

  FLAREr config list (must contain `run_config$use_s3` and optionally
  `run_config$use_faasr`).

## Value

Character scalar: `"faasr"`, `"s3"`, or `"local"`.
