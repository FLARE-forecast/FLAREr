# Check if NOAA forecasts have been downloaded and processed

Check if NOAA forecasts have been downloaded and processed

## Usage

``` r
check_noaa_present(
  lake_directory,
  configure_run_file = "configure_run.yml",
  config_set_name = "default"
)
```

## Arguments

- lake_directory:

  four-letter code for site

- configure_run_file:

  name of simulation

- config_set_name:

  FLARE configuration object (needed for s3 buckets and endpoit)

## Value

logical

## Examples

``` r
if (FALSE) { # interactive()

dir <- normalizePath(tempdir(),  winslash = "/")
lake_directory <- file.path(dir, "extdata")
# Copy files to temporarly directory
dir.create(dir,showWarnings = FALSE)
file.copy(system.file("extdata", package = "FLAREr"),
          tempdir(),
          recursive = TRUE)

check_noaa_present(lake_directory,
                   configure_run_file = "configure_run.yml",
                   config_set_name = "default")
}
```
