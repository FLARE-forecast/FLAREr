# Generate parquet output file

Generate parquet output file

## Usage

``` r
write_forecast(
  da_forecast_output,
  use_s3 = FALSE,
  bucket = NULL,
  endpoint = NULL,
  local_directory = NULL,
  config = NULL
)
```

## Arguments

- da_forecast_output:

  list; object that is returned by run_da_forecast()

- use_s3:

  Boolen; use s3 storage for saving scores

- bucket:

  S3 bucket

- endpoint:

  S3 endpoint

- local_directory:

  local directory of scores if not using s3

## Value

None

## Details

Function generates a parquet file from the object that is returned by
run_da_forecast()

## Author

Quinn Thomas
