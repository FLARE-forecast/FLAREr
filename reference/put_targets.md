# Save target files to s3 bucket

Save target files to s3 bucket

## Usage

``` r
put_targets(
  site_id,
  cleaned_insitu_file = NA,
  cleaned_met_file = NA,
  cleaned_inflow_file = NA,
  use_s3 = FALSE,
  config = NULL
)
```

## Arguments

- site_id:

  four letter code for the site

- cleaned_insitu_file:

  full path of the cleaned insitu file

- cleaned_met_file:

  full path of the cleaned met file

- cleaned_inflow_file:

  full path of the cleaned inflow file

- use_s3:

  logical; TRUE = use s3

- config:

  list of FLARE configurations
