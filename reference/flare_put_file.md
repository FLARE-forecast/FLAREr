# Upload a single local file as an object.

Upload a single local file as an object.

## Usage

``` r
flare_put_file(
  local_file,
  remote_file,
  server_name = "",
  local_folder = ".",
  remote_folder = ".",
  config
)
```

## Arguments

- local_file:

  Local destination filename.

- remote_file:

  Remote object filename.

- server_name:

  Name of the DataStore (key under `config$s3`).

- local_folder:

  Local folder for `local_file`.

- remote_folder:

  Remote folder/prefix for `remote_file`.

- config:

  FLAREr config list.
