# Download a single object to a local path.

Uniform wrapper that dispatches to FaaSr RPC, direct S3, or local
filesystem based on `flare_io_mode(config)`. Argument names and defaults
match the FaaSr R stub `faasr_get_file()`.

## Usage

``` r
flare_get_file(
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

## Value

`TRUE` on success (invisibly), or whatever the underlying stub returns
under `mode="faasr"`.
