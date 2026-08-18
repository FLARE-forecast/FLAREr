# List object keys under a prefix.

List object keys under a prefix.

## Usage

``` r
flare_get_folder_list(server_name = "", prefix = "", local_path = NULL, config)
```

## Arguments

- server_name:

  Name of the DataStore (key under `config$s3`).

- prefix:

  Prefix to list under.

- local_path:

  Optional local filesystem directory to enumerate when `mode="local"`.
  When supplied AND mode resolves to "local", returns
  `list.files(local_path, recursive=TRUE, full.names=FALSE)`. When NULL
  (default) AND mode="local", returns `character(0)`, so remote-list
  callers iterate zero times in pure-local mode. Callers that need an
  existence check that works under BOTH local and remote modes (e.g.
  [`get_run_config()`](http://flare-forecast.org/FLAREr/reference/get_run_config.md))
  should pass `local_path` so the local enumeration is meaningful.

- config:

  FLAREr config list.

## Value

Character vector of object keys / relative file paths.
