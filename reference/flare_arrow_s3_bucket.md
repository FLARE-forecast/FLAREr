# Return an `arrow::s3_bucket()` (or local) handle for a partitioned dataset under a server/prefix.

Used for
[`arrow::open_dataset()`](https://arrow.apache.org/docs/r/reference/open_dataset.html)
/
[`arrow::write_dataset()`](https://arrow.apache.org/docs/r/reference/write_dataset.html)
calls. Note that under any mode, `arrow` operations on the returned
handle transit directly between R and the storage backend — they do not
funnel through the FaaSr RPC server. FaaSr (when in `mode="faasr"`)
provides the credentials, but not the traffic path.

## Usage

``` r
flare_arrow_s3_bucket(
  server_name = "",
  faasr_prefix = "",
  local_path = NULL,
  mode_override = NULL,
  config
)
```

## Arguments

- server_name:

  Name of the DataStore (key under `config$s3`).

- faasr_prefix:

  Optional sub-prefix appended to the bucket path.

- local_path:

  Filesystem path used in `mode="local"`. Caller is responsible for
  constructing this from any per-driver YAML conventions (e.g.
  `config$met$future_met_model`). Ignored in `mode="s3"`/`"faasr"`. If
  NULL in `mode="local"`, falls back to
  `<.flare_local_root(config)>/<server_name>/<faasr_prefix>`.

- mode_override:

  Optional explicit mode (`"local"`, `"s3"`, or `"faasr"`) that bypasses
  `flare_io_mode(config)`. Used when a per-driver YAML toggle (e.g.
  `config$met$future_met_use_s3`, `config$flows$use_flows_s3`) makes a
  specific driver local while global outputs still go to S3. NULL
  (default) uses config-driven dispatch.

- config:

  FLAREr config list.

## Value

An
[`arrow::s3_bucket()`](https://arrow.apache.org/docs/r/reference/s3_bucket.html)
handle (modes `s3`/`faasr`) or
[`arrow::SubTreeFileSystem`](https://arrow.apache.org/docs/r/reference/FileSystem.html)
(mode `local`).
