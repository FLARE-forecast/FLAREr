# Delete a single remote object.

Delete a single remote object.

## Usage

``` r
flare_delete_file(remote_file, server_name = "", remote_folder = "", config)
```

## Arguments

- remote_file:

  Remote object filename.

- server_name:

  Name of the DataStore (key under `config$s3`).

- remote_folder:

  Remote folder/prefix.

- config:

  FLAREr config list.

## Value

`TRUE` on success (invisibly).
