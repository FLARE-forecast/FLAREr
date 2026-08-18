# Build observation error covariance matrix

Build observation error covariance matrix

## Usage

``` r
build_R_matrix(psi, z_index)
```

## Arguments

- psi:

  Numeric vector of observation standard deviations (all obs types).

- z_index:

  Integer vector of active observation indices for this timestep.

## Value

Diagonal matrix `[nobs, nobs]` with squared SDs on the diagonal.
