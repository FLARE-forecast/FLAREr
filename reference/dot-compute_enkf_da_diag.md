# Compute raw EnKF diagnostic quantities for one assimilation step

All quantities are computed from objects that already exist at the call
site in `run_enkf`, so there is no redundant matrix work.

## Usage

``` r
.compute_enkf_da_diag(
  x_matrix,
  update,
  h,
  k_t,
  s_mat,
  zt,
  curr_psi,
  ens_mean,
  nstates,
  ndepths_modeled,
  n_non_vertical,
  npars,
  inflation_start,
  pars_config,
  states_config,
  obs_diag_meta
)
```

## Arguments

- x_matrix:

  prior ensemble `[n_cols x nmembers]`

- update:

  posterior ensemble `[n_cols x nmembers]` (before bound clipping)

- h:

  observation operator `[n_obs x n_cols]`

- k_t:

  Kalman gain `[n_cols x n_obs]`

- s_mat:

  innovation covariance H*P*H^T + R `[n_obs x n_obs]`

- zt:

  active observation vector `[n_obs]`

- curr_psi:

  observation variances `[n_obs]` (= `psi[z_index]^2`)

- ens_mean:

  prior ensemble row means `[n_cols]`

- nstates, ndepths_modeled, n_non_vertical, npars:

  dimension integers

- inflation_start:

  covariance inflation factor applied this step

- pars_config, states_config:

  config data frames (may be NULL)

- obs_diag_meta:

  named list(variable=, depth=) pre-built by caller

## Value

named list of raw diagnostic scalars/vectors
