# Propose new parameters for use in data assimilation and forecasting

Propose new parameters for use in data assimilation and forecasting

## Usage

``` r
propose_parameters(
  i,
  m,
  pars,
  pars_config,
  npars,
  par_fit_method,
  da_method,
  hist_days,
  include_uncertainty
)
```

## Arguments

- i:

  time step index

- m:

  ensemble member number

- pars:

  matrix of parameters

- pars_config:

  parameter configuration data frame

- npars:

  number of parameters

- par_fit_method:

  method for parameter fitting

- da_method:

  data assimilation method

- hist_days:

  number of simulation days before forecasting

- include_uncertainty:

  include parameter uncertainty in forecasts

## Value

vector of new parameter values for the ensemble member
