# Map the states to the observations

Map the states to the observations

## Usage

``` r
generate_states_to_obs_mapping(states_config, obs_config)
```

## Arguments

- states_config:

  list; list of state configurations

- obs_config:

  list; list of observation configurations

## Value

updated states_config list

## Details

Convert columns the map states to observations to a single vector. This
is necessary because not all states have observations, multiple states
can contribute to the same observation, and one observation can be
associated with multiple states. Function must be assigned to the
states_config object

## Author

Quinn Thomas
