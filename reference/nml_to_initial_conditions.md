# Read GLM nml initial profile fields and return a list in the format of generate_initial_conditions().

The GLM nml stores the_heights, the_temps, the_sals, and wq_init_vals in
bottom-to-top (ascending height) order. wq_init_vals is a flat vector
where the first num_heights values belong to `wq_names[1]`, the next
num_heights values to `wq_names[2]`, etc. (same ordering written by
run_model.R). This function reverses that encoding back into the depth-
major, surface-first layout used by the FLARE states array.

## Usage

``` r
nml_to_initial_conditions(
  nml_file,
  states_config,
  modeled_depths,
  max_model_layers = NULL,
  nmembers = 1L,
  aed2_nml_file = NULL
)
```

## Arguments

- nml_file:

  Path to a GLM glm3.nml file.

- states_config:

  Data frame with at least a `state_names` column, matching the FLARE
  states_config.csv convention (temp, salt, then WQ vars).

- modeled_depths:

  Numeric vector of depths from the surface (m, positive downward) at
  which FLARE tracks states.

- max_model_layers:

  Integer. Defaults to length(modeled_depths).

- nmembers:

  Number of ensemble members. Defaults to 1; the same interpolated
  values are replicated across all members.

- aed2_nml_file:

  Optional path to an aed2.nml file. When supplied, any state absent
  from the glm nml is filled with the corresponding `*_initial` value
  from the AED2 module configuration.

## Value

A list with elements `states`, `pars`, and `aux_states_init` matching
the structure returned by generate_initial_conditions().

## Details

For any state in states_config that is not found in the glm nml (e.g. a
WQ variable absent from wq_names), the function optionally falls back to
the scalar initial value from aed2_nml_file, replicated uniformly across
all modeled depths.
