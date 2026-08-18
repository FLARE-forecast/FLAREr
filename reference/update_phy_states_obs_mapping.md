# Update states_to_obs_mapping_1 for PHY\_ variables in a states_config data frame.

Reads Xcc (carbon-to-chlorophyll ratio) from the phytoplankton parameter
database referenced by aed_phytoplankton\$dbase in the supplied
aed2.nml, then sets states_to_obs_mapping_1 = 12.0 / Xcc for each PHY\_
row. This matches the AED2 conversion chla = (carbon_mmolC_m3 / Xcc) \*
12.0, so the multiplier from carbon state to chla observation is 12/Xcc.

## Usage

``` r
update_phy_states_obs_mapping(states_config, nml_path)
```

## Arguments

- states_config:

  Data frame with a state_names column and a states_to_obs_mapping_1
  column (as returned by read.csv on states_config.csv).

- nml_path:

  Path to an aed2.nml file.

## Value

The modified states_config data frame.
