# Read AED2 module initial values from an aed2.nml file.

Looks up the `*_initial` field in each AED module section for the
standard FLARE WQ variable names, and reads phytoplankton initial values
from the CSV database referenced by `aed_phytoplankton$dbase`.

## Usage

``` r
read_aed_initial_values(aed2_nml_file)
```

## Arguments

- aed2_nml_file:

  Path to an aed2.nml file.

## Value

Named list mapping FLARE state name -\> scalar initial value.
