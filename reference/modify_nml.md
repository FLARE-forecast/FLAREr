# Apply in-memory updates to a parsed GLM/ELCOM namelist

Apply in-memory updates to a parsed GLM/ELCOM namelist

## Usage

``` r
modify_nml(nml, var_list, var_name_list)
```

## Arguments

- nml:

  Parsed nml list (from `read_nml`).

- var_list:

  List of values, parallel to `var_name_list`.

- var_name_list:

  Character vector of variable names to update.

## Value

The modified nml list.
