# Get Sea Level Rise (SLR) scenario

Get Sea Level Rise (SLR) scenario

## Usage

``` r
get_scenario(
  id = 520,
  scenario = c("IntLow", "Int", "IntHigh"),
  method = "curl",
  quiet = TRUE
)
```

## Arguments

- id:

  numeric, gauge number. Default is 520 (St. Petersburg, FL).

- scenario:

  character, SLR scenario. Default is `'IntLow'`, `'Int'`, and
  `'IntHigh'` (NOAA Intermediate Low, Intermediate, and Intermediate
  High). See details for available options.

- method:

  character, download method. Default is `'curl'`, passed to
  [`download.file()`](https://rdrr.io/r/utils/download.file.html).

- quiet:

  logical, suppress download messages. Default is `TRUE`.

## Value

A data frame with columns for id, scenario, year, SLR in meters, and SLR
in feet.

## Details

Information from
<https://sealevel.nasa.gov/task-force-scenario-tool?psmsl_id=520>, by
default. Results are SLR in meters and feet for the intermediate low,
intermediate, and intermediate high scenarios based on recommended
scenarios from the Climate Science Advisory Panel. Full options for
scenarios are `'Low'`, `'IntLow'`, `'Int'`, `'IntHigh'`, and `'High'`.
Values for SLR are relative change from 2020.

## Examples

``` r
dat <- get_scenario()
head(dat)
#> # A tibble: 6 × 5
#>      id scenario               year slr_m slr_ft
#>   <dbl> <fct>                 <dbl> <dbl>  <dbl>
#> 1   520 NOAA Intermediate Low  2020 0.113  0.372
#> 2   520 NOAA Intermediate Low  2030 0.180  0.592
#> 3   520 NOAA Intermediate Low  2040 0.250  0.821
#> 4   520 NOAA Intermediate Low  2050 0.322  1.06 
#> 5   520 NOAA Intermediate Low  2060 0.392  1.29 
#> 6   520 NOAA Intermediate Low  2070 0.463  1.52 
```
