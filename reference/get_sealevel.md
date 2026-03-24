# Get monthly Mean Sea Level (MSL) data for selected gauge

Get monthly Mean Sea Level (MSL) data for selected gauge

## Usage

``` r
get_sealevel(gauge = 8726520)
```

## Arguments

- gauge:

  numeric, gauge number. Default is 8726520 (St. Petersburg, FL).

## Value

A data frame with columns for gauge, year, month, date, MSL in meters,
and MSL in feet.

## Details

Information from <https://tidesandcurrents.noaa.gov> using the URL
<https://tidesandcurrents.noaa.gov/sltrends/data/8726520_meantrend.txt>,
by default. Results are monthly Mean Sea Level (MSL) in meters and feet,
with the seasonal cycle removed.

## Examples

``` r
dat <- get_sealevel(gauge = 8726520)
head(dat)
#>     gauge Year Month       date  msl_m     msl_ft
#> 1 8726520 1947     1 1947-01-01 -0.062 -0.2034121
#> 2 8726520 1947     2 1947-02-01 -0.132 -0.4330709
#> 3 8726520 1947     3 1947-03-01 -0.143 -0.4691601
#> 4 8726520 1947     4 1947-04-01 -0.150 -0.4921260
#> 5 8726520 1947     5 1947-05-01 -0.086 -0.2821522
#> 6 8726520 1947     6 1947-06-01 -0.064 -0.2099738
```
