# Plot sea level scenario data

Plot sea level scenario data

## Usage

``` r
plot_scenario(
  dat,
  cols = c("deepskyblue", "orange", "red"),
  units = "ft",
  linewidth = 1,
  caption = TRUE,
  xrng = c(2020, 2100),
  xbrk = 10,
  yrng = NULL,
  ybrk = 7,
  plotly = FALSE
)
```

## Arguments

- dat:

  Input data from
  [`get_scenario()`](https://github.com/tbep-tech/slrcsap/reference/get_scenario.md).

- cols:

  character, color palette for the lines. Default is
  `c('deepskyblue', 'orange', 'red')`.

- units:

  character, units for the y-axis. Default is `'ft'`. Options are `'ft'`
  and `'m'`.

- linewidth:

  numeric, line width. Default is `1`.

- caption:

  logical, add caption with source. Default is `TRUE`. Does not apply if
  `plotly = TRUE`.

- xrng:

  numeric, x-axis range. Default is `c(2020, 2100)`.

- xbrk:

  numeric, number of x-axis breaks. Default is `10`. Does not apply if
  `plotly = TRUE`.

- yrng:

  numeric, y-axis range as two values. Default is `NULL`, which uses the
  range of the data.

- ybrk:

  numeric, number of y-axis breaks. Default is `7`. Does not apply if
  `plotly = TRUE`.

- plotly:

  logical, if `TRUE`, returns a plotly object instead of ggplot. Default
  is `FALSE`.

## Value

A ggplot object

## Examples

``` r
dat <- get_scenario()
plot_scenario(dat)
```
