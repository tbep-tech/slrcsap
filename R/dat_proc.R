# setup ---------------------------------------------------------------------------------------

library(tbeptools)
library(tidyverse)
library(here)

# get st pete tide gauge data -----------------------------------------------------------------

# saves as csv
tddatraw <- tbeptools::read_importsealevels(
  path_csv = here('data-raw/stpete_tide_gauge.csv'), 
  df_stations = tibble(station_id = '8726520', station_name = 'St. Petersburg'),
  product = 'monthly_mean', 
  datum = 'MSL', 
  units = 'english')

tddat <- tddatraw |> 
  select(date, msl_ft = msl) |> 
  mutate(
    msl_m = msl_ft * 0.3048 # convert to meters
  )

save(tddat, file = here('data/tddat.RData'))