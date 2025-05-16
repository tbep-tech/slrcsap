# setup ---------------------------------------------------------------------------------------

library(tidyverse)
library(readxl)
library(here)

# get st pete tide gauge data -----------------------------------------------------------------

# monthly MSL, seasonal cycle removed
tddat <- read.table('https://tidesandcurrents.noaa.gov/sltrends/data/8726520_meantrend.txt', 
                    skip = 6, sep = '', header = F) |> 
  select(Year = V1, Month = V2, msl_m = V3) |> 
  mutate(
    date = as.Date(paste(Year, Month, 1), format = '%Y %m %d'),
    msl_ft = msl_m * 3.28084
  )

save(tddat, file = here('data/tddat.RData'))

# SLR scenarios for St Pete -------------------------------------------------------------------

# https://sealevel.nasa.gov/task-force-scenario-tool?psmsl_id=520
download.file('https://d3qt3aobtsas2h.cloudfront.net/edge/ws/search/projection?psmsl_id=520&format=csv&task_force=true', 
              destfile = here('data-raw/sl_taskforce_scenarios_psmls_id_520.xlsx'), 
              method = 'curl')

# units in mm, convert to feet
# select low, intermediate, high scenario
slrdat <- read_excel(here('data-raw/sl_taskforce_scenarios_psmls_id_520.xlsx'), sheet = 'Total') |> 
  filter(scenario %in% c('IntLow', 'Int', 'IntHigh')) |> 
  filter(quantile == 50) |> 
  select(scenario, `2020`:`2100`) |> 
  pivot_longer(cols = `2020`:`2100`, names_to = 'year', values_to = 'slr_mm') |>
  mutate(
    year = as.numeric(year),
    slr_ft = slr_mm / 304.8, # convert mm to feet
    scenario = factor(scenario, levels = c('IntLow', 'Int', 'IntHigh'), 
                      labels = c('NOAA Intermediate Low', 'NOAA Intermediate', 'NOAA Intermediate High'))
  ) 

save(slrdat, file = here('data/slrdat.RData'))

