# setup ---------------------------------------------------------------------------------------

library(tidyverse)
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
