# setup ---------------------------------------------------------------------------------------

library(tidyverse)
library(here)

load(file = here('data/tddat.RData'))

# st pete tide gauge data ---------------------------------------------------------------------

brks <- seq(1950, 2020, by = 10) |> 
  paste0('-01-01') |> 
  as.Date()

p <- tddat |> 
  ggplot(aes(x = date, y = msl_ft)) +
  geom_line(color = 'deepskyblue') +
  labs(
    x = NULL,
    y = 'Feet (MSL)', 
    caption = 'Source: https://tidesandcurrents.noaa.gov/sltrends/data/8726520_meantrend.txt'
  ) +
  scale_y_continuous(breaks = seq(-1, 1, by = 0.5), limits = c(-1, 1)) +
  scale_x_date(breaks = brks, date_labels = '%Y') +
  theme_minimal() + 
  theme(
    panel.grid.minor = element_blank(), 
    panel.grid.major.x = element_blank(), 
    axis.ticks = element_line()
  )

  png(here('docs/fig/tddat.png'), height = 3, width = 8, res = 300, units = 'in')
  print(p)
  dev.off()

# SLR scenarios for st pete -------------------------------------------------------------------

load(file = here('data/slrdat.RData'))
  
p <- ggplot(slrdat, aes(x = year, y = slr_ft, color = scenario, group = scenario)) +
  geom_line(linewidth = 1) +
  labs(
    x = NULL,
    y = 'RSLC in Feet (LMSL)', 
    color = NULL,
    title = 'Relative Sea Level Change Predictions',
    subtitle = 'Gauge 8726520, St. Petersburg, FL' ,
    caption = 'Source: https://sealevel.nasa.gov/task-force-scenario-tool?psmsl_id=520'
  ) +
  scale_color_manual(
    values = c('NOAA Intermediate Low' = 'deepskyblue', 
               'NOAA Intermediate' = 'orange', 
               'NOAA Intermediate High' = 'red')
  ) +
  scale_y_continuous(breaks = seq(0, 6, by = 1), limits = c(0, 6)) +
  scale_x_continuous(breaks = seq(2020, 2100, by = 10)) +
  theme_minimal() + 
  theme(
    panel.grid.minor = element_blank(), 
    panel.grid.major.x = element_blank(), 
    axis.ticks = element_line(), 
    legend.position = 'bottom'
  )
  
png(here('docs/fig/slrdat.png'), height = 4, width = 8, res = 300, units = 'in')
print(p)
dev.off()
  