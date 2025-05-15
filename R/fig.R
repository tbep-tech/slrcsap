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
    x = 'Date',
    y = 'Feet (MSL)'
  ) +
  scale_x_date(breaks = brks, date_labels = '%Y') +
  theme_minimal() + 
  theme(
    panel.grid.minor = element_blank(), 
    panel.grid.major.x = element_blank(), 
    axis.ticks = element_line()
  )

  png(here('fig/tddat.png'), height = 3, width = 8, res = 300, units = 'in')
  print(p)
  dev.off()
