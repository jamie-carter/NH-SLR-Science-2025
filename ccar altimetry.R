library(tidyverse)
library(plotly)

# data from CCAR ---- https://ccar.colorado.edu/altimetry/

## Galveston

g <- read_csv("CCAR/CCAR_altimetry_292500N_947500W_tidegauge_775_Galveston.csv", skip = 16) |> 
  dplyr::select(1,2,5) |> 
  rename(time = 1, Altimetry = 2, `Tide Gauge` = 3) |> 
  mutate(`Tide Gauge` = `Tide Gauge` + 11.48) |> 
  mutate(Difference = `Tide Gauge` - Altimetry)

g_lm <- lm(Difference ~ time, g)

summary(g_lm)

gl <- g |> 
  pivot_longer(cols = c(2:4), names_to = "Sensor", values_to = "SLA (cm)")

p <- ggplot(gl, aes(time, `SLA (cm)`, colour = Sensor)) +
  geom_line()

ggplotly(p)
