library(tidyverse)
library(tidync)
library(sf)
library(terra)
library(tidyterra)
library(rnaturalearth)
library(plotly)
library(VulnToolkit)



p <- ggplot() +
  # geom_line(data = gom_analysis, aes(Date, SLA), color = "lightblue", linewidth = 1) +
  geom_line(data = SSHA_seavey_monthly, aes(Date, SLA), color = "lightblue") +
  geom_line(data = NOAA_Seavey, aes(Date, MSL)) +
  geom_smooth(data = SSHA_seavey_monthly, aes(Date, SLA), color = "lightblue", method = lm) +
  geom_smooth(data = NOAA_Seavey, aes(Date, MSL), alpha = .5, method = lm)

  
ggplotly(p, dynamicTicks = T)

p <- ggplot() +
  # geom_line(data = gom_analysis, aes(Date, SLA), color = "lightblue", linewidth = 1) +
  # geom_ribbon(data = SSHA_portland_monthly, aes(Date, ymin = SLA-SLA_ERR, ymax = SLA+SLA_ERR), fill = "lightblue", alpha = .5) +
  geom_line(data = SSHA_portland_monthly, aes(Date, SLA), color = "lightblue")
  # geom_line(data = NOAA_Seavey, aes(Date, MSL)) +
  # geom_line(data = NOAA_Portland, aes(Date, MSL)) +
  # geom_line(data = NOAA_Portland_daily, aes(Date, daily_MSL), alpha = .5)
  # geom_line(data = NOAA_Portland_weekly, aes(Date, fivedayavg), alpha = .5) +
  geom_line(data = NOAA_Portland, aes(Date, MSL), alpha = .5) 
  # geom_smooth(data = SSHA_portland_monthly, aes(Date, SLA), color = "lightblue", method = lm) +
  # geom_smooth(data = NOAA_Portland_weekly, aes(Date, fivedayavg), alpha = .5, method = lm) +
  # geom_smooth(data = gom_analysis, aes(Date, SLA), color = "lightblue") +
  # geom_smooth(data = NOAA_Portland, aes(Date, MSL), alpha = .5, method = lm)
  # geom_smooth(data = gom_monthly, aes(Date, SLA), color = "lightblue", method = lm)

ggplotly(p, dynamicTicks = T)

tgalt <- gom |> 
  mutate(Year = year(Date),
         Month = month(Date)) |> 
  group_by(Year, Month) |> 
  summarise(SLA = mean(SLA)) |> 
  mutate(Date = ymd_h(paste0(as.character(Year), "-", as.character(Month), "-", 01, " ", "12")) + months(0)) |> 
  left_join(NOAA_Seavey, gom_analysis, by = c("Date"))

ggplot(tgalt, aes(SLA, Monthly_MSL)) +
  geom_point()
