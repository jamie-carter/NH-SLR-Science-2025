library(tidyverse)
library(tidync)
library(sf)
library(terra)
library(tidyterra)
library(rnaturalearth)
library(plotly)
library(VulnToolkit)
library(fpp3)

# Obtain tide gauge data ----

## Seavey Island ----

NOAA_Seavey1 <-
  noaa(
    begindate = "19260901",
    enddate = "20241231",
    station = "8419870",
    units = "meters",
    datum = "MSL",
    interval = "monthly",
    time = "GMT"
  ) |> 
  drop_na(MSL)

NOAA_Seavey2 <-
  noaa(
    begindate = "20030801",
    enddate = "20190930",
    station = "8423898",
    units = "meters",
    datum = "MSL",
    interval = "monthly",
    time = "GMT"
  ) |> 
  drop_na(MSL)

NOAA_Seavey <- bind_rows(NOAA_Seavey1, NOAA_Seavey2) |>
  mutate(Date = as.Date(ym(paste0(as.character(Year), "-", as.character(Month))))) |>
  drop_na(Date) |> 
  complete(Date = seq.Date(min(Date), max(Date), by = "month")) |> 
  arrange(Date) |>
  mutate(station = "Seavey Island")

# NOAA_Seavey <- read_csv("COOPS Tide Gauges/8419870_meantrend.csv", skip = 5) |> 
#   mutate(Date = as.Date(ym(paste0(as.character(Year), "-", as.character(Month))))) |> 
#   arrange(Date) |> 
#   rename("MSL" = "Monthly_MSL") |> 
#   mutate(station = "Seavey") |> 
#   complete(Date = seq.Date(min(Date), max(Date), by = "month")) |> 
#   mutate(Date = as.POSIXct(Date))


tgs <- read_csv("C:/Users/Jamie.Carter/Documents/aa_Resilience/SLRTF/Sea_Level_Rise_Datasets_2022/SLR_TF U.S. Sea Level Projections.csv", skip = 17) %>%
  select(1:4,6:8) %>%
  unique() |> 
  filter(`Regional Classification` == "Northeast", Lat >= 41, !is.na(`NOAA Name`), `PSMSL Site` != "CUTLER_II")

wrangle_NOAA <- function(x,y) {
  noaa(
    begindate = "19921010",
    enddate = "20240101",
    station = x,
    units = "meters",
    datum = "MSL",
    interval = "monthly",
    time = "GMT"
  ) |> 
    drop_na(MSL) |> 
    mutate(Date = as.Date(ym(paste0(as.character(Year), "-", as.character(Month))))) |>
    drop_na(Date) |> 
    complete(Date = seq.Date(min(Date), max(Date), by = "month")) |> 
    arrange(Date) |>
    mutate(station = y)
}

tg_ts1 <- map2(tgs$`NOAA ID`, tgs$`NOAA Name`, wrangle_NOAA, .progress = T) |> 
  bind_rows()

tg_ts <- bind_rows(tg_ts1, NOAA_Seavey |> filter(Date >= "1992-10-10", Date <= "2024-01-01"))  # this gets produced further down in this script

tg_ts_lm9208 <- tg_ts |> 
  filter(Date <= "2008-06-01") |> 
  group_by(station) |> 
  summarise(
    proportion = sum(!is.na(MSL)) / n(),
    trend = lm(MSL ~ datetime)$coefficients[2]) |> 
  mutate(trend_mmyr = m2mm(trend),
         trend_indec = m2in(trend) * 10) |> # inches per decade
  left_join(tgs, by = c("station" = "NOAA Name")) |> 
  mutate(Lat = if_else(station == "Seavey Island", 43.08, Lat),
         Long = if_else(station == "Seavey Island", -70.74, Long)) |> 
  st_as_sf(coords = c("Long", "Lat"), crs = st_crs(4326))

tg_ts_lm0823 <- tg_ts |> 
  filter(Date > "2008-06-01") |> 
  group_by(station) |> 
  summarise(
    proportion = sum(!is.na(MSL)) / n(),
    trend = lm(MSL ~ datetime)$coefficients[2]) |> 
  mutate(trend_mmyr = m2mm(trend),
         trend_indec = m2in(trend) * 10) |> # inches per decade
  left_join(tgs, by = c("station" = "NOAA Name")) |> 
  mutate(Lat = if_else(station == "Seavey Island", 43.08, Lat),
         Long = if_else(station == "Seavey Island", -70.74, Long)) |> 
  st_as_sf(coords = c("Long", "Lat"), crs = st_crs(4326))

tg_ts_lm9223 <- tg_ts |> 
  filter(Date < "2008-06-01") |> 
  group_by(station) |> 
  summarise(
    proportion = sum(!is.na(MSL)) / n(),
    trend = lm(MSL ~ datetime)$coefficients[2]) |> 
  mutate(trend_mmyr = m2mm(trend),
         trend_indec = m2in(trend) * 10) |> # inches per decade
  left_join(tgs, by = c("station" = "NOAA Name")) |> 
  mutate(Lat = if_else(station == "Seavey Island", 43.08, Lat),
         Long = if_else(station == "Seavey Island", -70.74, Long)) |> 
  st_as_sf(coords = c("Long", "Lat"), crs = st_crs(4326))


## Portland ----

NOAA_Portland <-
  noaa(
    begindate = "19120101",
    enddate = "20241231",
    station = "8418150",
    units = "meters",
    datum = "MSL",
    interval = "monthly",
    time = "GMT"
  ) |> 
  mutate(station = "Portland") |> 
  mutate(Date = as.POSIXct(ym(paste0(as.character(Year), "-", as.character(Month)))))


# NOAA_Portland_daily <- NOAA_Portland |> 
#   group_by(year(time_GMT), month(time_GMT), day(time_GMT)) |> 
#   summarise(daily_MSL = mean(`verified water level at 8418150 (meters rel. to MSL)`)) |> 
#   mutate(Date = as.POSIXct(paste0(as.character(`year(time_GMT)`), "-", 
#                              as.character(`month(time_GMT)`), "-", 
#                              as.character(`day(time_GMT)`)))) |> 
#   ungroup()
# 
# NOAA_Portland_weekly <- NOAA_Portland_daily |> 
#   mutate(fivedayavg = zoo::rollmean(daily_MSL, 5, fill = NA))
# 
# NOAA_Portland_meantrend <- read_csv("COOPS Tide Gauges/8418150_meantrend.csv", skip = 5) |>
#   mutate(Date = as.Date(ym(paste0(as.character(Year), "-", as.character(Month))))) |> 
#   arrange(Date) |> 
#   rename("MSL" = "Monthly_MSL") |> 
#   mutate(station = "Portland") |> 
#   complete(Date = seq.Date(min(Date), max(Date), by = "month")) |> 
#   mutate(Date = as.POSIXct(Date))

## Boston ----

NOAA_Boston <-
  noaa(
    begindate = "19210101",
    enddate = "20241231",
    station = "8443970",
    units = "meters",
    datum = "MSL",
    interval = "monthly",
    time = "GMT"
  ) |> 
  mutate(station = "Boston") |> 
  mutate(Date = as.POSIXct(ym(paste0(as.character(Year), "-", as.character(Month))))) #|> 
  mutate(MSL = zoo::na.approx(MSL))

# NOAA_Boston <- read_csv("COOPS Tide Gauges/CO-OPS__8443970__ml.csv") |>
#   mutate(Date = as.POSIXct(ym(paste0(as.character(Year), "-", as.character(Month))))) |>
#   arrange(Date) |>
#   mutate(station = "Boston")

# Plot TGs ----

NOAA_monthly <- bind_rows(NOAA_Seavey, NOAA_Portland, NOAA_Boston)

NOAA_yearly <- NOAA_monthly |> 
  group_by(station, Year) |> 
  summarise(MSL_m = mean(MSL))

p <- ggplot() +
  geom_line(data = NOAA_monthly, aes(Date, MSL, color = station)) +  
  geom_point(data = NOAA_monthly, aes(Date, MSL, color = station))

ggplotly(p)

p <- ggplot() +
  geom_line(data = NOAA_yearly, aes(Year, MSL_m, color = station)) +  
  geom_point(data = NOAA_yearly, aes(Year, MSL_m, color = station))

ggplotly(p)

# Correlations among tide gauges ----

tg_correlation_9308 <- NOAA_monthly |> 
  select(Date, MSL, station) |> 
  filter(Date > "1992-06-01", Date < "2008-06-01") |> 
  pivot_wider(names_from = station, values_from = MSL) |> 
  select(-Date) |> 
  cor(use = "pairwise.complete.obs")

tg_correlation_0823 <- NOAA_monthly |> 
  select(Date, MSL, station) |> 
  filter(Date >= "2008-06-01", Date < "2023-06-01") |> 
  pivot_wider(names_from = station, values_from = MSL) |> 
  select(-Date) |> 
  cor(use = "pairwise.complete.obs")

tg_correlation_9323 <- NOAA_monthly |> 
  select(Date, MSL, station) |> 
  filter(Date > "1992-06-01", Date < "2023-06-01") |> 
  pivot_wider(names_from = station, values_from = MSL) |> 
  select(-Date) |> 
  cor(use = "pairwise.complete.obs")

NOAA_monthly_blend <- NOAA_monthly |> 
  select(Date, MSL, station) |> 
  pivot_wider(names_from = station, values_from = MSL) |>
  rowwise() |> 
  mutate(MSL_avg = weighted.mean(c(Portland, `Seavey Island`, Boston), 
                                 c(.95, 1, .98),
                                 na.rm = T)) |> 
  ungroup() |> 
  arrange(Date) |> 
  mutate(DateDec = decimal_date(Date)) |> 
  mutate(trend = mgcv::gam(MSL_avg ~ DateDec + poly(DateDec,2))$fitted.values)

NOAA_yearly_blend <- NOAA_monthly_blend |> 
  group_by(year(Date)) |> 
  reframe(MSL_avg = mean(MSL_avg)) |> 
  mutate(Date = as.POSIXct(ym(paste0(`year(Date)`, "-06")))) 
  
# Incorporate Dangendorf's reconstruction for NH ----

recon_nh <- read_csv("Dangendorf_reconstruction_NH.csv") |> 
  rename(station = "Parameter",
         MSL_m = m)

MSL <- bind_rows(NOAA_yearly, recon_nh)

p <- ggplot() +
  geom_line(data = MSL, aes(Year, MSL_m, color = station))

ggplotly(p)

# Linear Trends ----

lm_portland <- lm(MSL ~ index, data = NOAA_Portland |> mutate(index = row_number()))
trend_portland <- lm_portland$coefficients[2] * 1000 * 12
trend_portland

lm_seavey <- lm(MSL ~ index, data = NOAA_Seavey |>
                  # filter(Date > "1992-10-01" & Date <= "2022-12-01") |> 
                  mutate(index = row_number()))
trend_seavey <- lm_seavey$coefficients[2] * 1000 * 12
trend_seavey

lm_boston <- lm(MSL ~ index, data = NOAA_Boston|> mutate(index = row_number()))
trend_boston <- lm_boston$coefficients[2] * 1000 * 12
trend_boston

# Non Linear Trends----

lm_boston_nonlinear <- lm(MSL ~ index + I(index^2), data = NOAA_Boston |>
                            # filter(Date > "1992-10-01" & Date <= "2022-12-01") |> 
                            mutate(index = row_number()))
trend_boston_nonlinear_trend <- lm_boston_nonlinear$coefficients[2] * 1000 * 12
trend_boston_nonlinear_trend

lm_seavey_nonlinear <- lm(MSL ~ index + I(index^2), data = NOAA_Seavey |>
                  # filter(Date > "1992-10-01" & Date <= "2022-12-01") |> 
                  mutate(index = row_number()))
trend_seavey_nonlinear_trend <- lm_seavey_nonlinear$coefficients[2] * 1000 * 12
trend_seavey_nonlinear_trend

# Detrend tide gauge data ----

stl_portland <- NOAA_Portland |> 
  mutate(Date = yearmonth(Date)) |>
  tsibble(index = Date) |>
  model(STL(MSL ~ 
              # trend(window = 11) + 
              # season(period = c(360,223,52.8,12,6)), # 18.6 yrs = 223.2
              # season(period = c(223,52.8,12,6)), # 18.6 yrs = 223.2
              # season(period = c(223.2,12)), # 18.6 yrs = 223.2
              season(period = c(12)), # 18.6 yrs = 223.2
            # season(period = c(52.8,12,6)), # 18.6 yrs = 223.2
            robust = TRUE)) |> 
  components()

autoplot(stl_portland)

stl_boston <- NOAA_Boston |> 
  mutate(Date = yearmonth(Date)) |>
  tsibble(index = Date) |>
  model(STL(MSL ~ 
              # trend(window = 11) + 
              # season(period = c(360,223,52.8,12,6)), # 18.6 yrs = 223.2
              # season(period = c(223,52.8,12,6)), # 18.6 yrs = 223.2
              # season(period = c(223.2,12)), # 18.6 yrs = 223.2
              season(period = c(12)), # 18.6 yrs = 223.2
            # season(period = c(52.8,12,6)), # 18.6 yrs = 223.2
            robust = TRUE)) |> 
  components()

autoplot(stl_boston)

stl_seavey <- NOAA_Seavey |> 
  mutate(MSL = zoo::na.approx(MSL)) |>
  mutate(Date = yearmonth(Date)) |>
  tsibble(index = Date) |>
  model(STL(MSL ~ 
              # trend(window = 11) + 
              # season(period = c(360,223,52.8,12,6)), # 18.6 yrs = 223.2
              # season(period = c(223,52.8,12,6)), # 18.6 yrs = 223.2
              # season(period = c(223.2,12)), # 18.6 yrs = 223.2
              season(period = c(12)), # 18.6 yrs = 223.2
            # season(period = c(52.8,12,6)), # 18.6 yrs = 223.2
            robust = TRUE)) |> 
  components()

autoplot(stl_seavey)

stl_gom <- bind_rows(as_tibble(stl_portland), 
                     as_tibble(stl_seavey), 
                     as_tibble(stl_boston), 
                     .id = "station") |> 
  mutate(station = case_when(station == 1 ~ "Portland",
                             station == 2 ~ "Seavey Island",
                             station == 3 ~ "Boston"))

p <- ggplot(stl_gom, aes(Date, trend, colour = station)) +
  geom_line()

ggplotly(p)

# Model nonlinear trends from deseasoned data ----

trend_start <- "1993-01-01"

lm_boston_deseason_trend_nonlinear <- lm(trend ~ index + I(index^2), data = stl_boston |>
                                           as_tibble() |>
                                           mutate(Date = as.POSIXct(Date)) |> 
                                           filter(Date > trend_start) |>
                                           mutate(index = row_number()))

lm_boston_deseason_trend_nonlinear_acc <- lm_boston_deseason_trend_nonlinear$coefficients[2] * 1000 * 12
lm_boston_deseason_trend_nonlinear_acc

lm_portland_deseason_trend_nonlinear <- lm(trend ~ index + I(index^2), data = stl_portland |>
                                           as_tibble() |>
                                           mutate(Date = as.POSIXct(Date)) |> 
                                           filter(Date > trend_start) |>
                                           mutate(index = row_number()))

lm_seavey_deseason_trend_nonlinear <- lm(trend ~ index + I(index^2), data = stl_seavey |>
                                           as_tibble() |>
                                           mutate(Date = as.POSIXct(Date)) |> 
                                           filter(Date > trend_start) |>
                                           mutate(index = row_number()))

pred_portland <- stl_portland |>
  as_tibble() |>
  mutate(Date = as.POSIXct(Date)) |>
  filter(Date > trend_start) |>
  # mutate(index = row_number()) |>
  mutate(predlm = predict(lm_portland_deseason_trend_nonlinear))

pred_boston <- stl_boston |>
  as_tibble() |>
  mutate(Date = as.POSIXct(Date)) |>
  filter(Date > trend_start) |>
  # mutate(index = row_number()) |>
  mutate(predlm = predict(lm_boston_deseason_trend_nonlinear))

pred_seavey <- stl_seavey |>
  as_tibble() |>
  mutate(Date = as.POSIXct(Date)) |>
  filter(Date > trend_start) |>
  # mutate(index = row_number()) |>
  mutate(predlm = predict(lm_seavey_deseason_trend_nonlinear))

pred_gom <- bind_rows(pred_portland, pred_seavey, pred_boston, .id = "station") |> 
  mutate(station = case_when(station == 1 ~ "Portland",
                             station == 2 ~ "Seavey Island",
                             station == 3 ~ "Boston"))

## Plot nonlinear trends from deseasoned data ----
  
p <- ggplot(pred_gom, aes(Date, predlm)) +
  geom_line(aes(colour = station)) +
  geom_line(data = NASA_POR_OBS |> 
              mutate(Date = as.POSIXct(ym(paste0(as.character(Year), "-01")))), 
            aes(Date, `Obs Extrap. 50th`), color = "lightgreen") +
  geom_line(data = NASA_BOS_OBS |> 
              mutate(Date = as.POSIXct(ym(paste0(as.character(Year), "-01")))), 
            aes(Date, `Obs Extrap. 50th`), color = "pink") +
  labs(title = "Modeled trends from tide gauges")

ggplotly(p)

# Plot deseasoned data ----

p <- stl_portland |>
  as_tsibble() |>
  # mutate(season_adjust = slider::slide_dbl(season_adjust, mean,
  #                                          .before = 2, .after = 2, .complete = TRUE)) |>
  autoplot(MSL, colour = "gray") +
  geom_line(aes(y=season_adjust), colour = "#0072B2") +
  geom_line(data = NOAA_Portland_meantrend, aes(Date, MSL), color = "orange") +
  # geom_line(data = recon_nh, aes(Date, IBAint), color = "green") +
  # geom_line(data = recon_nh, aes(Date, SDSL), color = "lightgreen") +
  geom_line(data = SSHA_portland_monthly_deseason, aes(Date, season_adjust), color = "red") +
  geom_line(data = SSHA_gom_monthly_deseason, aes(Date, season_adjust), color = "pink") +
  labs(y = "Mean Sea Level (m)",
       title = "Relative Sea Level Trend",
       subtitle = "8418150 Portland, Maine")

ggplotly(p, dynamicTicks = T)

# Plot trends ----

pp <- stl_portland |>
  as_tsibble() |>
  # mutate(season_adjust = slider::slide_dbl(season_adjust, mean,
  #                                          .before = 2, .after = 2, .complete = TRUE)) |>
  autoplot(MSL, colour = "gray") +
  geom_line(aes(y=trend), colour = "#0072B2") +
  geom_line(data = NOAA_Portland_meantrend, aes(Date, MSL), color = "orange") +
  # geom_line(data = recon_nh, aes(Date, SL_multi), color = "green") +
  geom_line(data = SSHA_portland_monthly_deseason, aes(Date, trend), color = "red") +
  # geom_ribbon(data = SSHA_portland_monthly, aes(Date, ymin = SLA-SLA_ERR, ymax = SLA+SLA_ERR), color = "red", alpha = 0.5) +
  geom_line(data = SSHA_gom_monthly_deseason, aes(Date, trend), color = "magenta") +
  labs(y = "Mean Sea Level (m)",
       title = "Relative Sea Level Trend",
       subtitle = "8418150 Portland, Maine")

ggplotly(pp, dynamicTicks = T)

# Plot trends and SLR ----

ppp <- stl_portland |>
  as_tsibble() |>
  # mutate(season_adjust = slider::slide_dbl(season_adjust, mean,
  #                                          .before = 2, .after = 2, .complete = TRUE)) |>
  ggplot() +
  geom_line(aes(Date, MSL), colour = "gray") +
  geom_line(data = NASA_BOS_SLR, aes(Date, IntLow_50), color = "lightyellow") +
  geom_line(data = NASA_BOS_SLR, aes(Date, Int_50), color = "lightyellow") +
  geom_line(data = NASA_BOS_SLR, aes(Date, IntHigh_50), color = "lightyellow") +
  geom_line(data = NASA_POR_SLR, aes(Date, IntLow_50), color = "yellow") +
  geom_line(data = NASA_POR_SLR, aes(Date, Int_50), color = "yellow") +
  geom_line(data = NASA_POR_SLR, aes(Date, IntHigh_50), color = "yellow") +
  geom_line(aes(Date, trend), colour = "#0072B2") +
  geom_line(aes(Date, season_adjust), colour = "#0072B2", linetype = "dashed") +
  geom_line(data = NOAA_Portland_meantrend, aes(Date, MSL), color = "orange") +
  geom_ribbon(data = NASA_POR_OBS, aes(x=Date, ymin = `Obs Extrap. 17th`, ymax = `Obs Extrap. 83rd`), fill = "green", alpha = .33) +
  geom_line(data = NASA_POR_OBS, aes(Date, `TG Obs`), color = "green") +
  geom_line(data = NASA_POR_OBS, aes(Date, `Obs Extrap. 50th`), color = "green", linetype = "dashed") +
  geom_line(data = NASA_BOS_OBS, aes(Date, `TG Obs`), color = "darkgreen") +
  geom_line(data = NASA_BOS_OBS, aes(Date, `Obs Extrap. 50th`), color = "darkgreen", linetype = "dashed") +
  geom_line(data = NASA_NE_OBS, aes(Date, `TG Obs`), color = "brown") +
  geom_line(data = NASA_NE_OBS, aes(Date, `Obs Extrap. 50th`), color = "brown", linetype = "dashed") +
  geom_line(data = SSHA_portland_monthly_deseason, aes(Date, trend), color = "red") +
  geom_line(data = SSHA_gom_monthly_deseason, aes(Date, trend), color = "magenta") +
  labs(y = "Mean Sea Level (m)",
       title = "Relative Sea Level Trend",
       subtitle = "8418150 Portland, Maine")

ggplotly(ppp, dynamicTicks = T)
