library(tidyverse)
library(tidync)
library(sf)
library(terra)
library(tidyterra)
library(rnaturalearth)
library(plotly)
library(fpp3)
library(cmocean)
library(patchwork)
library(sysfonts)
library(showtext)
library(gganimate)
library(gifski)
library(ggalt)
library(ggedit)

# font_families_google()
# font_add_google("Source Sans Pro")
# font_add_google("Roboto")
font_add_google("Roboto Condensed")
showtext_auto()
showtext_opts(dpi = 300)

mm2ft <- function(x) (x * 0.00328084)
cm2ft <- function(x) (x * 0.0328084)
m2ft <- function(x) (x * 3.28084)
m2in <- function(x) (x * 39.3701)
m2mm <- function(x) (x * 1000)
m2cm <- function(x) (x * 100)
mm2m <- function(x) (x / 1000)
cm2m <- function(x) (x * .01)
cm2mm <- function(x) (x * 10)
ft2m <- function(x) (x * 0.3048)

# Integrated Multi-Mission Ocean Altimeter Data for Climate Research complete time series Version 5.2 ----
# https://podaac.jpl.nasa.gov/dataset/MERGED_TP_J1_OSTM_OST_ALL_V52
file <- "MERGED_TP_J1_OSTM_OST_ALL_V52_5.2-20241222_143116/Merged_TOPEX_Jason_OSTM_Jason-3_Sentinel-6_Version_V5.2.nc"

TPJAOS <- tidync(file)
print(TPJAOS)

TPJAOS_G1 <- TPJAOS |> 
  hyper_tibble()

TPJAOS_G2 <- TPJAOS |> 
  activate("D2,D3") |> 
  hyper_filter(flag = flag == 0) |> 
  hyper_tibble()

TPJAOS_G3 <- TPJAOS |> 
  activate("D2") |> 
  hyper_tibble()

ggplot(TPJAOS_G1 |> filter(Surface_Type == 0)) +
  geom_point(aes(lon, lat, color = index))

# MEaSUREs Gridded Sea Surface Height Anomalies Version 2205 ----
# https://podaac.jpl.nasa.gov/dataset/SEA_SURFACE_HEIGHT_ALT_GRIDS_L4_2SATS_5DAY_6THDEG_V_JPL2205#
ssha <- tidync("SEA_SURFACE_HEIGHT_ALT_GRIDS_L4_2SATS_5DAY_6THDEG_V_JPL2205_2205-20241223_011322/ssh_grids_v2205_1992101012.nc")
print(ssha)

## Check altimetry data
ssha_global <- ssha |> 
  hyper_tibble()

ssha_global_sf <- ssha_global |> 
  mutate(across(1:4, ~ as.numeric(.x))) |> 
  mutate(Longitude = case_when(Longitude > 180 ~ Longitude - 360, 
                               .default = Longitude)) |> 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = F)

## Map Altimetry points ----

bbox_gomex <- st_as_sfc(st_bbox(c(xmin = -98, xmax = -80, ymax = 31, ymin = 25), crs = 4326))

gomex <- st_intersection(ssha_global_sf, bbox_gomex)

mapview::mapView(gomex)

ssha_global_rast <- ssha_global |> 
  mutate(across(1:4, ~ as.numeric(.x))) |> 
  mutate(Longitude = case_when(Longitude > 180 ~ Longitude - 360, 
                               .default = Longitude)) |> 
  dplyr::select(3,4,1) |> 
  rast(type = "xyz", digits = 3, crs = "epsg:4326")

writeRaster(ssha_global_rast, "ssh_grids_v2205_1992101012.tif")

# library(mapview)
# mapview(ssha_global_rast, xcol = "Longitude", ycol = "Latitude",
#         maxpixels = 1976400)
# tmap::tmap_mode("view")
# tmap::qtm(ssha_global_rast)

## Extract Gulf of Maine points ----

gom_boundary <- read_sf("NH CFR Science Update/GOM_boundary.shp")
plot(gom_boundary)
print(gom_boundary)

# ggplot() +
#   geom_sf(data = gom_boundary) +
#   geom_sf(data = ssha_global_sf, aes(color = SLA))

ssha_gom_sf <- ssha_global_sf |> 
  st_intersection(gom_boundary)

ggplot() +
  geom_sf(data = gom_boundary) +
  geom_sf(data = ssha_gom_sf, aes(color = SLA))

ssha_gom_pts <- ssha_gom_sf |> 
  st_drop_geometry() |> 
  select(3,4)

## Select Portland points ----

portland_boundary <- read_sf("NH CFR Science Update/Portland_boundary.shp")
plot(portland_boundary)

ssha_portland_sf <- ssha_gom_sf |> 
  st_intersection(portland_boundary)

ggplot() +
  geom_sf(data = portland_boundary) +
  geom_sf(data = ssha_portland_sf, aes(color = SLA))

ssha_portland_pts <- ssha_portland_sf |> 
  st_drop_geometry() |> 
  select(3,4)

## Select Seavey points ----

nh_boundary <- read_sf("NH CFR Science Update/NH_boundary.shp")
plot(nh_boundary)

ssha_nh_sf <- ssha_gom_sf |> 
  st_intersection(nh_boundary)

ggplot() +
  geom_sf(data = nh_boundary) +
  geom_sf(data = ssha_nh_sf, aes(color = SLA))

ssha_nh_pts <- ssha_nh_sf |> 
  st_drop_geometry() |> 
  select(3,4)

## Select Boston points ----

boston_boundary <- read_sf("NH CFR Science Update/Boston_boundary.shp")
plot(boston_boundary)

ssha_boston_sf <- ssha_gom_sf |> 
  st_intersection(boston_boundary)

ggplot() +
  geom_sf(data = boston_boundary) +
  geom_sf(data = ssha_boston_sf, aes(color = SLA))

ssha_boston_pts <- ssha_boston_sf |> 
  st_drop_geometry() |> 
  select(3,4)

## Select CZMA points (Portland to Boston) ----

czma_boundary <- read_sf("NH CFR Science Update/CZMA_boundary_PorSeaBos.shp") |> 
  sf::st_union() |> 
  st_buffer(dist = 22000) |> 
  st_transform(crs = st_crs(ssha_gom_sf))
plot(czma_boundary)

ssha_czma_sf <- ssha_gom_sf |> 
  st_intersection(czma_boundary)

ggplot() +
  geom_sf(data = czma_boundary) +
  geom_sf(data = ssha_czma_sf, aes(color = SLA))

ssha_czma_pts <- ssha_czma_sf |> 
  st_drop_geometry() |> 
  select(3,4)

## Gulf of Maine altimetry ----

ssha_gom <- ssha |> 
  hyper_tibble() |> 
  mutate(across(1:4, ~ as.numeric(.x))) |> 
  mutate(Longitude = case_when(Longitude > 180 ~ Longitude - 360, 
                               .default = Longitude)) |> 
  inner_join(ssha_gom_pts, by = c("Longitude", "Latitude")) |> 
  select(c(3,4,1,2))

print(ssha_gom)

ggplot(ssha_gom, aes(Longitude, Latitude, color = SLA)) +
  geom_point()

ssha_gom_r <- rast(ssha_gom, type = "xyz", digits = 3)
plot(ssha_gom_r)

basemap <- ne_countries(scale = 50)
plot(basemap)

gom_map <- ggplot(ssha_gom_r) +
  geom_spatraster(data = ssha_gom_r, aes(fill = SLA)) +
  geom_sf(data = basemap, fill = "white")

gom_map

ggplotly(gom_map)

# Gulf of Maine SSH time-series analysis ----

# lof <- list.files("TEST_SEA_SURFACE_HEIGHT_ALT_GRIDS_L4_2SATS_5DAY_6THDEG_V_JPL2205_2205-20241223_011322", full.names = T)
lof <- list.files("SEA_SURFACE_HEIGHT_ALT_GRIDS_L4_2SATS_5DAY_6THDEG_V_JPL2205_2205-20241223_011322", full.names = T)

extract_gom <- function(x) {
    tidync(x) |>  
    # hyper_filter(Longitude = between(Longitude, 289, 295), Latitude = between(Latitude, 41.6, 45),
    #              Time = between(Time, min(Time), max(Time))) |>
    hyper_tibble() |> 
    mutate(across(1:4, ~ as.numeric(.x))) |> 
    mutate(Longitude = case_when(Longitude > 180 ~ Longitude - 360, 
                                 .default = Longitude)) |> 
    inner_join(ssha_gom_pts, by = c("Longitude", "Latitude")) |> 
    select(c(3,4,1,2))
}

gom <- lof |> 
  set_names() |> 
  map(extract_gom, .progress = "Extracting Gulf of Maine points") |> 
  bind_rows(.id = 'Name') |> 
  mutate(Name = str_extract(Name, "(?<=/).+(?=.nc)")) |> 
  mutate(Date = ymd_h(str_extract(Name, "(?<=2205_).+")))

# GOM raster and time-series animation ----

gom_rast <- gom |> 
  group_by(Longitude, Latitude) |> 
  reframe(SLA = range(SLA)) |> 
  rast(type = "xyz", digits = 3, crs = "EPSG:4326")

gom_rast_timeseries <- gom |> 
  mutate(date1=month(Date), date2=year(Date)) |> 
  group_by(Longitude, Latitude, date1, date2) |> 
  reframe(SLA = mean(SLA)) |> 
  mutate(Date = ym(paste0(date2, "-", date1))) |> 
  arrange(Date) |> 
  mutate(DateLabel = format(Date, "%Y %B"))

gom_ts <- ggplot(gom_rast_timeseries) +
  geom_tile(aes(x=Longitude, y=Latitude, fill=SLA*100, group = Date)) +
  geom_contour(aes(x=Longitude, y=Latitude, z=SLA*100, group = Date), binwidth = 5) +
  geom_sf(data = na) +
  # labs(title = "Year: {frame_time}", fill = "Sea-Level Anomaly (cm)") +
  labs(title = "{gom_rast_timeseries %>% filter(Date == closest_state) %>% pull(DateLabel)}", fill = "Sea-Level Anomaly (cm)") +
  scale_fill_cmocean(name = "balance", na.value = "transparent", limits = c(-60,60)) +
  coord_sf(xlim = c(-71.15,-64), ylim = c(41,45.5)) +
  # transition_time(Date) +
  transition_states(Date, transition_length = 2, state_length = 1) +
  ease_aes('cubic-in-out') +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(legend.title = element_text(angle = 90, hjust = .5),
        legend.title.position = "left",
        legend.key.height = unit(1, "null"),
        axis.title=element_text(size=11),
        axis.text.y = element_blank(),
        plot.title=element_text(size=13, face = "bold"),
        plot.subtitle=element_text(size=12),
        legend.text=element_text(size=9))

gganimate::animate(
  gom_ts,
  # nframes = 361,
  nframes = 363,
  fps = .5,
  width = 1000,
  height = 1000,
  renderer=gifski_renderer("GOM Altimetry.gif")
)

# na <- ne_countries(continent = "North America", scale = "medium")["geometry"]
na <- ne_states(country = c("United States of America", "canada"))["geometry"]
plot(na)

plot(gom_rast)

ggplot() +
  geom_spatraster(data = gom_rast, aes(fill = SLA)) +
  geom_sf(data = na) +
  scale_fill_distiller(palette = "RdBu", na.value = "transparent") +
  coord_sf(xlim = c(-71.15,-64), ylim = c(41,45.5)) +
  theme_void()

# panel of GOM raster and time-series trends (in/dec) ----

altcols <- cmocean("balance", clip = .1)(8)

gom_9208 <- gom |> 
  filter(Date < "2008-06-01") |> 
  group_by(Longitude, Latitude, DateY=year(Date), DateM = month(Date)) |> 
  reframe(SLA = mean(SLA), SLA_ERR = mean(SLA_ERR)) |> 
  mutate(DateDec = decimal_date(ym(paste0(DateY, "-", DateM)))) |> 
  group_by(Longitude, Latitude) |> 
  reframe(trend = lm(SLA ~ DateDec)$coefficients[2]) |> 
  mutate(trend_mmyr = m2mm(trend),
         trend_indec = m2in(trend) * 10) |> # inches per decade
  rast(type = "xyz", digits = 3, crs = "EPSG:4326")

p1 <- ggplot() +
  # geom_spatraster(data = gom_9208, aes(fill=trend_indec), interpolate = T) +
  # geom_spatraster_contour(data = gom_9208, aes(z=trend_indec), breaks = seq(-5,5,1), color = "grey20") +
  geom_spatraster_contour_filled(data = gom_9208, aes(z=trend_indec), breaks = seq(-4,4,1)) +
  geom_sf(data = na) +
  # geom_sf(data = tg_ts_lm9208 |>
  #           mutate(trend_indec = case_when(trend_indec > 4 ~ NA, trend_indec < -4 ~ NA, .default = trend_indec)) |>
  #           mutate(trend_indec = factor(round(trend_indec, 0))),
  #         aes(color = trend_indec), size = 4, show.legend = F) +
  scale_fill_manual(values = altcols, drop = F) +
  geom_sf(data = tg_ts_lm9208, size = 2.4, show.legend = F) +
  geom_sf(data = tg_ts_lm9208,
          aes(color = trend_indec), size = 2, show.legend = F) +
  scale_color_stepsn(colors = altcols, breaks = seq(-4,4,1), na.value = "transparent", limits = c(-4,4),
                    values = scales::rescale(seq(-3.5,3.5,1), from = c(-4,4))) +
  # scale_fill_cmocean(name = "balance", na.value = "transparent", breaks = seq(-5,5,1),
  #                    discrete = T) +
  # scale_fill_cmocean(name = "balance", na.value = "transparent", limits = c(-4.25,4.25),
  #                    breaks = seq(-4,4,1), discrete = F) +
  # scale_fill_stepsn(colors = altcols, breaks = seq(-4,4,1), na.value = "transparent", limits = c(-4,4),
  #                   values = scales::rescale(seq(-3.5,3.5,1), from = c(-4,4))) +
  coord_sf(xlim = c(-71.15,-64), ylim = c(41,45.5)) +
  labs(title = "1993-2008") +
  theme_void(base_family = "Roboto Condensed") +
  labs(fill = "Sea Level Trend (inches/decade)") +
  guides(fill = guide_bins(show.limits = TRUE)) +
  theme(axis.title=element_text(size=9),
         axis.text = element_blank(),
         # legend.title = element_text(angle = 90, hjust = .5),
         # legend.title.position = "left",
         legend.title.position = "bottom",
        legend.title = element_text(hjust = .5),
        legend.position = "bottom",
         # legend.key.width = unit(1, "null"),
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(.25, "cm"),
        legend.axis.line = element_blank(),
        # legend.key.height = unit(1, "null"),
         plot.title=element_text(size=13, face = "bold"),
         plot.subtitle=element_text(size=12),
         legend.text=element_text(size=9))
p1

gom_0823 <- gom |> 
  filter(Date >= "2008-06-01") |> 
  group_by(Longitude, Latitude, DateY=year(Date), DateM = month(Date)) |> 
  reframe(SLA = mean(SLA), SLA_ERR = mean(SLA_ERR)) |> 
  mutate(DateDec = decimal_date(ym(paste0(DateY, "-", DateM)))) |> 
  group_by(Longitude, Latitude) |> 
  reframe(trend = lm(SLA ~ DateDec)$coefficients[2]) |> 
  mutate(trend_mmyr = m2mm(trend),
         trend_indec = m2in(trend) * 10) |> # inches per decade
  mutate(trend_mmyr = if_else(trend_mmyr >= 10, 9.9, trend_mmyr),
         trend_indec = if_else(trend_indec > 4, 4, trend_indec)) |> # forcing little pocket of high SLR values to fit scale
  rast(type = "xyz", digits = 3, crs = "EPSG:4326")

p2 <- ggplot() +
  # geom_spatraster(data = gom_0823, aes(fill = trend_indec), interpolate = T) +
  # geom_spatraster_contour(data = gom_0823, aes(z=trend_indec), breaks = seq(-5,5,1), color = "grey20") +
  geom_spatraster_contour_filled(data = gom_0823, aes(z=trend_indec), breaks = seq(-4,4,1)) +
  geom_sf(data = na) +
  scale_fill_manual(values = altcols, drop = F) +
  geom_sf(data = tg_ts_lm0823, size = 2.4, show.legend = F) +
  geom_sf(data = tg_ts_lm0823,
          aes(color = trend_indec), size = 2, show.legend = F) +
  scale_color_stepsn(colors = altcols, breaks = seq(-4,4,1), na.value = "transparent", limits = c(-4,4),
                     values = scales::rescale(seq(-3.5,3.5,1), from = c(-4,4))) +
  # scale_fill_cmocean(name = "balance", na.value = "transparent", breaks = seq(-5,5,1),
  #                    discrete = T) +
  # scale_fill_cmocean(name = "balance", na.value = "transparent", limits = c(-4.25,4.25),
  #                    breaks = seq(-4,4,1), discrete = F) +
  # scale_fill_stepsn(colors = altcols, breaks = seq(-4,4,1), na.value = "transparent", limits = c(-4,4),
  #                   values = scales::rescale(seq(-3.5,3.5,1), from = c(-4,4))) +
  coord_sf(xlim = c(-71.15,-64), ylim = c(41,45.5)) +
  labs(title = "2008-2023") +
  theme_void(base_family = "Roboto Condensed") +
  labs(fill = "Sea Level Trend (inches/decade)") +
  guides(fill = guide_bins(show.limits = TRUE)) +
  theme(axis.title=element_text(size=9),
        axis.text = element_blank(),
        # legend.title = element_text(angle = 90, hjust = .5),
        # legend.title.position = "left",
        legend.title.position = "bottom",
        legend.title = element_text(hjust = .5),
        legend.position = "bottom",
        # legend.key.width = unit(1, "null"),
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(.25, "cm"),
        legend.axis.line = element_blank(),
        # legend.key.height = unit(1, "null"),
        plot.title=element_text(size=13, face = "bold"),
        plot.subtitle=element_text(size=12),
        plot.margin = unit(c(0,5,0,5), "pt"),
        legend.text=element_text(size=9))
p2

gom_9323 <- gom |> 
  group_by(Longitude, Latitude, DateY=year(Date), DateM = month(Date)) |> 
  reframe(SLA = mean(SLA), SLA_ERR = mean(SLA_ERR)) |> 
  mutate(DateDec = decimal_date(ym(paste0(DateY, "-", DateM)))) |> 
  group_by(Longitude, Latitude) |> 
  reframe(trend = lm(SLA ~ DateDec)$coefficients[2]) |> 
  mutate(trend_mmyr = m2mm(trend),
         trend_indec = m2in(trend) * 10) |> # inches per decade
  rast(type = "xyz", digits = 3, crs = "EPSG:4326")

p3 <- ggplot() +
  # geom_spatraster(data = gom_9323, aes(fill = trend_indec), interpolate = T) +
  # geom_spatraster_contour(data = gom_9323, aes(z=trend_indec), breaks = seq(-5,5,1), color = "grey20") +
  geom_spatraster_contour_filled(data = gom_9323, aes(z=trend_indec), breaks = seq(-4,4,1)) +
  geom_sf(data = na) +
  scale_fill_manual(values = altcols, drop = F) +
  geom_sf(data = tg_ts_lm9223, size = 2.4, show.legend = F) +
  geom_sf(data = tg_ts_lm9223,
          aes(color = trend_indec), size = 2, show.legend = F) +
  scale_color_stepsn(colors = altcols, breaks = seq(-4,4,1), na.value = "transparent", limits = c(-4,4),
                     values = scales::rescale(seq(-3.5,3.5,1), from = c(-4,4))) +
  # scale_fill_cmocean(name = "balance", na.value = "transparent", breaks = seq(-5,5,1),
  #                    discrete = T) +
  # scale_fill_cmocean(name = "balance", na.value = "transparent", limits = c(-4.25,4.25),
  #                    breaks = seq(-4,4,1), discrete = F) +
  # scale_fill_stepsn(colors = altcols, breaks = seq(-4,4,1), na.value = "transparent", limits = c(-4,4),
  #                   values = scales::rescale(seq(-3.5,3.5,1), from = c(-4,4))) +
  coord_sf(xlim = c(-71.15,-64), ylim = c(41,45.5)) +
  labs(title = "1993-2023") +
  theme_void(base_family = "Roboto Condensed") +
  labs(fill = "Sea Level Trend (inches/decade)") +
  guides(fill = guide_bins(show.limits = TRUE)) +
  theme(axis.title=element_text(size=9),
        axis.text = element_blank(),
        # legend.title = element_text(angle = 90, hjust = .5),
        # legend.title.position = "left",
        legend.title.position = "bottom",
        legend.title = element_text(hjust = .5),
        legend.position = "bottom",
        # legend.key.width = unit(1, "null"),
        legend.key.width = unit(1, "cm"),
        legend.key.height = unit(.25, "cm"),
        legend.axis.line = element_blank(),
        # legend.key.height = unit(1, "null"),
        plot.title=element_text(size=13, face = "bold"),
        plot.subtitle=element_text(size=12),
        legend.text=element_text(size=9))
p3

# nh_tg_slgov <- openxlsx::read.xlsx("SeaLeveldotGOV/sealevel_explorer_data_NH.xlsx", sheet = "Observations") |>
#   rename(year = TG.Obs.Year) |>
#   mutate(tg_obs = mm2ft(TG.Obs)) |>
#   select(2, 11) |> 
#   filter(year >= 1993) |> 
#   mutate(tg_obs = slider::slide_dbl(tg_obs, mean, .before = 2, .after = 2, .complete = TRUE))
# 
# nh_alt_slgov <- openxlsx::read.xlsx("SeaLeveldotGOV/sealevel_explorer_data_NH.xlsx", sheet = "Observations") |> 
#   rename(year = Satellite.Altimetry.Year) |> 
#   mutate(alt_obs = mm2ft(Satellite.Altimetry)) |> 
#   select(4,11) |> 
#   mutate(alt_obs = slider::slide_dbl(alt_obs, mean, .before = 2, .after = 2, .complete = TRUE))
# 
# ggplot() +
#   geom_xspline(data = nh_tg_slgov, aes(year, tg_obs), color = "blue") +
#   geom_xspline(data = nh_alt_slgov, aes(year, alt_obs), color = "red") +
#   geom_xspline(data = NOAA_monthly_blend |> 
#                  filter(Date >= "1993-01-01") |> 
#                  mutate(MSL_avg = slider::slide_dbl(MSL_avg, mean, .before = 2, .after = 2, .complete = TRUE), year = decimal_date(Date)), aes(year, m2ft(MSL_avg)), color = "green")

colfunc <- cmocean(name = "balance", clip = .1)
cols <- c("Tide Gauge" = colfunc(256)[30], "Satellite Altimetry" = colfunc(256)[226])

p4 <- ggplot() +
  # geom_smooth(data = SSHA_czma_monthly, method = "gam", alpha = .25, linewidth = .25,
  #              aes(Date, m2ft(SLA), color = "Satellite Altimetry"), key_glyph = "timeseries") +
  # geom_smooth(data = NOAA_monthly_blend, method = "gam", alpha = .25, linewidth = .25,
  #              aes(Date, m2ft(MSL_avg), color = "Tide Gauge"), key_glyph = "timeseries") +
  stat_xspline(data = SSHA_czma_monthly, spline_shape = -.5, size = .5, alpha = .5,
               aes(Date, m2in(trend), color = "Satellite Altimetry"), key_glyph = "timeseries") +
  # stat_xspline(data = NOAA_monthly_blend, spline_shape = -.5, size = .5, alpha = .5,
  #              aes(Date, m2in(trend), color = "Tide Gauge"), key_glyph = "timeseries") +
  stat_xspline(data = SSHA_czma_monthly, spline_shape = -.5, size = .25, alpha = .5,
               aes(Date, m2in(SLA), color = "Satellite Altimetry"), key_glyph = "timeseries") +
  stat_xspline(data = NOAA_monthly_blend, spline_shape = -.5, size = .25, alpha = .5,
               aes(Date, m2in(MSL_avg), color = "Tide Gauge"), key_glyph = "timeseries") +
  stat_xspline(data = SSHA_czma_yearly, spline_shape = -.5, alpha = .75, size = 1,
               aes(Date, m2in(SLA), color = "Satellite Altimetry"), key_glyph = "timeseries") +
  stat_xspline(data = NOAA_yearly_blend, spline_shape = -.5, alpha = .75, size = 1,
               aes(Date, m2in(MSL_avg), color = "Tide Gauge"), key_glyph = "timeseries") +
  scale_color_manual(values = cols) +
  scale_y_continuous(breaks = seq(-6,9,3),
                     sec.axis = sec_axis(transform = ~.x*25.4, name = "Sea Level Change (mm)", breaks = seq(-150,200,50))) +
  scale_x_datetime(breaks = as.POSIXct(seq(as.Date("1995-01-01"), as.Date("2020-01-01"), by = "5 years")),
                   date_minor_breaks = "1 year", date_labels = "%Y",
                   limits = c(as.POSIXct("1993-01-01"), as.POSIXct("2023-01-01")),
                   expand = c(0,0)) +
  theme_bw(base_family = "Roboto Condensed") +
  labs(y = "Sea Level Change (inches)",
       x = NULL,
       title = NULL) +
  theme(legend.title = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(.87,.15),
        axis.title=element_text(size=11),
        plot.title=element_text(size=13, face = "bold"),
        plot.subtitle=element_text(size=12),
        legend.text=element_text(size=9))
p4

# p1234 <- ((p1 | p2 | p3) + plot_layout(guides = "collect") &
#     theme(legend.position='bottom')) / free(p4) + plot_layout(heights = c(2.5,2.5))

p1234 <- wrap_plots((((p1 | p2 | p3) + 
                          plot_layout(guides = "collect") &
                          theme(legend.position='bottom'))), 
                      free(p4), nrow = 2) + plot_layout(heights = c(2.5,2.5))

ggsave(plot=p1234, filename="Altimetry maps and time-series for GOM in per decade.png", 
       width = 6.5, height = 6, dpi = 300)

# panel of GOM raster and time-series trends (mm/yr) ----

altcols_mm <- cmocean("balance", clip = .1)(14)

p1mm <- ggplot() +
  geom_spatraster_contour_filled(data = gom_9208, aes(z=trend_mmyr), breaks = seq(-7,7,1)) +
  # geom_sf(data = czma_boundary, fill = NA, color = "black", linetype = "dashed") +
  # geom_spatraster_contour(data = gom_9208, aes(z=trend_mmyr), color = "white", breaks = seq(-7,7,1)) +
  geom_sf(data = na, color = "gray") +
  geom_spatraster_contour_text(data = gom_9208, aes(z=trend_mmyr, family = "Roboto Condensed"), color = "black",
                               label_placer = isoband::label_placer_minmax(placement = "t", rot_adjuster = isoband::angle_fixed()),
                               breaks = seq(0,2,1)) +
  scale_fill_manual(values = altcols_mm, drop = F) +
  geom_sf(data = tg_ts_lm9208, size = 2.5, show.legend = F) +
  geom_sf(data = tg_ts_lm9208, aes(color = trend_mmyr), size = 2, show.legend = F) +
  geom_sf(data = tg_ts_lm9208 |> filter(proportion < .8), color = "white", size = 2, show.legend = F) +
  geom_sf_label(data = tg_ts_lm9208 |> filter(station %in% c("Boston", "Seavey Island", "Portland")), 
               aes(label = station), size = 2.5, nudge_y = .3, inherit.aes = FALSE) +
  scale_color_stepsn(colors = altcols_mm, breaks = seq(-7,7,1), na.value = "transparent", limits = c(-7,7),
                     values = scales::rescale(seq(-6.5,6.5,1), from = c(-7,7))) +
  coord_sf(xlim = c(-71.5,-65.9), ylim = c(41.25,45), label_axes = ) +
  labs(title = "1993-2008") +
  theme_void(base_family = "Roboto Condensed") +
  labs(fill = "Sea Level Trend (mm/year)") +
  guides(fill = guide_bins(show.limits = TRUE)) +
  theme(axis.title=element_blank(),
        axis.text = element_blank(),
        # legend.title = element_text(angle = 90, hjust = .5),
        # legend.title.position = "left",
        legend.title.position = "bottom",
        legend.title = element_text(hjust = .5),
        legend.position = "bottom",
        # legend.key.width = unit(1, "null"),
        legend.key.width = unit(.5, "cm"),
        legend.key.height = unit(.25, "cm"),
        legend.axis.line = element_blank(),
        # legend.key.height = unit(1, "null"),
        plot.title=element_text(size=13, face = "bold"),
        plot.subtitle=element_text(size=12),
        legend.text=element_text(size=9))
p1mm

p2mm <- ggplot() +
  geom_spatraster_contour_filled(data = gom_0823, aes(z=trend_mmyr), breaks = seq(-7,7,1)) +
  # geom_spatraster_contour(data = gom_0823, aes(z=trend_mmyr), color = "white", breaks = seq(-7,7,1)) +
  geom_sf(data = na, color = "gray") +
  geom_spatraster_contour_text(data = gom_0823, aes(z=trend_mmyr, family = "Roboto Condensed"), color = "white", 
                               label_placer = isoband::label_placer_minmax(placement = "t", rot_adjuster = isoband::angle_fixed()),
                               breaks = seq(-7,7,1)) +
  scale_fill_manual(values = altcols_mm, drop = F) +
  geom_sf(data = tg_ts_lm0823, size = 2.5, show.legend = F) +
  geom_sf(data = tg_ts_lm0823, aes(color = trend_mmyr), size = 2, show.legend = F) +
  geom_sf(data = tg_ts_lm0823 |> filter(proportion < .8), color = "white", size = 2, show.legend = F) +
  # geom_sf_text(data = tg_ts_lm0823 |> filter(station %in% c("Boston", "Seavey Island", "Portland")), 
  #              aes(label = station), size = 3, nudge_y = .2, inherit.aes = FALSE) +
  scale_color_stepsn(colors = altcols_mm, breaks = seq(-7,7,1), na.value = "transparent", limits = c(-7,7),
                     values = scales::rescale(seq(-6.5,6.5,1), from = c(-7,7))) +
  coord_sf(xlim = c(-71.5,-65.9), ylim = c(41.25,45), label_axes = ) +
  labs(title = "2008-2023") +
  theme_void(base_family = "Roboto Condensed") +
  labs(fill = "Sea Level Trend (mm/year)") +
  guides(fill = guide_bins(show.limits = TRUE)) +
  theme(axis.title=element_blank(),
        axis.text = element_blank(),
        # legend.title = element_text(angle = 90, hjust = .5),
        # legend.title.position = "left",
        legend.title.position = "bottom",
        legend.title = element_text(hjust = .5),
        legend.position = "bottom",
        # legend.key.width = unit(1, "null"),
        legend.key.width = unit(.5, "cm"),
        legend.key.height = unit(.25, "cm"),
        legend.axis.line = element_blank(),
        # legend.key.height = unit(1, "null"),
        plot.title=element_text(size=13, face = "bold"),
        plot.subtitle=element_text(size=12),
        plot.margin = unit(c(0,5,0,5), "pt"),
        legend.text=element_text(size=9))
p2mm

p3mm <- ggplot() +
  geom_spatraster_contour_filled(data = gom_9323, aes(z=trend_mmyr), breaks = seq(-7,7,1)) +
  # geom_spatraster_contour(data = gom_9323, aes(z=trend_mmyr), color = "white", breaks = seq(-7,7,1)) +
  geom_sf(data = na, color = "gray") +
  geom_spatraster_contour_text(data = gom_9323, aes(z=trend_mmyr, family = "Roboto Condensed"), color = "black", 
                               label_placer = isoband::label_placer_minmax(placement = "t", rot_adjuster = isoband::angle_fixed()),
                               breaks = seq(-7,7,1)) +
  scale_fill_manual(values = altcols_mm, drop = F) +
  geom_sf(data = tg_ts_lm9223, size = 2.5, show.legend = F) +
  geom_sf(data = tg_ts_lm9223, aes(color = trend_mmyr), size = 2, show.legend = F) +
  geom_sf(data = tg_ts_lm9223 |> filter(proportion < .8), color = "white", size = 2, show.legend = F) +
  # geom_sf_text(data = tg_ts_lm9223 |> filter(station %in% c("Boston", "Seavey Island", "Portland")), 
  #              aes(label = station), size = 3, nudge_y = .2, inherit.aes = FALSE) +
  scale_color_stepsn(colors = altcols_mm, breaks = seq(-7,7,1), na.value = "transparent", limits = c(-7,7),
                     values = scales::rescale(seq(-6.5,6.5,1), from = c(-7,7))) +
  coord_sf(xlim = c(-71.5,-65.9), ylim = c(41.25,45), label_axes = ) +
  labs(title = "1993-2023") +
  theme_void(base_family = "Roboto Condensed") +
  labs(fill = "Sea Level Trend (mm/year)") +
  guides(fill = guide_bins(show.limits = TRUE)) +
  theme(axis.title=element_blank(),
        axis.text = element_blank(),
        # legend.title = element_text(angle = 90, hjust = .5),
        # legend.title.position = "left",
        legend.title.position = "bottom",
        legend.title = element_text(hjust = .5),
        legend.position = "bottom",
        # legend.key.width = unit(1, "null"),
        legend.key.width = unit(.5, "cm"),
        legend.key.height = unit(.25, "cm"),
        legend.axis.line = element_blank(),
        # legend.key.height = unit(1, "null"),
        plot.title=element_text(size=13, face = "bold"),
        plot.subtitle=element_text(size=12),
        legend.text=element_text(size=9))
p3mm

p4mm <- ggplot() +
  stat_xspline(data = SSHA_czma_monthly, spline_shape = -.5, size = .5,
               aes(Date, trend, color = "Satellite Altimetry"), key_glyph = "timeseries") +
  stat_xspline(data = SSHA_czma_monthly, spline_shape = -.5, size = .25, alpha = 0.5,
               aes(Date, SLA, color = "Satellite Altimetry"), key_glyph = "timeseries") +
  stat_xspline(data = NOAA_monthly_blend, spline_shape = -.5, size = .25, alpha = 0.5,
               aes(Date, MSL_avg, color = "Tide Gauge"), key_glyph = "timeseries") +
  stat_xspline(data = SSHA_czma_yearly, spline_shape = -.5, alpha = .75, size = 1,
               aes(Date, SLA, color = "Satellite Altimetry"), key_glyph = "timeseries") +
  stat_xspline(data = NOAA_yearly_blend, spline_shape = -.5, alpha = .75, size = 1,
               aes(Date, MSL_avg, color = "Tide Gauge"), key_glyph = "timeseries") +
  scale_color_manual(values = cols) +
  scale_y_continuous(sec.axis = sec_axis(transform = ~.x*100, name = "Sea Level Change (cm)", breaks = seq(-50,50,5))) +
  scale_x_datetime(breaks = as.POSIXct(seq(as.Date("1995-01-01"), as.Date("2020-01-01"), by = "5 years")),
                   date_minor_breaks = "1 year", date_labels = "%Y",
                   limits = c(as.POSIXct("1993-01-01"), as.POSIXct("2023-01-01")),
                   expand = c(0,0)) +
  theme_bw(base_family = "Roboto Condensed") +
  labs(y = "Sea Level Change (meters)",
       x = NULL,
       title = NULL) +
  theme(legend.title = element_blank(),
        legend.position = "inside",
        legend.position.inside = c(.87,.15),
        axis.title=element_text(size=11),
        plot.title=element_text(size=13, face = "bold"),
        plot.subtitle=element_text(size=12),
        legend.text=element_text(size=9))
p4mm

# use this for just metric (this is not the figure used in the paper)

p1234mm <- wrap_plots((((p1mm | p2mm | p3mm) + 
                          plot_layout(guides = "collect") &
                          theme(legend.position='bottom'))), 
                      free(p4mm), nrow = 2) + plot_layout(heights = c(2.5,2.5))

ggsave(plot=p1234mm, filename="Altimetry maps and time-series for GOM mm per year.png", 
       width = 6.5, height = 6, dpi = 300)

# use this for final figure combining maps in mm/yr with time series in inches and mm

p1234mm_ftin <- wrap_plots((((p1mm | p2mm | p3mm) + 
                          plot_layout(guides = "collect") &
                          theme(legend.position='bottom'))), 
                      free(p4), nrow = 2) + plot_layout(heights = c(2.5,2.5))

ggsave(plot=p1234mm_ftin, filename="Altimetry maps and time-series for GOM mm per year and ft in.png", 
       width = 6.5, height = 6, dpi = 300)

# GOM time-series ----

gom_analysis <- gom |> 
  group_by(Date) |> 
  summarise(SLA = mean(SLA), SLA_ERR = mean(SLA_ERR))

gom_monthly <- gom |> 
  group_by(year(Date), month(Date)) |> 
  summarise(SLA = mean(SLA), SLA_ERR = mean(SLA_ERR)) |> 
  ungroup() |> 
  mutate(Date = as.POSIXct(ym(paste0(`year(Date)`, "-", `month(Date)`)))) # |> 
  filter(Date > "1992-10-01" & Date < "2022-12-01")

ggplot() +
  geom_line(data = gom_analysis, aes(Date, SLA)) +
  geom_smooth(data = gom_analysis, aes(Date, SLA))

## Portland SSH time-series ----

SSHA_portland <- gom |> 
  inner_join(ssha_portland_pts, by = c("Longitude", "Latitude"))

SSHA_portland_monthly <- SSHA_portland |> 
  group_by(year(Date), month(Date)) |> 
  summarise(SLA = mean(SLA), SLA_ERR = mean(SLA_ERR)) |> 
  ungroup() |> 
  mutate(Date = as.POSIXct(ym(paste0(`year(Date)`, "-", `month(Date)`)))) |> 
  # filter(Date > "1992-10-01" & Date < "2022-12-01") |> 
  mutate(SLA = SLA + ((mm2m(0.28)/12) * (row_number() - 1))) # adding VLM to make relative SLR

lm_portland_alt <- lm(SLA ~ index, data = SSHA_portland_monthly |> mutate(index = row_number()))
trend_portland_alt <- lm_portland_alt$coefficients[2] * 1000 * 12
trend_portland_alt

## Seavey SSH time-series ----

SSHA_seavey <- gom |> 
  inner_join(ssha_nh_pts, by = c("Longitude", "Latitude"))

SSHA_seavey_monthly <- SSHA_seavey |> 
  group_by(year(Date), month(Date)) |> 
  summarise(SLA = mean(SLA), SLA_ERR = mean(SLA_ERR)) |> 
  ungroup() |> 
  mutate(Date = as.POSIXct(ym(paste0(`year(Date)`, "-", `month(Date)`)))) |> 
  # filter(Date > "1992-10-01" & Date < "2022-12-01") |> 
  mutate(SLA = SLA + ((mm2m(0.06)/12) * (row_number() - 1))) # adding VLM to make relative SLR

lm_seavey_alt <- lm(SLA ~ index, data = SSHA_seavey_monthly |> mutate(index = row_number()))
trend_seavey_alt <- lm_seavey_alt$coefficients[2] * 1000 * 12
trend_seavey_alt

## Boston SSH time-series ----

SSHA_boston <- gom |> 
  inner_join(ssha_boston_pts, by = c("Longitude", "Latitude"))

SSHA_boston_monthly <- SSHA_boston |> 
  group_by(year(Date), month(Date)) |> 
  summarise(SLA = mean(SLA), SLA_ERR = mean(SLA_ERR)) |> 
  ungroup() |> 
  mutate(Date = as.POSIXct(ym(paste0(`year(Date)`, "-", `month(Date)`)))) |> 
  # filter(Date > "1992-10-01" & Date < "2022-12-01") |> 
  mutate(SLA = SLA + ((mm2m(1.09)/12) * (row_number() - 1))) # adding VLM to make relative SLR

lm_boston_alt <- lm(SLA ~ index, data = SSHA_boston_monthly |> mutate(index = row_number()))
trend_boston_alt <- lm_boston_alt$coefficients[2] * 1000 * 12
trend_boston_alt

## CZMA SSH time-series ----

SSHA_czma <- gom |> 
  inner_join(ssha_czma_pts, by = c("Longitude", "Latitude"))

SSHA_czma_monthly <- SSHA_czma |> 
  dplyr::group_by(year(Date), month(Date)) |> 
  reframe(SLA = mean(SLA), SLA_ERR = mean(SLA_ERR)) |> 
  mutate(Date = as.POSIXct(ym(paste0(`year(Date)`, "-", `month(Date)`)))) |> 
  mutate(DateDec = decimal_date(Date)) |> 
  mutate(trend = mgcv::gam(SLA ~ DateDec + poly(DateDec,2))$fitted.values)
mutate(trend = lm(SLA ~ DateDec + I(DateDec^2))$fitted.values) #|> mutate(SLA = SLA + ((mm2m(1.09)/12) * (row_number() - 1))) # adding VLM to make relative SLR

SSHA_czma_yearly <- SSHA_czma |> 
  dplyr::group_by(year(Date)) |> 
  reframe(SLA = mean(SLA), SLA_ERR = mean(SLA_ERR)) |> 
  mutate(Date = as.POSIXct(ym(paste0(`year(Date)`, "-06"))))  

lm_czma_alt <- lm(SLA ~ index, data = SSHA_czma_monthly |> mutate(index = row_number()))
trend_czma_alt <- lm_czma_alt$coefficients[2] * 1000 * 12
trend_czma_alt

# Global SSH time-series analysis ----

lof <- list.files("TEST_SEA_SURFACE_HEIGHT_ALT_GRIDS_L4_2SATS_5DAY_6THDEG_V_JPL2205_2205-20241223_011322", full.names = T)
lof <- list.files("SEA_SURFACE_HEIGHT_ALT_GRIDS_L4_2SATS_5DAY_6THDEG_V_JPL2205_2205-20241223_011322", full.names = T)

extract_global <- function(x) {
  tidync(x) |>  
    hyper_tibble() |> 
    summarize(SLA = mean(SLA))
}

global_analysis <- lof |> 
  set_names() |> 
  map(extract_global, .progress = "Compiling global SSH points") |> 
  bind_rows(.id = 'Name') |> 
  mutate(Name = str_extract(Name, "(?<=/).+(?=.nc)")) |> 
  mutate(Date = ymd_h(str_extract(Name, "(?<=2205_).+")))

global_monthly <- global_analysis |> 
  group_by(year(Date), month(Date)) |> 
  summarise(SLA = mean(SLA)) |> 
  ungroup() |> 
  rename(Year = "year(Date)", Month = "month(Date)") |> 
  mutate(Date = as.POSIXct(ym(paste0(as.character(Year), "-", as.character(Month)))))

## Galveston SSH time-series ----

lof <- list.files("TEST_SEA_SURFACE_HEIGHT_ALT_GRIDS_L4_2SATS_5DAY_6THDEG_V_JPL2205_2205-20241223_011322", full.names = T)
lof <- list.files("SEA_SURFACE_HEIGHT_ALT_GRIDS_L4_2SATS_5DAY_6THDEG_V_JPL2205_2205-20241223_011322", full.names = T)

extract_sla <- function(x) {
  tidync(x) |>  
    hyper_filter(Longitude = between(Longitude, -91.2+360, -89.8+360), Latitude = Latitude == 29.25) |> # Proximity of Grand Isle
    # hyper_filter(Longitude = between(Longitude, -94.8+360, -94.7+360), Latitude = Latitude == 29.25) |> # Proximity of Galveston Pier 21
    # hyper_filter(Longitude = between(Longitude, -93.8+360, -93.7+360), 
    #              Latitude = between(Latitude, 29.4, 29.45)) |> # Proximity of Sabine Pass
    hyper_tibble()
}  

gal_sla <- lof |> 
  set_names() |> 
  map(extract_sla, .progress = "Extracting SLA points") |> 
  bind_rows(.id = 'Name') |> 
  mutate(Name = str_extract(Name, "(?<=/).+(?=.nc)")) |> 
  mutate(Date = ymd_h(str_extract(Name, "(?<=2205_).+")))

gal_sla_smooth <- gal_sla |> 
  mutate(Date = yearmonth(Date)) |>
  group_by(Date) |>
  summarize(SLA = mean(SLA)) |>
  ungroup() |>
  tsibble(index = Date) |>
  model(STL(SLA ~ 
              # trend(window = 11) + 
              season(period = c(12, 6)), # 18.6 yrs = 223.2
            robust = TRUE)) |> 
  components() |> 
  mutate(season_adjust = slider::slide_dbl(season_adjust, mean, .before = 8, .after = 8, .complete = TRUE)) |> 
  as_tibble() |> 
  mutate(Date = as.POSIXct(Date, format = "%Y %U"))

gal <- ggplot(gal_sla_smooth, aes(Date, season_adjust)) +
  geom_line(color = "blue")

ggplotly(gal)

# Global Mean Sea Level Trend from Integrated Multi-Mission Ocean Altimeters TOPEX/Poseidon, Jason-1, OSTM/Jason-2, Jason-3, and Sentinel-6 Version 5.2 ----
# https://podaac.jpl.nasa.gov/dataset/MERGED_TP_J1_OSTM_OST_GMSL_ASCII_V52#
# Values in mm

GMSL_TPJAOS_5.2 <- read_fwf("Global Mean Sea Level Trend from Integrated Multi-Mission Ocean Altimeters/GMSL_TPJAOS_5.2.txt",
                              skip = 52)

column_names <- c(
  'altimeter_type',
  'cycle', 
  'year',
  'observation_count', 
  'observation_count_weighted',
  'gmsl_variation',
  'gmsl_variation_std',
  'gmsl_variation_smooth',
  'gmsl_variation_with_gia',
  'gmsl_variation_with_gia_std',
  'gmsl_variation_with_gia_smooth',
  'gmsl_variation_with_gia_smooth_and_signals_removed',
  'gmsl_variation_without_gia_smooth_and_signals_removed')

names(GMSL_TPJAOS_5.2) <- column_names

GMSL_TPJAOS_5.2 <- GMSL_TPJAOS_5.2 |> 
  mutate(Date = as.POSIXct(format(date_decimal(year), "%Y-%m-%d")))

g <- ggplot() +
  geom_line(data = GMSL_TPJAOS_5.2, aes(Date, gmsl_variation/1000), color = "red") +
  geom_line(data = GMSL_TPJAOS_5.2, aes(Date, gmsl_variation_smooth/1000 + .01), color = "pink") +
  geom_line(data = GMSL_TPJAOS_5.2, aes(Date, gmsl_variation_with_gia_smooth/1000 + .02), color = "orange") +
  geom_line(data = GMSL_TPJAOS_5.2, aes(Date, gmsl_variation_with_gia_smooth_and_signals_removed/1000 + .03), color = "blue") +
  geom_line(data = GMSL_TPJAOS_5.2, aes(Date, gmsl_variation_without_gia_smooth_and_signals_removed/1000 + .04), color = "green")

ggplotly(g, dynamicTicks = TRUE)

g <- ggplot() +
  geom_line(data = GMSL_TPJAOS_5.2, aes(Date, gmsl_variation_with_gia_smooth_and_signals_removed/1000 + .03), color = "blue") +
  geom_line(data = GMSL_TPJAOS_5.2, aes(Date, gmsl_variation_without_gia_smooth_and_signals_removed/1000 + .04), color = "green") +
  geom_smooth(data = GMSL_TPJAOS_5.2, aes(Date, gmsl_variation_with_gia_smooth_and_signals_removed/1000 + .03), 
              color = "blue",
              formula = y ~ x + I(x^2)) +
  geom_smooth(data = GMSL_TPJAOS_5.2, aes(Date, gmsl_variation_without_gia_smooth_and_signals_removed/1000 + .04), 
              color = "green",
              formula = y ~ x + I(x^2))

ggplotly(g, dynamicTicks = TRUE)

GMSL_TPJAOS_5.2_annual <- GMSL_TPJAOS_5.2 |> 
  # group_by(year(Date)) |> 
  group_by(as.integer(year)) |> 
  summarise(gmsl_variation_with_gia_smooth_and_signals_removed = mean(gmsl_variation_with_gia_smooth_and_signals_removed),
            gmsl_variation_without_gia_smooth_and_signals_removed = mean(gmsl_variation_without_gia_smooth_and_signals_removed)) |> 
  # rename(Year = "year(Date)") |> 
  rename(Year = "as.integer(year)")

# Ben H used the `gmsl_variation_with_gia_smooth_and_signals_removed` variable in his 2024 paper
fit_with_gia_linear <- lm(gmsl_variation_with_gia_smooth_and_signals_removed ~ Year, data = GMSL_TPJAOS_5.2_annual)
summary(fit_with_gia_linear)
fit_with_gia_nonlinear <- lm(gmsl_variation_with_gia_smooth_and_signals_removed ~ Year + I(Year^2), data = GMSL_TPJAOS_5.2_annual)
summary(fit_with_gia_nonlinear)

fit_without_gia_linear <- lm(gmsl_variation_without_gia_smooth_and_signals_removed ~ Year, data = GMSL_TPJAOS_5.2_annual)
summary(fit_without_gia_linear)
fit_without_gia_nonlinear <- lm(gmsl_variation_without_gia_smooth_and_signals_removed ~ Year + I(Year^2), data = GMSL_TPJAOS_5.2_annual)
summary(fit_without_gia_nonlinear)

new <- tibble(Year = c(1993, 1994, 2023, 2024))

pred_1992 <- predict(fit_with_gia_nonlinear, new)

## Plot global and regional ----

p <- ggplot() +
  geom_line(data = global_analysis, aes(Date, SLA)) +
  # geom_line(data = gom_analysis, aes(Date, SLA), color = "lightblue") +
  geom_line(data = GMSL_TPJAOS_5.2, aes(Date, gmsl_variation_with_gia_smooth_and_signals_removed/1000), color = "red")
  # geom_smooth(data = global_analysis, aes(Date, SLA), alpha = .5, method = "lm") +
  # geom_smooth(data = gom_analysis, aes(Date, SLA), color = "lightblue", method = "lm")

ggplotly(p, dynamicTicks = TRUE)

gom_glob <- left_join(global_analysis, gom_analysis, by = "Date")

ggplot(gom_glob, aes(SLA.x, SLA.y)) +
  geom_point()

## Detrend and deseason global altimetry ----

stl_global <- global_monthly |> 
  mutate(Date = yearmonth(Date)) |>
  tsibble(index = Date) |>
  model(STL(SLA ~ 
              trend(window = 11) + 
              season(period = c(12,6)), # 18.6 yrs = 223.2
            robust = TRUE)) |> 
  components()

stl_global <- global_analysis |> 
  mutate(Date = yearweek(Date)) |>
  group_by(Date) |> 
  summarize(SLA = mean(SLA)) |> 
  ungroup() |> 
  tsibble(index = Date) |>
  model(STL(SLA ~ 
              # trend(window = 11) + 
            season(period = c(52, 26)), # 18.6 yrs = 223.2
            robust = TRUE)) |> 
  components()

autoplot(stl_global)

stl_global |>
  as_tsibble() |>
  mutate(season_adjust = slider::slide_dbl(season_adjust, mean,
                               .before = 4, .after = 4, .complete = TRUE)) |>
  autoplot(SLA, colour = "gray") +
  geom_line(aes(y=season_adjust), colour = "#0072B2") +
  geom_line(data = GMSL_TPJAOS_5.2, aes(Date, gmsl_variation_with_gia_smooth_and_signals_removed/1000 + .03), color = "blue") +
  geom_smooth(data = GMSL_TPJAOS_5.2, aes(Date, gmsl_variation_with_gia_smooth_and_signals_removed/1000 + .03), 
              color = "blue",
              formula = y ~ x + I(x^2)) + 
  labs(y = "Sea Level Anomaly",
       title = "MEaSUREs Gridded Sea Surface Height Anomalies Version 2205")

## Check fit for seasonally adjusted data ----

stl_global_yr <- stl_global |> 
  as_tibble() |> 
  mutate(Date = as.POSIXct(Date)) |> 
  group_by(year(Date)) |> 
  summarise(season_adjust = mean(season_adjust)*1000) |> 
  ungroup() |> 
  rename(Date = `year(Date)`)

fit <- lm(season_adjust ~ Date, data = stl_global_yr)
summary(fit) # looks like the MEaSURES 2205 data don't have the GIA adjustment...

g <- ggplot() +
  geom_line(data = stl_global, aes(Date, season_adjust), color = "blue") +
  geom_line(data = GMSL_TPJAOS_5.2, aes(Date, gmsl_variation_without_gia_smooth_and_signals_removed/1000 + .04), color = "green") +
  geom_smooth(data = stl_global, aes(Date, season_adjust), 
              color = "blue",
              formula = y ~ x + I(x^2)) +
  geom_smooth(data = GMSL_TPJAOS_5.2, aes(Date, gmsl_variation_without_gia_smooth_and_signals_removed/1000 + .04), 
              color = "green",
              formula = y ~ x + I(x^2))

ggplotly(g, dynamicTicks = TRUE)

# de-season local altimetry data ----

SSHA_portland_monthly_deseason <- SSHA_portland_monthly |> 
  mutate(Date = yearmonth(Date)) |>
  tsibble(index = Date) |>
  model(STL(SLA ~ 
              # trend(window = 11) + 
              # season(period = c(52.8,12,6)), # 18.6 yrs = 223.2
              # season(period = c(223.2,12)), # 18.6 yrs = 223.2
              season(period = c(12,6)), # 18.6 yrs = 223.2
            # season(period = c(12)), # 18.6 yrs = 223.2
            robust = TRUE)) |> 
  components() |> 
  mutate(season_adjust = slider::slide_dbl(season_adjust, mean,
                                           .before = 2, .after = 2, .complete = TRUE))

autoplot(SSHA_portland_monthly_deseason)

SSHA_boston_monthly_deseason <- SSHA_boston_monthly |> 
  mutate(Date = yearmonth(Date)) |>
  tsibble(index = Date) |>
  model(STL(SLA ~ 
              # trend(window = 11) + 
              # season(period = c(52.8,12,6)), # 18.6 yrs = 223.2
              # season(period = c(223.2,12)), # 18.6 yrs = 223.2
              # season(period = c(12)), # 18.6 yrs = 223.2
              season(period = c(12,6)), # 18.6 yrs = 223.2
            robust = TRUE)) |> 
  components() |> 
  mutate(season_adjust = slider::slide_dbl(season_adjust, mean,
                                           .before = 2, .after = 2, .complete = TRUE))

autoplot(SSHA_boston_monthly_deseason)

SSHA_seavey_monthly_deseason <- SSHA_seavey_monthly |> 
  mutate(Date = yearmonth(Date)) |>
  tsibble(index = Date) |>
  model(STL(SLA ~ 
              # trend(window = 11) + 
              # season(period = c(52.8,12,6)), # 18.6 yrs = 223.2
              # season(period = c(223.2,12)), # 18.6 yrs = 223.2
              # season(period = c(12)), # 18.6 yrs = 223.2
              season(period = c(12,6)), # 18.6 yrs = 223.2
              robust = TRUE)) |> 
  components() |> 
  mutate(season_adjust = slider::slide_dbl(season_adjust, mean,
                                           .before = 2, .after = 2, .complete = TRUE))

autoplot(SSHA_seavey_monthly_deseason)

SSHA_gom_monthly_deseason <- gom_monthly |> 
  mutate(Date = yearmonth(Date)) |>
  tsibble(index = Date) |>
  model(STL(SLA ~ 
              # trend(window = 11) + 
              # season(period = c(52.8,12,6)), # 18.6 yrs = 223.2
              # season(period = c(223.2,12)), # 18.6 yrs = 223.2
              # season(period = c(12)), # 18.6 yrs = 223.2
              season(period = c(12,6)), # 18.6 yrs = 223.2
            robust = TRUE)) |> 
  components() |> 
  mutate(season_adjust = slider::slide_dbl(season_adjust, mean,
                                           .before = 2, .after = 2, .complete = TRUE))

autoplot(SSHA_gom_monthly_deseason)

## Compute linear trend ----

lm_seavey_alt_linear <- lm(
  season_adjust ~ index,
  data = SSHA_seavey_monthly_deseason |>
    as_tibble() |>
    mutate(Date = as.POSIXct(Date)) |>
    filter(Date < "2020-01-01") |>
    mutate(index = decimal_date(Date))
)

lm_seavey_alt_linear

lm_gom_alt_linear <- lm(
  season_adjust ~ index,
  data = SSHA_gom_monthly_deseason |>
    as_tibble() |>
    mutate(Date = as.POSIXct(Date)) |>
    filter(Date > "1970-01-01") |>
    mutate(index = decimal_date(Date))
)

lm_gom_alt_linear

## Compute nonlinear trend ----

lm_portland_alt_nl <- lm(
  season_adjust ~ index + I(index^2),
  data = SSHA_portland_monthly_deseason |>
    as_tibble() |>
    mutate(Date = as.POSIXct(Date)) |>
    filter(Date > "1970-01-01") |>
    mutate(index = decimal_date(Date))
)

lm_seavey_alt_nl <- lm(
  season_adjust ~ index + I(index^2),
  data = SSHA_seavey_monthly_deseason |>
    as_tibble() |>
    mutate(Date = as.POSIXct(Date)) |>
    filter(Date < "2020-01-01") |>
    mutate(index = decimal_date(Date))
)

lm_boston_alt_nl <- lm(
  season_adjust ~ index + I(index^2),
  data = SSHA_boston_monthly_deseason |>
    as_tibble() |>
    mutate(Date = as.POSIXct(Date)) |>
    filter(Date > "1970-01-01") |>
    mutate(index = decimal_date(Date))
)

lm_gom_alt_nl <- lm(
  season_adjust ~ index + I(index^2),
  data = SSHA_gom_monthly_deseason |>
    as_tibble() |>
    mutate(Date = as.POSIXct(Date)) |>
    filter(Date > "1970-01-01") |>
    mutate(index = decimal_date(Date))
)

# compute TRENDS ----

pred_portland_alt_nld <- SSHA_portland_monthly_deseason |>
  as_tibble() |>
  mutate(Date = as.POSIXct(Date)) |>
  filter(Date > "1970-01-01") |>
  mutate(predlm = predict(lm_portland_alt_nl))

pred_seavey_alt_nld <- SSHA_seavey_monthly_deseason |>
  as_tibble() |>
  mutate(Date = as.POSIXct(Date)) |>
  filter(Date > "1970-01-01") |>
  mutate(predlm = predict(lm_seavey_alt_nl))

pred_boston_alt_nld <- SSHA_boston_monthly_deseason |>
  as_tibble() |>
  mutate(Date = as.POSIXct(Date)) |>
  filter(Date > "1970-01-01") |>
  mutate(predlm = predict(lm_boston_alt_nl))

pred_gom_alt_nld <- SSHA_gom_monthly_deseason |>
  as_tibble() |>
  mutate(Date = as.POSIXct(Date)) |>
  filter(Date > "1970-01-01") |>
  mutate(predlm = predict(lm_gom_alt_nl))

trends <- bind_rows(pred_portland_alt_nld, pred_boston_alt_nld, 
                    pred_seavey_alt_nld, pred_gom_alt_nld,
                    .id = "station") |> 
  mutate(station = case_when(station == 1 ~ "Portland",
                             station == 2 ~ "Seavey Island",
                             station == 3 ~ "Boston",
                             station == 4 ~ "Gulf of Maine"))

# create EXTRAPOLATIONS ----

newdata <- tibble(Date = as.POSIXct(seq(as.Date("1992-10-01"), by = "month", length.out = 688)),
                  # index = seq(1,688,1),
                  index = decimal_date(Date))

extrapolation_portland <- newdata |> 
  mutate(pred = predict(lm_portland_alt_nl, newdata = newdata)) |> 
  mutate(type = case_when(Date < "2022-12-01" ~ "Trend",
                          .default = "Extrapolation"))

extrapolation_boston <- newdata |> 
  mutate(pred = predict(lm_boston_alt_nl, newdata = newdata)) |> 
  mutate(type = case_when(Date < "2022-12-01" ~ "Trend",
                          .default = "Extrapolation"))

extrapolation_seavey <- newdata |> 
  mutate(pred = predict(lm_seavey_alt_nl, newdata = newdata)) |> 
  mutate(type = case_when(Date < "2022-12-01" ~ "Trend",
                          .default = "Extrapolation"))

extrapolation_gom <- newdata |> 
  mutate(pred = predict(lm_gom_alt_nl, newdata = newdata)) |> 
  mutate(type = case_when(Date < "2022-12-01" ~ "Trend",
                          .default = "Extrapolation"))

extrapolation <- bind_rows(extrapolation_portland, extrapolation_seavey, 
                           extrapolation_boston, extrapolation_gom,
                          .id = "station") |> 
  mutate(station = case_when(station == 1 ~ "Portland",
                             station == 2 ~ "Seavey Island",
                             station == 3 ~ "Boston",
                             station == 4 ~ "Gulf of Maine"))  


## Plots altimetry trends ----

p <- ggplot(extrapolation, aes(Date, pred)) +
  geom_line(data = NASA_POR_OBS |>
              mutate(Date = as.POSIXct(ym(paste0(as.character(Year), "-01")))),
            aes(Date, `Obs Extrap. 50th`), color = "lightblue") +
  geom_line(data = NASA_BOS_OBS |>
              mutate(Date = as.POSIXct(ym(paste0(as.character(Year), "-01")))),
            aes(Date, `Obs Extrap. 50th`), color = "pink") +
  geom_line(aes(color = station, linetype = type)) +
  geom_line(data = SSHA_seavey_monthly_deseason |> 
              as_tibble() |> 
              mutate(Date = as.POSIXct(Date)), 
            aes(Date, season_adjust)) +
  geom_line(data = SSHA_seavey_monthly_deseason |> 
              as_tibble() |> 
              mutate(Date = as.POSIXct(Date)), 
            aes(Date, trend), color = "red") +
  labs(title = "Modeled trends from satellite altimetry (adjusted for VLM)")

ggplotly(p, dynamicTicks = T)

# Altimetry and 2022 Scenarios ----
scenario_selector <- slr2022_seavey |> 
  mutate(across(pctl_50:pctl_83, m2ft),
         scenario = fct_rev(scenario)) |> 
  ggplot() +
  geom_ribbon(aes(x = year, ymin = pctl_17, ymax = pctl_83, fill = scenario), alpha = 0.2) +
  geom_xspline(spline_shape = -0.5, aes(year, pctl_50, color = scenario), size = .75, alpha = .8) +
  scale_color_scico_d(palette = "hawaii", direction = 1, 
                      aesthetics = c("color", "fill")) +
  ggnewscale::new_scale_color() +
  geom_line(data = extrapolation |> filter(month(Date) == 1) |> 
              mutate(pred = m2ft(pred),
                     Year = year(Date)),             
            aes(x=Year, y=pred, 
            color = station), size = 2, alpha = .8, key_glyph = "timeseries") +
  geom_xspline(data = NASA_NE_OBS %>% filter(Year>=2020), spline_shape = -0.5, aes(Year, m2ft(`Obs Extrap. 50th`), size = "Observation Extrapolation"),
               linetype = "dashed", color = "black", alpha = .8) +
  # geom_ribbon(data = NASA_NE_OBS %>% filter(Year>=2020), aes(Year, ymin = m2ft(`Obs Extrap. 17th`), ymax = m2ft(`Obs Extrap. 83rd`), size = "Observation Extrapolation"), fill = "black", alpha = .2) +
  scale_y_continuous(breaks = seq(-2, 12, 1),
                     sec.axis = sec_axis(~.*0.3048, breaks = seq(-1,5,.5), name = "Sea level (m)")) +
  scale_x_continuous(breaks = c(1970,2000,2020,2050,2100), minor_breaks = seq(1970,2100,10)) +
  coord_cartesian(xlim = c(1992, 2050), ylim = c(-0.5, 2), expand = FALSE) +
  # scale_linewidth_manual(labels = c("Tide Gauge Observations"),
  #                   values = c(.75)) +
  # scale_size_manual(limits = c("Tide Gauge Observations", "Observation Extrapolation"),
  #                   values = c(0.75, 0.5)) +
  labs(
    title = "Satellite Altimetry and 2022 Sea Level Rise Scenarios",
    subtitle = "Seavey Island Scenarios",
    size = "Observations",
    color = "Scenarios",
    fill = "Scenarios",
    x = NULL,
    y = "Relative Sea Level (ft)") +
  theme_bw() +
  theme(text = element_text(family = "Roboto Condensed", size = 12),
        plot.title = element_text(face = 'bold', family = "Roboto"),
        plot.subtitle = element_text(face = "italic", family = "Roboto")
  )

scenario_selector
ggplotly(scenario_selector)
