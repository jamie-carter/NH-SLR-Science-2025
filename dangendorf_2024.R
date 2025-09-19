library(tidyverse)
library(sf)
library(openxlsx)

d24r <- read_csv("Dangendorf_reconstruction_NH.csv")
d24rse <- read_csv("Dangendorf_reconstruction_se_NH.csv")
nh_obs <- read.xlsx("SeaLeveldotGOV/sealevel_explorer_data_NH.xlsx", sheet = "Observations") |>
  rename(Year = TG.Obs.Year) |> 
  mutate(TG_obs = mm2ft(TG.Obs)) |> 
  select(2,11) |> 
  mutate(avg5yr = slider::slide_dbl(TG_obs, mean, .before = 2, .after = 2, .complete = TRUE))

d24s <- st_as_sf(d24r, coords = c("Lon", "Lat"), crs = 4326, remove = F)

# Extract Gulf of Maine points

gom_boundary <- read_sf("NH CFR Science Update/GOM_Boundary.shp")
plot(gom_boundary)

d24s_gom <- d24s |> 
  st_intersection(gom_boundary)

d24s_gom_pts <- d24s_gom |> 
  st_drop_geometry()

# Select Portland points
portland_boundary <- read_sf("NH CFR Science Update/Portland_boundary.shp")
plot(portland_boundary)

d24s_portland_index <- st_centroid(portland_boundary) |> 
  st_nearest_feature(d24s)

d24s_portland <- d24s[d24s_portland_index,]

d24s_portland_pts <- d24s_portland |> 
  st_drop_geometry() |> 
  select(4) |> 
  mutate(psmsl_id = 183)

# Select Seavey points
nh_boundary <- read_sf("NH CFR Science Update/NH_boundary.shp")
plot(nh_boundary)

d24s_seavey_index <- st_centroid(nh_boundary) |> 
  st_nearest_feature(d24s)

d24s_seavey <- d24s[d24s_seavey_index,]

d24s_seavey_pts <- d24s_seavey |> 
  st_drop_geometry() |> 
  select(4) |> 
  mutate(psmsl_id = 288)

# Select Boston points
boston_boundary <- read_sf("NH CFR Science Update/Boston_boundary.shp")
plot(boston_boundary)

d24s_boston_index <- st_centroid(boston_boundary) |> 
  st_nearest_feature(d24s)

d24s_boston <- d24s[d24s_boston_index,]

d24s_boston_pts <- d24s_boston |> 
  st_drop_geometry() |> 
  select(4) |> 
  mutate(psmsl_id = 235)

# Combine points

d24s_psb <- bind_rows(d24s_portland_pts, d24s_seavey_pts, d24s_boston_pts)

# plot Dangendorf reconstruction ----

dangendorf <- d24r |> 
  select(4:10) |> 
  rename(Sterodynamic = "SDSL", Barystatic = "BarySL", GIA = "GIA_Field_KS", `Inverted Barometer` = "IBAint", SL_multi = "SL_multi") |> 
  pivot_longer(cols = c(2:7), names_to = "Process", values_to = "amount") |> 
  left_join(d24rse) |> 
  mutate(amount = m2ft(amount)) |>
  mutate(m_se = m2ft(m_se)) |>
  rowwise() |> 
  mutate(lower = amount - (0.5 * m_se),
         upper = amount + (0.5 * m_se)) |> 
  # filter(! Process %in% c("GIA")) |> 
  mutate(Process = factor(Process, levels = c("Sterodynamic", "Barystatic", "GIA", "Inverted Barometer", "SL_multi", "Reconstruction")))

p.dangendorf <- ggplot(dangendorf) +
  # geom_line(data = processes |> filter(process == "Observed GMSL"), aes(year, median), color = "black", linetype = "dotted") +
  geom_line(data = nh_obs, aes(Year, avg5yr), color = "black", linetype = "dotted") +
  geom_ribbon(aes(Year, ymin = lower, ymax = upper, fill = Process), alpha=.25) +
  geom_line(aes(Year, amount, color = Process), linewidth = 1) +
  scale_y_continuous(breaks = seq(-2, 2, .5),
                     sec.axis = sec_axis(~.*12, breaks = seq(-12,12,1), name = "Relative Sea Level (inches)")) +
  scale_x_continuous(breaks = seq(1880,2020,20)) +
  coord_cartesian(xlim = c(1900, 2021), expand = FALSE) +
  # scale_color_cmocean(name = "thermal", direction = -1, discrete = TRUE) +
  # scale_fill_cmocean(name = "thermal", direction = -1, discrete = TRUE) +
  # scale_color_scico_d(palette = "glasgow", aesthetics = c("color", "fill"), direction = -1) +
  # scale_color_brewer(palette = "PRGn") +
  scale_color_viridis_d(direction = -1, aesthetics = c("colour", "fill")) +
  labs(
    # title = "Historic Sea-Level Rise",
    # color = NULL,
    x = NULL,
    y = "Relative Sea Level (feet)") +
  theme_bw() +
  theme(text = element_text(family = "Roboto Condensed", size = 12),
        plot.title = element_text(face = 'bold'),
        plot.subtitle = element_text(face = "italic"),
        legend.key.width = unit(1, "cm"),
        # legend.position = "bottom",
        legend.title = element_blank()
  )

p.dangendorf

ggsave(plot = p.dangendorf,
       filename = "Reconstruction for New Hampshire.png", 
       width = 6.5, height = 3, dpi = 300, units = "in")

