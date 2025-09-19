library(tidyverse)
library(sf)
library(basemaps)
library(sysfonts)
library(showtext)
library(scico)
library(cmocean)
library(terra)
library(tidyterra)
library(patchwork)
library(ggspatial)

# Add fonts ----
font_add_google("Roboto")
font_add_google("Roboto Condensed")
showtext_auto()
showtext_opts(dpi = 300)

# NH tide gauges
tgs <- tribble(~name, ~lon, ~lat,
               "Hampton", -70.8185224, 42.9001094,
               "Seavey Island", -70.7410940, 43.0797549,
               # "Fort Point", -70.7106842, 43.0716508
               ) |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

# bounding box
bb <- tgs |>
  st_buffer(dist = 18000) |> 
  st_bbox() |> 
  st_as_sfc()

# VLM data
trevlm <- read_sf("C:/Users/Jamie.Carter/Documents/a_Project_15/VLM/VLM data/NOAA_SNT_A_VERT_12_50m.shp") |> 
  st_intersection(bb) |> 
  arrange(abs(VEL_V))

# COLORS ----

# vlmcols <- cmocean("curl", direction = -1)(10) # red to green
vlmcols <- cmocean("balance", direction = -1, clip = .1)(10) # red to blue

# ----

## run flush_cache() if map doesn't plot
flush_cache()

# spatial extent
ext <- tribble(~name, ~lon, ~lat,
               "NH", -70.75, 43.000) |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |> 
  st_buffer(dist = 16000) |> 
  st_bbox()

# ext <- tgs |>
#   filter(name == "Hampton") |> 
#   st_buffer(dist = 5000) |> 
#   st_bbox()

# bm <- basemap_terra(ext = ext, map_service = "esri", map_type = "world_ocean_base")
# bm <- basemap_terra(ext = ext, map_service = "esri", map_type = "world_shaded_relief")
bm <- basemap_terra(ext = ext, map_service = "esri", map_type = "world_light_gray_base")
# bm <- basemap_terra(ext = ext, map_service = "carto", map_type = "light_no_labels")
# bm <- basemap_terra(ext = ext, map_service = "carto", map_type = "voyager_no_labels")
plot(bm)
# bmref <- basemap_terra(ext = ext, map_service = "esri", map_type = "world_street_map")
# bmref <- basemap_terra(ext = ext, map_service = "carto", map_type = "light_only_labels")
# bmref <- basemap_terra(ext = ext, map_service = "carto", map_type = "voyager_only_labels")
# bmref <- basemap_terra(ext = ext, map_service = "esri", map_type = "world_dark_gray_reference")
bmref <- basemap_terra(ext = ext, map_service = "esri", map_type = "world_light_gray_reference")
# bmref <- basemap_terra(ext = ext, map_service = "esri", map_type = "world_boundaries_and_places")
plot(bmref)

pNH <- ggplot(trevlm) +
  # basemap_gglayer(ext = ext, map_service = "esri", map_type = "world_ocean_base", force = TRUE) +
  geom_spatraster_rgb(data = bm) +
  geom_sf(data = trevlm, aes(color = VEL_V), size = .75, shape = 16) + # shape = 15 for squares
  geom_spatraster_rgb(data = bmref) +
  geom_sf(data = tgs, fill = "yellow", shape = 23, size = 3, show.legend = FALSE) +
  geom_sf_label(data = tgs, aes(label = name), nudge_x = -.04, nudge_y = .01, 
                  size = 4, show.legend = FALSE) +
  # geom_sf_text(data = cora_sf_wm, aes(label = name), check_overlap = TRUE, size = 8/.pt) +
  coord_sf(expand = FALSE, xlim = c(ext[1], ext[3]), ylim = c(ext[2], ext[4])) +
  # scale_color_brewer(palette = "Set1") +
  # scale_color_manual(values = vlmcols, drop = F) +
  scale_color_stepsn(colors = vlmcols, breaks = seq(-4,4,1), na.value = "transparent", limits = c(-4,4),
                     values = scales::rescale(seq(-4,4,1), from = c(-4,4))) +
  scale_fill_stepsn(colors = vlmcols, breaks = seq(-4,4,1), na.value = "transparent", limits = c(-4,4),
                     values = scales::rescale(seq(-4,4,1), from = c(-4,4))) +
  # scale_fill_identity() +
  annotation_scale(location = "tr", pad_y = unit(2, "inches"), unit_category = "imperial") +
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_minimal) +
  # labs(title = "Hampton") +
  theme_void(base_family = "Roboto Condensed") +
  labs(fill = "Vertical Land Motion (mm/year)") +
  labs(color = "Vertical Land Motion (mm/year)") +
  guides(fill = guide_bins(show.limits = TRUE)) +
  theme(axis.title=element_blank(),
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
pNH

# spatial extent
extS <- tribble(~name, ~lon, ~lat,
               "SHEA", -70.8065, 42.915) |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326) |> 
  st_buffer(dist = 2500) |> 
  st_bbox()

# extS <- tgs |>
#   filter(name == "Seavey Island") |> 
#   st_buffer(dist = 2000) |> 
#   st_bbox()

# bmS <- basemap_terra(ext = extS, map_service = "esri", map_type = "world_ocean_base")
# bmS <- basemap_terra(ext = extS, map_service = "carto", map_type = "voyager_no_labels")
bmS <- basemap_terra(ext = extS, map_service = "esri", map_type = "world_light_gray_base")
plot(bmS)
# bmrefS <- basemap_terra(ext = extS, map_service = "esri", map_type = "world_reference_overlay")
bmrefS <- basemap_terra(ext = extS, map_service = "esri", map_type = "world_light_gray_reference")
# bmrefS <- basemap_terra(ext = extS, map_service = "carto", map_type = "voyager_only_labels")
# plot(bmref)

pS <- ggplot(trevlm) +
  geom_spatraster_rgb(data = bmS) +
  geom_sf(data = trevlm, aes(color = VEL_V), size = 1, shape = 16) +
  # geom_spatraster_rgb(data = bmrefS) +
  geom_sf(data = tgs, fill = "yellow", shape = 23, size = 3, show.legend = FALSE) +
  geom_sf_label(data = tgs, aes(label = name), nudge_x = -.01, nudge_y = .002, 
                size = 3.5, show.legend = FALSE) +
  # geom_sf_text(data = cora_sf_wm, aes(label = name), check_overlap = TRUE, size = 8/.pt) +
  coord_sf(expand = FALSE, xlim = c(extS[1], extS[3]), ylim = c(extS[2], extS[4])) +
  # scale_color_brewer(palette = "Set1") +
  # scale_color_manual(values = vlmcols, drop = F) +
  scale_color_stepsn(colors = vlmcols, breaks = seq(-4,4,1), na.value = "transparent", limits = c(-4,4),
                     values = scales::rescale(seq(-4,4,1), from = c(-4,4))) +
  scale_fill_stepsn(colors = vlmcols, breaks = seq(-4,4,1), na.value = "transparent", limits = c(-4,4),
                    values = scales::rescale(seq(-4,4,1), from = c(-4,4))) +
  # scale_fill_identity() +
  annotation_scale(location = "br", unit_category = "imperial") +
  annotation_north_arrow(location = "tr", which_north = "true", style = north_arrow_minimal) +
  # labs(title = "Seavey Island") +
  theme_void(base_family = "Roboto Condensed") +
  labs(fill = "Vertical Land Motion (mm/year)") +
  labs(color = "Vertical Land Motion (mm/year)") +
  guides(fill = guide_bins(show.limits = TRUE)) +
  theme(axis.title=element_blank(),
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
        panel.border = element_rect(color = "darkgray", fill = NA),
        plot.title=element_text(size=13, face = "bold"),
        plot.subtitle=element_text(size=12),
        legend.text=element_text(size=9))
pS

# pNHpS <- (pS + theme(plot.margin = unit(c(0,5,0,0), "pt"))) + 
#   (pNH + theme(plot.margin = unit(c(0,0,0,5), "pt"))) +
#   plot_layout(guides = "collect") &
#   theme(legend.position='bottom')

pNHpS <- pNH + inset_element(pS, left = .55, bottom = 0, right = 1, top = .45) +
  plot_layout(guides = "collect") & theme(legend.position='bottom')

pNHpS

ggsave(plot = pNHpS,
       filename = "VLM map.png", 
       width = 6.5, height = 6.5, dpi = 300, units = "in")
