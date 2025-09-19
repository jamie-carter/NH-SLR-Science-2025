library(tidyverse)
library(sf)

# GIA
gia_names <- c("colatitude", "longitude", "vlm_exp_mmyr", "vlm_std_mmyr", "geoid_exp_mmyr", "geoid_std_mmyr", "gravity_exp_mmyr", "gravity_std_mmyr")
gia_raw <- read_fwf("GIA/GIA_maps_Caron_et_al_2018.txt", skip = 7)
names(gia_raw) <- gia_names

gia <- gia_raw |> 
  mutate(longitude = case_when(longitude > 180 ~ longitude - 360, 
                               .default = longitude),
         latitude = 90 - colatitude) |> 
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326, remove = F)

tmap::qtm(gia, dots.col = "vlm_exp_mmyr")

# library(arcgisbinding)
# arc.check_product()
# arc.write("GIA/Caron_2018.gdb/GIA_Caron_2018",
#           gia, overwrite = TRUE)

# Extract Gulf of Maine points

gom_boundary <- read_sf("NH CFR Science Update/GOM_Boundary.shp")
plot(gom_boundary)

gia_gom <- gia |> 
  st_intersection(gom_boundary)

gia_gom_pts <- gia_gom |> 
  st_drop_geometry()

# Select Portland points
portland_boundary <- read_sf("NH CFR Science Update/Portland_boundary.shp")
plot(portland_boundary)

gia_portland_index <- st_centroid(portland_boundary) |> 
  st_nearest_feature(gia)

gia_portland <- gia[gia_portland_index,]

gia_portland_pts <- gia_portland |> 
  st_drop_geometry() |> 
  select(3,4) |> 
  mutate(psmsl_id = 183)

# Select Seavey points
nh_boundary <- read_sf("NH CFR Science Update/NH_boundary.shp")
plot(nh_boundary)

gia_seavey_index <- st_centroid(nh_boundary) |> 
  st_nearest_feature(gia)

gia_seavey <- gia[gia_seavey_index,]

gia_seavey_pts <- gia_seavey |> 
  st_drop_geometry() |> 
  select(3,4) |> 
  mutate(psmsl_id = 288)

# Select Boston points
boston_boundary <- read_sf("NH CFR Science Update/Boston_boundary.shp")
plot(boston_boundary)

gia_boston_index <- st_centroid(boston_boundary) |> 
  st_nearest_feature(gia)

gia_boston <- gia[gia_boston_index,]

gia_boston_pts <- gia_boston |> 
  st_drop_geometry() |> 
  select(3,4) |> 
  mutate(psmsl_id = 235)

# Combine points

gia_psb <- bind_rows(gia_portland_pts, gia_seavey_pts, gia_boston_pts)
