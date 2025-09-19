library(tidyverse)
library(readxl)

## Add functions ----

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
addoffset <- function(x) (x + SLR2022a$`Offset 2000 to 2005 (cm)`)


# Compile VLM rates ----

SLR2022 <- read_csv("C:/Users/Jamie.Carter/Documents/aa_Resilience/SLRTF/Sea_Level_Rise_Datasets_2022/SLR_TF U.S. Sea Level Projections.csv", skip = 17)

PSMSLIDs <- SLR2022 %>%
  dplyr::select(psmsl_id = `PSMSL ID`, name = `PSMSL Site`) %>%
  unique() |>
  mutate(name = str_replace_all(name, "_", " ")) |>
  mutate(name = str_to_title(name)) |>
  mutate(name = str_replace(name, "Ii", "II")) |>
  mutate(name = str_replace(name, "Dc", "DC"))

v2022 <- SLR2022 %>%
  dplyr::select(1:2, 11) %>%
  filter(`PSMSL ID` != 0) %>%
  unique() %>%
  rename(psmsl_id = `PSMSL ID`,
         name = `PSMSL Site`) %>%
  mutate(`vlm_mmyr` = cm2mm(`RSL contribution from VLM (trend: cm/year)`)) |> 
  dplyr::select(2,4) |>
  mutate(source = "2022 Technical Report") |> 
  mutate(vlm_mmyr = vlm_mmyr * -1) |> 
  mutate(approach = "Spatiotemporal statistical model of tide-gauge data")

ar6lof <- list.files("AR6/", pattern = "ipcc_ar6", full.names = TRUE)

ar6_parse <- function(x) {
  read_excel(x, sheet = "VerticalLandMotion") |> 
  pivot_longer(6:19, names_to = "year", values_to = "m") |> 
  pivot_wider(names_from = quantile, values_from = m, names_prefix = "pctl_") |> 
  mutate(year = as.numeric(year)) |> 
  unite("scenario", 4:3, sep = "-") |> 
  filter(scenario == "ssp245-medium") |> 
  summarise(vlm_mmyr = as.numeric(m2mm(lm(pctl_50 ~ year)$coefficients[2])),
            vlm_mmyr_sd = as.numeric(m2mm(((lm(pctl_83 ~ year)$coefficients[2]) - 
                                 lm(pctl_17 ~ year)$coefficients[2]) / 2))) |> 
    mutate(psmsl_id = str_extract(x, "\\d+(?=.xlsx)"), .before = everything(),
           source = "IPCC AR6",
           approach = "Spatiotemporal statistical model of tide-gauge data")
}

ar6v <- ar6lof |> 
  map(ar6_parse, .progress = TRUE) |> 
  bind_rows() |> 
  mutate(vlm_mmyr = vlm_mmyr * -1,
         psmsl_id = as.numeric(psmsl_id))

# vk14 <- k14 |> 
#   dplyr::select(1:4) |> 
#   unique() %>%
#   mutate(source = "Kopp 2014") |> 
#   rename(vlm_mmyr = bkgd_est_mmyr,
#          vlm_mmyr_sd = bkgd_sd_mmyr) |> 
#   mutate(vlm_mmyr = vlm_mmyr * -1) |> 
#   mutate(approach = "Spatiotemporal statistical model of tide-gauge data")

vk17 <- read_tsv("C:/Users/Jamie.Carter/Documents/aa_Resilience/SLR/2017 - Kopp - SUPPLEMENTAL/eft2271-sup-0003-2017ef000663-ds02.tsv", skip = 0) |> 
  dplyr::select(2,5,6) |> 
  rename(psmsl_id = ID,
         vlm_mmyr = `bkgd rate`,
         vlm_mmyr_sd = std) |> 
  mutate(vlm_mmyr = vlm_mmyr * -1) |> 
  mutate(source = "Kopp 2017") |> 
  mutate(approach = "Spatiotemporal statistical model of tide-gauge data")

c18 <- read_csv("GIA/caron_vlm_at_tidegauges.csv", na = "-9999.0000000") |> 
  rename(vlm_mmyr_sd = 4) |> 
  dplyr::select(psmsl_id = 2, 3, 4) |> 
  mutate(source = "Caron 2018") |> 
  mutate(approach = "Glacial isostatic adjustment (GIA) based on GNSS and RSL data")

tre <- read_csv("C:/Users/Jamie.Carter/Documents/a_Project_15/VLM/tre_vlm_at_tidegauges.csv") |> 
  group_by(psmsl_id = PSMSL_ID, 
           name = PSMSL_Site,
           region = Regional_C,
           lat = Lat,
           lon = Long) |> 
  summarise(vlm_mmyr = median(VEL_V),
            vlm_mmyr_sd = 0.3) |> 
  ungroup() |> 
  dplyr::select(1,6,7) |> 
  mutate(source = "NGS/TREA 2024") |> 
  mutate(approach = "InSAR calibrated with GNSS data")

vt <- read_csv("C:/Users/Jamie.Carter/Documents/a_Project_15/VLM/vt_vlm_at_tidegauges.csv") |> 
  group_by(psmsl_id = PSMSL_ID, 
           name = PSMSL_Site,
           region = Regional_C,
           lat = Lat,
           lon = Long) |> 
  summarise(vlm_mmyr = median(vlm_mmyr),
            vlm_mmyr_sd = median((vlm_mmyr_sd))) |> 
  ungroup() |> 
  dplyr::select(1,6,7) |> 
  mutate(source = "USGS/VT 2023") |> 
  mutate(approach = "InSAR calibrated with GNSS data")

h21 <- read_fwf("C:/Users/Jamie.Carter/Documents/aa_Resilience/SLR/2021 - Hammond - SUPPLEMENTAL/2021jb022355-sup-0002-table_si-s01.txt", 
                trim_ws = T, 
                skip = 0) |> 
  drop_na(X2) |> 
  mutate(name = case_when(X1 == ">>" ~ X2)) |> 
  separate_wider_delim(name, " ", names = c("psmsl_id", "name"), too_many = "merge") |> 
  mutate(X4 = case_when(X1 == "##" ~ "tide gauge",
                        .default = X4)) |> 
  fill(c(psmsl_id, name)) |> 
  drop_na(c(X3,X4)) |> 
  mutate(X2 = str_replace_all(X2, "[ ]+", "_")) |>
  separate_wider_delim(X2, "_", names = c("lon", "lat", "vlm_mmyr", "vlm_mmyr_sd", "ssf", "nnvar")) |> 
  rename(tempvar = X3,
         station = X4) |> 
  dplyr::select(-X1) |> 
  filter(station == "tide gauge") |>
  dplyr::select(9,3,4) |>
  mutate(across(1:3, as.numeric)) |> 
  mutate(source = "Hammond 2021") |> 
  mutate(approach = "GNSS imaging")

# write_csv(h21, "C:/Users/Jamie.Carter/Documents/a_Project_15/VLM/Hammond_2021.csv")

#!!! NEED TO GO BACK AND COMPUTE RATE FROM YEARLY DATA !!!#
# d24 <- d24s_psb  |> 
#   mutate(source = "Dangendorf 2024") |> 
#   rename(vlm_mmyr = GIA_Field_KS) |> 
#   mutate(vlm_mmyr = vlm_mmyr * -1)

psmsls_tgs <- c(183, 288, 235) # Portland, Seavey, Boston
psmsls_tgs <- c(360, 234) # Washington, Charleston
psmsls_tgs <- c(234, 161) # Charleston, Galveston
psmsls_tgs <- c(234, 526) # Charleston, Grand Isle
psmsls_tgs <- c(234, 2123) # Charleston, Trident Pier Port Canaverel
psmsls_tgs <- c(234, 1835) # Charleston, Sabine Pass
psmsls_tgs <- c(234, 246) # Charleston, Pensacola

vlm <- bind_rows(v2022, ar6v, vk17, c18, h21, tre, vt) |> 
  left_join(PSMSLIDs) |> 
  filter(psmsl_id %in% psmsls_tgs) |> 
  mutate(id = paste0(name, " (", psmsl_id, ")")) |> 
  mutate(source = factor(source, levels = c("Kopp 2017", "Caron 2018", "Hammond 2021", "IPCC AR6",
                                            "2022 Technical Report", "USGS/VT 2023", "NGS/TREA 2024")))

ggplot(vlm |> filter(psmsl_id == 234), aes(source, vlm_mmyr, colour = approach)) +
  geom_point(size = 2) +
  geom_linerange(aes(source, ymin = vlm_mmyr - vlm_mmyr_sd, ymax = vlm_mmyr + vlm_mmyr_sd)) +
  geom_hline(aes(yintercept = 0), alpha = 0.33) +
  # facet_wrap(~id) + 
  labs(title = "Vertical Land Motion (VLM) Rates",
      subtitle = "Charleston, SC", y = "VLM (mm/yr)", x = NULL) +
  guides(x = guide_axis(angle = 45)) +
  scale_y_continuous(breaks = seq(-10,2,1)) +
  theme(legend.position = "bottom", legend.title = element_blank(),
        legend.direction = "vertical",
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        plot.title=element_text(size=20, face = "bold"),
        plot.subtitle=element_text(size=16),
        legend.text=element_text(size=12)) +
  scale_color_brewer(palette = "Dark2")

ggsave("Comparison of VLM Rates - CHS.png", width = 6, height = 6, units = "in", dpi = 150)        

# VLM concept plot ----

gi <- NOAA_GrandIsle_smooth |> 
  # filter(Date >= "1993-06-01") |> 
  # mutate(Date = Date - months(1)) |> 
  select(Date = Date, MSL_tg = season_adjust) |> 
  left_join(gal_sla_smooth |> select(Date, MSL_alt = season_adjust), 
                by = "Date") |> 
  # filter(Date >= "2017-01-01" & Date < "2023-02-01") |> # NGS/TRE time period
  # filter(Date >= "2000-01-01" & Date < "2010-01-01") |> # High rate of change
  mutate(diff = MSL_tg - MSL_alt) |> 
  group_by(Year = year(Date)) |>
  summarise(MSL_tg = median(MSL_tg), 
            MSL_alt = median(MSL_alt),
            diff_mm = mean(diff)*1000) |> 
  mutate(Date = as.POSIXct(ymd(paste0(Year, "-01-01"))),
         diff_m = diff_mm/1000)

gi_lm <- lm(MSL_tg ~ Year, data=gi)
summary(gi_lm)
gi_lm <- lm(MSL_alt ~ Year, data=gi)
summary(gi_lm)
gi_lm <- lm(diff_mm ~ Year, data=gi)
summary(gi_lm)

cols <- c("Tide Gauge" = "#003087", "Satellite Altimetry" = "#0085CA", "Difference" = "#FF8B00",
          "Difference Trend" = "#FF8B00")

p <- ggplot() +
  geom_line(data=NOAA_GrandIsle_smooth, 
            aes(Date, season_adjust*100+15, color = "Tide Gauge")) +
  geom_line(data=gal_sla_smooth, 
            aes(Date, season_adjust*100, color = "Satellite Altimetry")) +
  # geom_line(data=gi, aes(Date, diff_m*100+16, color = "Difference"), linetype = "dashed") +
  # geom_smooth(data=gal_sla_smooth, aes(Date, season_adjust)) +
  # geom_smooth(data=NOAA_GrandIsle_smooth, aes(Date, season_adjust), fill = "blue") +
  geom_smooth(data=gi, aes(Date, diff_m*100+16, color = "Difference Trend"), se = F, alpha = 0.5) +
  # geom_text(aes(as.POSIXct("2010-01-01"), -2.5, hjust = "left"), 
  #           label = "Difference Trend = 3.9 mm/yr") +
  scale_color_manual(values = cols, name = "Measurement Source", 
                     breaks = c("Tide Gauge", "Satellite Altimetry", "Difference", "Difference Trend")) +
  labs(title = "Sea Level Trends (1992-2022)",
       subtitle = "Grand Isle, LA",
       y = "Relative Sea Level (cm)", x = NULL) +
  theme(legend.position = c(.25,.81),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        plot.title=element_text(size=20, face = "bold"),
        plot.subtitle=element_text(size=16),
        legend.text=element_text(size=12),
        legend.title=element_text(size=14))

p
ggplotly(p)

ggsave("TG and Altimetry SLA at Grand Isle with VLM trend.png", width = 6, height = 6, units = "in", dpi = 150)        
