library(tidyverse)

# https://agupubs.onlinelibrary.wiley.com/doi/10.1002/2014EF000239

## Add functions ----

mm2ft <- function(x) (x * 0.00328084)
cm2ft <- function(x) (x * 0.0328084)
m2ft <- function(x) (x * 3.28084)
m2in <- function(x) (x * 39.3701)
m2mm <- function(x) (x * 1000)
mm2m <- function(x) (x / 1000)
cm2m <- function(x) (x * .01)
ft2m <- function(x) (x * 0.3048)

knames <- c("name",
            "rcp85_50","rcp85_17--83","rcp85_5--95","rcp85_0.5--99.5","rcp85_99.9",
            "rcp45_50","rcp45_17--83","rcp45_5--95","rcp45_0.5--99.5","rcp45_99.9",
            "rcp26_50","rcp26_17--83","rcp26_5--95","rcp26_0.5--99.5","rcp26_99.9")
kyears <- c("2030", "2050", "2100", "2150", "2200")

k14a <- read_tsv("C:/Users/Jamie.Carter/Documents/aa_Resilience/SLR/2014 - Kopp - SUPPLEMENTAL/eft237-sup-0006-table07.tsv", skip = 1)

k14 <- k14a |> 
  filter(!(str_detect(...1, "COASTLINE"))) |> 
  set_names(knames) |> 
  mutate(year = name, .after = name) |> 
  mutate(year = as.numeric(year)) |> 
  mutate(name = case_when(name %in% kyears ~ NA,
                          .default = name)) |> 
  fill(name, .direction = "down") |> 
  drop_na(year) |> 
  separate_wider_delim(name, " [", names = c("name", "psmsl")) |> 
  separate_wider_delim(psmsl, "] (", names = c("psmsl_id", "bkgd_mmyr")) |> 
  mutate(bkgd_est_mmyr = str_extract(bkgd_mmyr, "(?<=Bkgd: )[:graph:]+(?! +/-)"), .after = psmsl_id, .keep = "all") |> 
  mutate(bkgd_sd_mmyr = str_extract(bkgd_mmyr, "(?<=/- )[:graph:]+(?!mm/y)"), .after = bkgd_est_mmyr, .keep = "unused") |> 
  mutate(across(2:4, as.numeric)) |> 
  mutate(across(6:20, as.character)) |> 
  pivot_longer(6:20, names_to = c("scenario", "pctl"), names_sep = "_", values_to = "cm") |> 
  pivot_wider(names_from = "pctl", values_from = "cm") |> 
  mutate(across(8:10, .fns = ~ str_replace(., "--", "_"))) |> 
  separate_wider_delim(8, "_", names = c("17", "83")) |> 
  separate_wider_delim(10, "_", names = c("5", "95")) |> 
  separate_wider_delim(12, "_", names = c("0.5", "99.5")) |> 
  mutate(`99.9` = str_extract(`99.9`, "[:digit:]+")) |> 
  rename_with(.cols = 7:14, ~ paste0("pctl_", .x)) |> 
  mutate(across(c(7:14), as.numeric)) |> 
  mutate(across(c(7:14), cm2m)) |> 
  mutate(source = "Kopp 2014") |> 
  mutate(scenario = factor(scenario, levels = c("rcp26", "rcp45", "rcp85"),
                           labels = c("RCP-2.6", "RCP-4.5", "RCP-8.5")))

k14_seavey <- k14 |> 
  filter(name == "SEAVEY ISLAND") |> 
  mutate(across(7:14, m2ft)) |>
  dplyr::select(psmsl_id, year:source)

k14_seavey_all_probs <- k14 |> 
  filter(name == "SEAVEY ISLAND") |>
  dplyr::select(psmsl_id, year:source) |> 
  pivot_longer(4:11, names_to = "pctl", values_to = "ft") |>
  mutate(ft = m2ft(ft)) |> 
  unite("scenario_combo", c(3,5), remove = F)

pk14 <- ggplot(k14_seavey_all_probs, aes(x = year, y = ft, color = scenario_combo)) +
  geom_line() +
  facet_grid(rows = vars(scenario), scales = "free")

ggplotly(pk14)
