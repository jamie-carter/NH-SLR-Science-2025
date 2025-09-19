library(tidyverse)
library(plotly)

# https://agupubs.onlinelibrary.wiley.com/doi/full/10.1002/2017ef000663

## Add functions ----

mm2ft <- function(x) (x * 0.00328084)
cm2ft <- function(x) (x * 0.0328084)
m2ft <- function(x) (x * 3.28084)
m2in <- function(x) (x * 39.3701)
m2mm <- function(x) (x * 1000)
mm2m <- function(x) (x / 1000)
cm2m <- function(x) (x * .01)
ft2m <- function(x) (x * 0.3048)

# knames <- c("name",
#             "rcp85_50","rcp85_17--83","rcp85_5--95","rcp85_0.5--99.5","rcp85_99.9",
#             "rcp45_50","rcp45_17--83","rcp45_5--95","rcp45_0.5--99.5","rcp45_99.9",
#             "rcp26_50","rcp26_17--83","rcp26_5--95","rcp26_0.5--99.5","rcp26_99.9")
# kyears <- c("2030", "2050", "2100", "2150", "2200")

k17a <- read_tsv("C:/Users/Jamie.Carter/Documents/aa_Resilience/SLR/2017 - Kopp - SUPPLEMENTAL/eft2271-sup-0004-2017ef000663-ds03.tsv", skip = 1)

k17 <- k17a |> 
  rename(name = `Site Name`, 
         psmsl_id = `Site ID`,
         scenario = Scenario,
         year = Year) |> 
  rename_with(.cols = 5:13, ~ paste0("pctl_", .x)) |> 
  mutate(across(c(5:13), cm2m)) |> 
  mutate(source = "Kopp 2017") |> 
  mutate(scenario = factor(scenario, levels = c("rcp26", "rcp45", "rcp60", "rcp85"),
                           labels = c("RCP-2.6", "RCP-4.5", "RCP-6.0", "RCP-8.5")))

k17_seavey <- k17 |> 
  filter(name == "SEAVEY ISLAND") |> 
  rename(pctl_01 = pctl_0.01,
         pctl_05 = pctl_0.05,
         pctl_17 = pctl_0.167,
         pctl_50 = pctl_0.5,
         pctl_83 = pctl_0.833,
         pctl_95 = pctl_0.95,
         pctl_99 = pctl_0.99,
         pctl_99.5 = pctl_0.995,
         pctl_99.9 = pctl_0.999) |> 
  # dplyr::select(2:4, 7:9, 14)
  dplyr::select(2:14)
  
k17_seavey_all_probs <- k17 |> 
  filter(name == "SEAVEY ISLAND") |>
  pivot_longer(5:13, names_to = "pctl", values_to = "ft") |>
  mutate(ft = m2ft(ft)) |> 
  unite("scenario_combo", c(3,6), remove = F)

pk17 <- ggplot(k17_seavey_all_probs |> filter(year <= 2150), aes(x = year, y = ft, color = scenario_combo)) +
  geom_line() +
  facet_grid(rows = vars(scenario), scales = "free")

ggplotly(pk17, dynamicTicks = T)
