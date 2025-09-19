library(tidyverse)
library(tidync)
library(ggridges)
library(scico)
library(sysfonts)
library(showtext)
library(reticulate)

# font_families_google()
# font_add_google("Source Sans Pro")
# font_add_google("Roboto")
font_add_google("Roboto Condensed")
showtext_auto()
showtext_opts(dpi = 300)

## Add functions ----

mm2ft <- function(x) (x * 0.00328084)
cm2ft <- function(x) (x * 0.0328084)
m2ft <- function(x) (x * 3.28084)
m2in <- function(x) (x * 39.3701)
m2mm <- function(x) (x * 1000)
mm2m <- function(x) (x / 1000)
cm2m <- function(x) (x * .01)
ft2m <- function(x) (x * 0.3048)

# ar6_explore <- tidync("D:/SLR/AR6/ar6-regional-confidence/regional/confidence_output_files/medium_confidence/ssp245/total_ssp245_medium_confidence_values.nc") |> 
#   hyper_tibble() |> 
#   filter(locations == "288")
# 
# ar6_ssp126_lc <- tidync("AR6/ar6/global/full_sample_workflows/wf_1f/ssp585/total-workflow.nc") |> 
#   hyper_tibble() |> 
#   rename(slr = sea_level_change) |> 
#   mutate(slr = mm2m(slr),
#          years = as.numeric(years)) |> 
#   mutate(scenario = case_when(years == 2100 & between(slr, 0.28, .32) ~ "Low",
#                               years == 2100 & between(slr, 0.48, .52) ~ "Intermediate-Low",
#                               years == 2100 & between(slr, 0.98, 1.02) ~ "Intermediate",
#                               years == 2100 & between(slr, 1.48, 1.52) ~ "Intermediate-High",
#                               years == 2100 & between(slr, 1.98, 2.02) ~ "High",
#                               .default = NA)) |> 
#   group_by(samples) |> 
#   fill(scenario, .direction = "updown") |> 
#   mutate(slr = m2ft(slr)) |>
#   # filter(!is.na(scenario)) |> 
#   mutate(ar6_scenario = "ssp585")

# get all data ----
# https://zenodo.org/records/5914710

lof_1e <- list.files("AR6/ar6/global/full_sample_workflows/wf_1e/", full.names = T,
                     pattern = "total", recursive = T) |> 
  str_subset(pattern = "tlim", negate = T)

lof_1f <- list.files("AR6/ar6/global/full_sample_workflows/wf_1f/", full.names = T,
                     pattern = "total", recursive = T) |> 
  str_subset(pattern = "tlim", negate = T)

lof_2e <- list.files("AR6/ar6/global/full_sample_workflows/wf_2e/", full.names = T,
                     pattern = "total", recursive = T) |> 
  str_subset(pattern = "tlim", negate = T)

lof_2f <- list.files("AR6/ar6/global/full_sample_workflows/wf_2f/", full.names = T,
                     pattern = "total", recursive = T) |> 
  str_subset(pattern = "tlim", negate = T)

lof_3e <- list.files("AR6/ar6/global/full_sample_workflows/wf_3e/", full.names = T,
                     pattern = "total", recursive = T) |> 
  str_subset(pattern = "tlim", negate = T)

lof_3f <- list.files("AR6/ar6/global/full_sample_workflows/wf_3f/", full.names = T,
                     pattern = "total", recursive = T) |> 
  str_subset(pattern = "tlim", negate = T)

lof_4 <- list.files("AR6/ar6/global/full_sample_workflows/wf_4/", full.names = T,
                    pattern = "total", recursive = T) |> 
  str_subset(pattern = "tlim", negate = T)

select_scenarios <- function(x) {
  tidync(x) |> 
    hyper_tibble() |> 
    rename(mm = sea_level_change,
           year = years) |> 
    mutate(m = mm2m(mm),
           year = as.numeric(year)) |> 
    mutate(scenario = case_when(year == 2100 & between(m, 0.28, .32) ~ "Low",
                                year == 2100 & between(m, 0.48, .52) ~ "Intermediate-Low",
                                year == 2100 & between(m, 0.98, 1.02) ~ "Intermediate",
                                year == 2100 & between(m, 1.48, 1.52) ~ "Intermediate-High",
                                year == 2100 & between(m, 1.98, 2.02) ~ "High",
                                .default = NA)) |> 
    group_by(samples) |> 
    fill(scenario, .direction = "updown") |> 
    mutate(slr_ft = m2ft(m),
           samples = as.numeric(samples),
           ar6_scenario = str_extract(x, "(?<=AR6/ar6/global/full_sample_workflows/).+(?=/)"),
           uid = paste0(samples, "_", ar6_scenario)) |>
    separate_wider_delim(ar6_scenario, delim = "/", names = c("workflow", "ssp")) |>
    mutate(ssp = as.character(ssp))
}

ar6_1e <- lof_1e |> 
  map(select_scenarios) |> 
  bind_rows()

ar6_1f <- lof_1f |> 
  map(select_scenarios) |> 
  bind_rows()

ar6_2e <- lof_2e |> 
  map(select_scenarios) |> 
  bind_rows()

ar6_2f <- lof_2f |> 
  map(select_scenarios) |> 
  bind_rows()

ar6_3e <- lof_3e |> 
  map(select_scenarios) |> 
  bind_rows()

ar6_3f <- lof_3f |> 
  map(select_scenarios) |> 
  bind_rows()

ar6_4 <- lof_4 |> 
  map(select_scenarios) |> 
  bind_rows()

## canonical IPCC results for projections of levels through 2100
# ar6 <- bind_rows(ar6_1e, ar6_2e, ar6_3e, ar6_4) |> 
#   mutate(ar6 = case_when(workflow == "wf_3e" ~ paste0(ssp, " + LC"),
#                          workflow == "wf_4" ~ paste0(ssp, " + LC"),
#                          .default = ssp))

## parametric fits for post-2100 projections and for projections of rates (used in 2022 report)
ar6 <- bind_rows(ar6_1e, ar6_2e, ar6_3e, ar6_1f, ar6_2f, ar6_3f, ar6_4) |>
  mutate(ar6 = case_when(workflow == "wf_3e" ~ paste0(ssp, " + LC"),
                         workflow == "wf_3f" ~ paste0(ssp, " + LC"),
                         workflow == "wf_4" ~ paste0(ssp, " + LC"),
                         .default = ssp)) |> 
  mutate(scenario = fct_rev(factor(scenario, levels = c("Low", "Intermediate-Low", "Intermediate", "Intermediate-High", "High"),
                                   labels = c("Low", "Int. Low", "Int.", "Int. High", "High"))))

ggplot(ar6 |> filter(!is.na(scenario))) +
  geom_line(aes(year, slr_ft, group = uid, color = scenario)) +
  facet_grid(rows = vars(workflow), cols = vars(ssp)) +
  scale_x_continuous(limits = c(2000,2150)) +
  scale_y_continuous(limits = c(0,20))

ggplot(ar6 |> filter(year == 2100, scenario == "High")) +
  geom_point(aes(samples, m))

ar6 |> filter(year == 2100) |> group_by(scenario) |> summarise(pctl_50 = quantile(m, probs = .5))
ar6 |> filter(year == 2100) |> group_by(scenario) |> summarise(pctl_50 = quantile(slr_ft, probs = .5))

ar6_1e_s <- ar6_1e |> filter(!is.na(scenario)) |> select(samples, scenario, workflow, ssp) |> unique()
ar6_2e_s <- ar6_2e |> filter(!is.na(scenario)) |> select(samples, scenario, workflow, ssp) |> unique()
ar6_3e_s <- ar6_3e |> filter(!is.na(scenario)) |> select(samples, scenario, workflow, ssp) |> unique()
ar6_1f_s <- ar6_1f |> filter(!is.na(scenario)) |> select(samples, scenario, workflow, ssp) |> unique()
ar6_2f_s <- ar6_2f |> filter(!is.na(scenario)) |> select(samples, scenario, workflow, ssp) |> unique()
ar6_3f_s <- ar6_3f |> filter(!is.na(scenario)) |> select(samples, scenario, workflow, ssp) |> unique()
ar6_4_s <- ar6_4 |> filter(!is.na(scenario)) |> select(samples, scenario, workflow, ssp) |> unique()

ar6_s <- bind_rows(ar6_1e_s, ar6_2e_s, ar6_3e_s, ar6_1f_s, ar6_2f_s, ar6_3f_s, ar6_4_s)
ar6_s <- bind_rows(ar6_1f_s, ar6_2f_s, ar6_3f_s, ar6_4_s) # per Kopp et al., 2023 [FACTS]

# ------------------------
# get data for Seavey ----
# https://github.com/Rutgers-ESSP/IPCC-AR6-Sea-Level-Projections?tab=readme-ov-file
# https://github.com/Rutgers-ESSP/IPCC-AR6-Sea-Level-Projections/blob/main/FAQ.md

# py_install(packages = c("xarray", "zarr", "dask", "intake"))
# py_install(packages = c("requests", "aiohttp", "s3fs", "jinja2"))
# py_install("pyarrow==10.0.1")
# py_install("pyarrow")

# Import Python modules
xr <- import("xarray")
# np <- import("numpy")
# dask <- import("dask")
# intake <- import("intake")
# pd <- import("pandas")

# Open a local Zarr dataset
# ds_url <- "https://storage.googleapis.com/ar6-lsl-simulations-public-standard/tide-gauges/full_sample_workflows/wf_4/ssp585/total-workflow.zarr"
# 
# ds = xr$open_dataset(ds_url, engine='zarr', chunks='auto')
# ds

# wf4_585 <- ds['sea_level_change']$sel(locations=288)$to_pandas() |>
#   mutate(samples = row_number() - 1) |>
#   pivot_longer(starts_with("2"), names_to = "year", values_to = "mm") |> 
#   mutate(year = as.numeric(year)) |> 
#   mutate(ssp = "ssp585", workflow = "wf_4", location = "seavey") |> 
#   mutate(uid = paste(workflow, ssp, samples, sep = "_"))
# 
# seavey_wf4_585 <- left_join(wf4_585, ar6_s |> filter(workflow == "wf_4", ssp == "ssp585"), 
#                            by = c("samples", "ssp", "workflow"))
# 
# ggplot(seavey_wf4_585) +
#   geom_line(aes(year, mm2ft(mm), group = samples, color = scenario)) +
#   facet_wrap(~scenario)
# 
# ar6_combined <- seavey_wf4_585 |> 
#   full_join(ar6 |> filter(workflow == "wf_4", ssp == "ssp585"), by = c("samples", "ssp", "workflow", "year")) |> 
#   filter(year == 2100)
# 
# ggplot(ar6_combined) +
#   geom_point(aes(mm.x, mm.y, colour = scenario.x))

# download samples for Seavey Island ----

# wf_ssp <- c("wf_1e/ssp119")
# wf_ssp <- c("wf_1e/ssp119", "wf_1e/ssp126", "wf_1e/ssp245", "wf_1e/ssp370", "wf_1e/ssp585",
#             "wf_2e/ssp119", "wf_2e/ssp126", "wf_2e/ssp245", "wf_2e/ssp370", "wf_2e/ssp585",
#             "wf_3e/ssp126", "wf_3e/ssp245", "wf_3e/ssp585",
#             "wf_4/ssp126", "wf_4/ssp245", "wf_4/ssp585")

wf_ssp <- c("wf_1f/ssp119", "wf_1f/ssp126", "wf_1f/ssp245", "wf_1f/ssp370", "wf_1f/ssp585",
            "wf_2f/ssp119", "wf_2f/ssp126", "wf_2f/ssp245", "wf_2f/ssp370", "wf_2f/ssp585",
            "wf_3f/ssp126", "wf_3f/ssp245", "wf_3f/ssp585",
            "wf_4/ssp126", "wf_4/ssp245", "wf_4/ssp585")

# wf_ssp <- c("wf_1e/ssp119", "wf_1e/ssp126", "wf_1e/ssp245", "wf_1e/ssp370", "wf_1e/ssp585",
#             "wf_2e/ssp119", "wf_2e/ssp126", "wf_2e/ssp245", "wf_2e/ssp370", "wf_2e/ssp585",
#             "wf_3e/ssp126", "wf_3e/ssp245", "wf_3e/ssp585",
#             "wf_1f/ssp119", "wf_1f/ssp126", "wf_1f/ssp245", "wf_1f/ssp370", "wf_1f/ssp585",
#             "wf_2f/ssp119", "wf_2f/ssp126", "wf_2f/ssp245", "wf_2f/ssp370", "wf_2f/ssp585",
#             "wf_3f/ssp126", "wf_3f/ssp245", "wf_3f/ssp585",
#             "wf_4/ssp126", "wf_4/ssp245", "wf_4/ssp585")

# wf_ssp <- c("wf_1f/ssp119", "wf_1f/ssp126", "wf_1f/ssp245", "wf_1f/ssp370", "wf_1f/ssp585",
#             "wf_2f/ssp119", "wf_2f/ssp126", "wf_2f/ssp245", "wf_2f/ssp370", "wf_2f/ssp585",
#             "wf_3f/ssp126", "wf_3f/ssp585",
#             "wf_4/ssp126", "wf_4/ssp585")

loz_fun <- function(wf_ssp) {
  path <- paste0("https://storage.googleapis.com/ar6-lsl-simulations-public-standard/tide-gauges/full_sample_workflows/", wf_ssp, "/total-workflow.zarr")
  wf <- str_extract(wf_ssp, ".+(?=/)")
  ssp <- str_extract(wf_ssp, "(?<=/).+")
  ds = xr$open_dataset(path, engine='zarr', chunks='auto')
  wf_ssp <- ds['sea_level_change']$sel(locations=288)$to_pandas() |>
    mutate(samples = row_number() - 1) |>
    pivot_longer(starts_with("2"), names_to = "year", values_to = "mm") |> 
    mutate(year = as.numeric(year)) |> 
    mutate(ssp = ssp, workflow = wf, location = "seavey") |>
    mutate(uid = paste(workflow, ssp, samples, sep = "_")) |> 
    left_join(ar6_s, by = c("samples", "ssp", "workflow"))
}

seavey_samples <- wf_ssp |> 
  map(loz_fun, .progress = TRUE) |> 
  bind_rows()

# save(seavey_samples, file = "seavey_samples.RData")
# load("seavey_samples.RData")

ggplot(seavey_samples |> filter(!is.na(scenario))) +
  geom_line(aes(year, mm2ft(mm), group = uid, color = scenario)) +
  facet_wrap(~scenario)

seavey_samples_2100 <- seavey_samples |> 
  filter(year == 2100) |> 
  mutate(slr_ft = mm2ft(mm)) |> 
  mutate(scenario = factor(scenario, levels = c("Low", "Intermediate-Low", "Intermediate", "Intermediate-High", "High"),
                           labels = c("Low", "Int. Low", "Int.", "Int. High", "High"))) |> 
  mutate(scenario = fct_rev(scenario)) |>
  mutate(ar6 = case_when(workflow == "wf_3e" ~ paste0(ssp, " + LC"),
                         workflow == "wf_3f" ~ paste0(ssp, " + LC"),
                         workflow == "wf_4" ~ paste0(ssp, " + LC"),
                         .default = ssp)) |> 
  mutate(ar6 = factor(ar6,
                      levels = c("ssp585 + LC", "ssp585", "ssp370", "ssp245 + LC", "ssp245",
                                 "ssp126 + LC", "ssp126", "ssp119"),
                      labels = c("SSP5-8.5, LC", "SSP5-8.5", "SSP3-7.0",
                                 "SSP2-4.5, LC", "SSP2-4.5",
                                 "SSP1-2.6, LC", "SSP1-2.6", "SSP1-1.9"))) |>
  mutate(ar6 = fct_rev(ar6))

ar6_seavey_combined <- seavey_samples_2100 |> 
  filter(year == 2100) |> 
  full_join(ar6 |> filter(year == 2100), by = c("samples", "ssp", "workflow"))

ggplot(ar6_seavey_combined) +
  geom_point(aes(mm.x, mm.y, colour = scenario.x)) +
  facet_grid(rows = vars(workflow), cols = vars(ssp), scales = "free")

# now work on the plots for valid data

seavey_samples_2100 <- seavey_samples_2100 |> 
  filter(!is.na(scenario)) |> 
  filter(workflow %in% c("wf_1f", "wf_2f", "wf_3f", "wf_4"))

ggplot(seavey_samples_2100) +
  geom_density_ridges(aes(slr_ft, ar6, fill = ar6)) +
  ggnewscale::new_scale_fill() +
  geom_rect(data = slr2022_seavey_2100, aes(xmin = slr_ft-.07, xmax = slr_ft+.07, 
                                            ymin = ymin, ymax = ymax, fill = scenario),
            inherit.aes = FALSE) +
  scale_fill_scico_d(palette = "hawaii", begin = .2, end = .8) +
  facet_wrap(~scenario)

slr2022_seavey_2100 <- slr2022_seavey |> 
  filter(year == 2100) |> 
  mutate(scenario = factor(scenario, levels = c("Low", "Intermediate-Low", "Intermediate", "Intermediate-High", "High"),
                           labels = c("Low", "Int. Low", "Int.", "Int. High", "High"))) |> 
  mutate(slr_ft = m2ft(pctl_50),
         scenario = fct_rev(scenario)) |> 
  mutate(ymin = -Inf,
         ymax = Inf)

ggplot(seavey_samples_2100) +
  geom_rect(data = slr2022_seavey_2100, aes(xmin = slr_ft-.07, xmax = slr_ft+.07, 
                                            ymin = ymin, ymax = ymax, fill = scenario),
            inherit.aes = FALSE) +
  scale_fill_scico_d(palette = "hawaii", begin = .2, end = .8) +
  ggnewscale::new_scale_fill() +
  scale_x_continuous(limits = c(-1,10)) + 
  geom_density_ridges(data = ar6, aes(slr_ft, ar6, group = ar6), fill = "lightgray", 
                      rel_min_height = 0.00, alpha = .75) +
  # geom_density_ridges(aes(slr_ft, ar6, group = ar6, fill = ar6), 
  #                     rel_min_height = 0.00, alpha = .9) +
  coord_flip() +
  theme_bw()

seavey_pctl_from_ar6 <- seavey_samples_2100 |> 
  group_by(scenario) |> 
  summarise(pctl_17 = quantile(slr_ft, probs = .17),
            pctl_50 = quantile(slr_ft, probs = .50),
            pctl_83 = quantile(slr_ft, probs = .83))

ggplot(seavey_samples_2100) +
  # geom_rect(data = slr2022_seavey_2100, aes(xmin = m2ft(pctl_17), xmax = m2ft(pctl_83), 
  #                                           ymin = ymin, ymax = ymax, fill = scenario), linewidth = 0, alpha = .5,
  #           inherit.aes = FALSE) +
  # geom_rect(data = slr2022_seavey_2100, aes(xmin = slr_ft-.07, xmax = slr_ft+.07, 
  #                                           ymin = ymin, ymax = ymax, fill = scenario), linewidth = 0, alpha = .5,
  #           inherit.aes = FALSE) +
  scale_fill_scico_d(name = "Scenarios", palette = "hawaii", begin = .2, end = .8, aesthetics = c("color", "fill")) +
  geom_density_ridges(aes(slr_ft, fct_rev(scenario), group = scenario, fill = scenario),
                      rel_min_height = 0.00, alpha = .9) +
  # geom_vline(data = seavey_pctl_from_ar6, aes(xintercept = pctl_50, colour = scenario), linewidth = .75, linetype = "dashed", show_guide = F) +
  # facet_wrap(~workflow) +
  # facet_wrap(~ssp) +
  scale_x_continuous(limits = c(-1,10)) + 
  coord_flip() +
  theme(axis.text=element_text(size=9),
        axis.title=element_text(size=11),
        plot.title=element_text(size=13, face = "bold"),
        plot.subtitle=element_text(size=12),
        legend.text=element_text(size=9)) +
  labs(title = "AR6 Sample Distributions within the 2022 Sea Level Rise Scenarios",
       # subtitle = "Seavey Island (PSMSL #288)",
       x = "Relative Sea Level (feet)", 
       y = NULL) +
  theme_bw() +
  theme(text = element_text(family = "Roboto Condensed", size = 12),
        plot.title = element_text(face = 'bold', family = "Roboto Condensed"),
        plot.subtitle = element_text(face = "italic", family = "Roboto Condensed"))

ggsave("AR6 Sample Distributions for Seavey Island.png", 
       width = 6.5, height = 3, dpi = 300)

# ggsave("AR6 Sample Distributions by Workflow for Seavey Island.png",
#        width = 6, height = 4, dpi = 300)

# ggsave("AR6 Sample Distributions by SSP for Seavey Island.png",
#        width = 6, height = 4, dpi = 300)

# create Figure 2.7 from tech report ----

# for Seavey
seavey_samples_proportions <- seavey_samples_2100 |> 
  mutate(scenario = fct_rev(scenario)) |> 
  mutate(contributions = case_when(ar6 == "SSP1-1.9" ~ "SSP1-1.9, SSP1-2.6",
                                   ar6 == "SSP1-2.6" ~ "SSP1-1.9, SSP1-2.6",
                                   ar6 == "SSP1-2.6, LC" ~ "SSP1-2.6, LC",
                                   ar6 == "SSP2-4.5" ~ "SSP2-4.5",
                                   ar6 == "SSP2-4.5, LC" ~ "SSP2-4.5, LC",
                                   ar6 == "SSP3-7.0" ~ "SSP3-7.0, SSP5-8.5",
                                   ar6 == "SSP5-8.5" ~ "SSP3-7.0, SSP5-8.5",
                                   ar6 == "SSP5-8.5, LC" ~ "SSP5-8.5, LC")) |> 
  mutate(contributions = factor(contributions, levels = c("SSP5-8.5, LC", "SSP3-7.0, SSP5-8.5",
                                                          "SSP2-4.5, LC", "SSP2-4.5",
                                                          "SSP1-2.6, LC", "SSP1-1.9, SSP1-2.6")))

bre.pair.cols <- c("#FB9A99", "#E31A1C", "#B2DF8A", "#33A02C", "#A6CEE3", "#1F8AB4")

p.props <- ggplot(seavey_samples_proportions) +
  geom_bar(aes(scenario, fill = contributions), linewidth = .25,
           position = "fill") +
  geom_text(data = seavey_samples_proportions |> group_by(scenario) |> summarise(count = n()),
            aes(scenario, 1.05, label = prettyNum(count, big.mark = ","))) +
  theme_classic() +
  # scale_fill_brewer(name = "AR6 Projections", palette = "Paired", direction = -1) +
  scale_fill_manual(name = "AR6 Projections", values = bre.pair.cols) +
  scale_y_continuous(labels = scales::label_percent(), expand = c(0, 0), limits = c(0, 1.1)) +
  labs(title = "AR6 Sample Composition of the 2022 Sea Level Rise Scenarios",
       y = "Proportion", 
       x = NULL) +
  theme(text = element_text(family = "Roboto Condensed", size = 12),
        plot.title = element_text(face = 'bold', family = "Roboto Condensed"),
        plot.subtitle = element_text(face = "italic", family = "Roboto Condensed"),
        axis.text=element_text(size=9),
        axis.title=element_text(size=11),
        legend.text=element_text(size=9)) +
  scale_color_scico_d(palette = "hawaii", begin = .2, end = .8)

ggsave(plot = p.props, filename = "AR6 Contributions to Sea Level Rise Scenarios for Seavey Island.png", 
       width = 6.5, height = 3, dpi = 300)

ggplot(seavey_samples_proportions) +
  geom_bar(aes(scenario, fill = contributions), color = "black", position = "stack") +
  theme_bw() +
  scale_fill_brewer(palette = "Paired", direction = -1)
