library(tidyverse)
library(readxl)
library(gt)
library(gtExtras)
library(cmocean)
library(flextable)
library(officer)

mm2ft <- function(x) (x * 0.00328084)
cm2ft <- function(x) (x * 0.0328084)
m2ft <- function(x) (x * 3.28084)
m2in <- function(x) (x * 39.3701)
m2mm <- function(x) (x * 1000)
mm2m <- function(x) (x / 1000)
cm2m <- function(x) (x * .01)
ft2m <- function(x) (x * 0.3048)

ar6_seavey <- read_excel("AR6/ipcc_ar6_sea_level_projection_psmsl_id_288.xlsx",
                   sheet = "Total") |> 
  pivot_longer(6:19, names_to = "year", values_to = "m") |> 
  pivot_wider(names_from = quantile, values_from = m, names_prefix = "pctl_") |> 
  mutate(year = as.numeric(year)) |> 
  unite("scenario", 4:3, sep = "-") |> 
  mutate(scenario = factor(scenario, levels = c("ssp119-medium", "ssp126-low", "ssp126-medium", 
                                                "ssp245-medium", "ssp370-medium", 
                                                "ssp585-medium", "ssp585-low"),
                           labels = c("SSP1-1.9", "SSP1-2.6 + LC", "SSP1-2.6", "SSP2-4.5", 
                                      "SSP3-7.0", "SSP5-8.5", "SSP5-8.5 + LC"))) |> 
  mutate(source = "IPCC AR6") |> 
  dplyr::select(-process) |> 
  rename(pctl_05 = "pctl_5") |> 
  mutate(across(4:8, ~ .x)) #  + 0.02 adjustment to year 2000

ar6_seavey |> 
  filter(year %in% c(2030, 2050, 2070, 2100)) |> 
  ggplot(aes(x = scenario)) +
  geom_linerange(aes(ymin = pctl_17, ymax = pctl_83)) +
  geom_point(aes(y = pctl_50)) +
  facet_grid(rows = vars(year), scales = "free")

ar6_seavey_allprobs <- ar6_seavey |> 
  pivot_longer(4:8, names_to = "pctl", values_to = "ft") |>
  mutate(ft = m2ft(ft)) |> 
  unite("scenario_combo", c(2,5), remove = F)

par6 <- ggplot(ar6_seavey_allprobs |> filter(year <= 2150), aes(x = year, y = ft, color = scenario_combo)) +
  geom_line() +
  facet_grid(rows = vars(scenario), scales = "free")

ggplotly(par6, dynamicTicks = T)

# SLR PROJECTION TABLE ----
## create SLR projection table AR6 ----

ar6_table <- ar6_seavey |> 
  rename(Year = year) |> 
  filter(Year %in% c(2030,2050,2100,2150)) |> 
  select(-c(1)) |> 
  mutate(across(3:7, ~round(m2ft(.), 1))) |> 
  mutate(across(3:7, ~formatC(., digits = 1, format = "f", flag = "#"))) |>
  mutate(across(2:7, ~as.character(.))) |> 
  mutate(slr = paste0(pctl_50, "\n(", pctl_17, " to ", pctl_83, ")",
                      "\n[", pctl_05, " to ", pctl_95, "]")) |> 
  select(1,2,9) |> 
  pivot_wider(names_from = scenario, values_from = slr)

ar6_table_full <- ar6_seavey |> 
  rename(Year = year) |> 
  # filter(Year %in% c(2030,2050,2100,2150)) |> 
  select(-c(1)) |> 
  mutate(across(3:7, ~round(m2ft(.), 1))) |> 
  mutate(across(3:7, ~formatC(., digits = 1, format = "f", flag = "#"))) |>
  mutate(across(2:7, ~as.character(.))) |> 
  mutate(slr = paste0(pctl_50, "\n(", pctl_17, " to ", pctl_83, ")",
                      "\n[", pctl_05, " to ", pctl_95, "]")) |> 
  select(1,2,9) |> 
  pivot_wider(names_from = scenario, values_from = slr)

## GT ----

tslrar6 <- ar6_table |> 
  gt() |> 
  # tab_header(
  #   title = md("**IPCC AR6 relative sea-level rise projections for New Hampshire.** Tabular presentation of ‘medium confidence’ relative sea-level rise projections at Seavey Island for five SSP scenarios and ‘low confidence’ projections for SSP1-2.6 and SSP5-8.5. The values shown are medians and, in parentheses, ‘likely’ ranges, except for the ‘low confidence’ projections, where the presented ranges are 17th to 83rd percentiles with no formal likelihood assessed. Units are feet relative to a baseline of 1995-2014.")
  # ) |> 
  tab_style(
    style = cell_text(whitespace = "pre"),
    locations = cells_body(columns = everything())
  ) |> 
  tab_style(
    style = cell_text(v_align = "top"),
    locations = cells_body(columns = 1)
  ) |> 
  cols_align(
    align = "center",
    columns = everything()
  ) |> 
  tab_style(
    location = cells_column_labels(columns = c("SSP1-2.6 + LC", "SSP5-8.5 + LC")),
    style = cell_fill("#80bcd8", .5)
  ) |>
  gt_highlight_cols(
    columns = c("SSP1-2.6 + LC", "SSP5-8.5 + LC"),
    fill = "#80bcd8",
    alpha = .5
  ) |> 
  opt_align_table_header(align = "left") |>
  opt_table_font(size = 11, font = "Roboto Condensed") |>
  tab_options(table.width = px(600)) |> 
  opt_table_font(size = 11) |>
  opt_row_striping()

tslrar6
gtsave(tslrar6, "SLR table - AR6.png")

## FLEXTABLE ----

tflex_slrar6 <- ar6_table |> 
  flextable() |> 
  theme_vanilla() |>
  flextable::align(align = "center", part = "all") |>
  style(pr_t = fp_text(font.family = "Roboto Condensed", font.size = 9),
        part = "all") |>
  bold(j=1) |> 
  bold(part = "header") |> 
  bg(j = c(7,8), part = "header", bg = "lightblue") |> 
  bg(j = c(7,8), part = "body", bg = "lightblue") |> 
  set_table_properties(width = 1, layout = "fixed")

tflex_slrar6

save_as_docx(tflex_slrar6, path = "SLR table - AR6.docx")

tflex_slrar6_full <- ar6_table_full |> 
  flextable() |> 
  theme_vanilla() |>
  flextable::align(align = "center", part = "all") |>
  style(pr_t = fp_text(font.family = "Roboto Condensed", font.size = 9),
        part = "all") |>
  bold(j=1) |> 
  bold(part = "header") |> 
  bg(j = c(7,8), part = "header", bg = "lightblue") |> 
  bg(j = c(7,8), part = "body", bg = "lightblue") |> 
  set_table_properties(width = 1, layout = "fixed")

tflex_slrar6_full

save_as_docx(tflex_slrar6_full, path = "SLR table - AR6 complete.docx")

# Plot AR6 ----

p.ar6 <- read_excel("AR6/ipcc_ar6_sea_level_projection_psmsl_id_288.xlsx",
                    sheet = "Total") |> 
  pivot_longer(6:19, names_to = "year", values_to = "m") |> 
  pivot_wider(names_from = quantile, values_from = m, names_prefix = "pctl_") |> 
  mutate(year = as.numeric(year)) |> 
  # unite("scenario", 4:3, sep = "-") |> 
  mutate(scenario = fct_rev(factor(scenario, levels = c("ssp119", "ssp126",
                                                "ssp245", "ssp370",
                                                "ssp585"),
                           labels = c("SSP1-1.9", "SSP1-2.6", "SSP2-4.5",
                                      "SSP3-7.0", "SSP5-8.5")))) |>
  mutate(confidence = factor(confidence, levels = c("medium", "low"),
                             labels = c("Medium", "Low"))) |> 
  mutate(id = paste0(scenario,confidence)) |> 
  filter(year != 2110, year != 2120) |> # skip these per Kopp's recommentation (https://github.com/Rutgers-ESSP/IPCC-AR6-Sea-Level-Projections/blob/main/FAQ.md#:~:text=it%20may%20be%20desirable%20to%20skip%20the%202110%20(and%20in%20some%20cases%202120)%20time%20steps)
  dplyr::select(-process) |> 
  rename(pctl_05 = "pctl_5") |> 
  # pivot_longer(5:9, names_to = "pctl", values_to = "ft") |>
  mutate(across(5:9, ~m2ft(.x))) |>   
  ggplot() +
  # geom_ribbon(aes(x = year, ymin = pctl_17, ymax = pctl_83, fill = scenario, group = id), alpha = 0.2) +
  # geom_line(data=~filter(.x, scenario == "SSP5-8.5", confidence == "Low"),
  #           aes(x = year, y = pctl_95, color = scenario), linetype = "dotted") +
  geom_line(aes(x = year, y = pctl_50, color = scenario, linetype = confidence), linewidth = 1) +
  # geom_line(data = nh_obs, aes(Year, TG_obs, size = "Tide Gauge Observations"),
  #           color = "#003087", alpha = .75, key_glyph = "timeseries") +
  # stat_xspline(spline_shape = -0.5, aes(year, pctl_50, color = scenario), size = 1) +
  scale_color_cmocean(name = "matter", discrete = T, start = .1, end = .9) +
  scale_fill_cmocean(name = "matter", discrete = T, start = .1, end = .9) +
  # scale_color_scico_d(palette = "glasgow", direction = 1, 
  #                     # limits = c("Low", "Intermediate-Low", "Intermediate", "Intermediate-High", "High"),
  #                     aesthetics = c("color", "fill")) +
  # geom_xspline(data = nh_obs_extrap %>% filter(Year>=1970), spline_shape = -0.5, aes(Year, pctl_50, size = "Observation Extrapolation"),
  #              linetype = "dashed", color = "black", alpha = .8) +
  # geom_ribbon(data = nh_obs_extrap %>% filter(Year>=2020), aes(Year, ymin = pctl_17, ymax = pctl_83, size = "Observation Extrapolation"), fill = "black", alpha = .2) +
  scale_y_continuous(breaks = seq(-2, 12, 1),
                     sec.axis = sec_axis(~.*0.3048, breaks = seq(-1,5,.5), name = "Relative Sea Level (m)")) +
  scale_x_continuous(breaks = c(2020,2050,2100,2150), minor_breaks = seq(1970,2150,10)) +
  coord_cartesian(xlim = c(2020, 2150), ylim = c(0,NA), expand = FALSE) +
  labs(
    title = "IPCC AR6 Sea Level Rise Projections for New Hampshire",
    color = "Scenarios",
    linetype = "Confidence",
    x = NULL,
    y = "Relative Sea Level (feet)") +
  theme_bw() +
  theme(text = element_text(family = "Roboto Condensed", size = 12),
        plot.title = element_text(face = 'bold'),
        plot.subtitle = element_text(face = "italic"),
        legend.key.width = unit(1, "cm")
  )

p.ar6

## Save figure ----
ggsave(plot = p.ar6,
       filename = "AR6 Scenarios for Seavey.png", 
       width = 6.5, height = 3, dpi = 300, units = "in")

ggsave(plot = p.ar6,
       filename = "AR6 Scenarios for Seavey.pdf", 
       device = cairo_pdf, 
       width = 6.5, height = 5.5, units = "in")
