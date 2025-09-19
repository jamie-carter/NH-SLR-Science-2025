library(readxl)
library(openxlsx)
library(ggalt)
library(ggnewscale)
library(sysfonts)
library(showtext)
library(tidyverse)
library(scico)
library(flextable)
library(officer)

# Add fonts ----
font_add_google("Roboto")
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

nh_obs_extrap <- read.xlsx("SeaLeveldotGOV/sealevel_explorer_data_NH.xlsx", sheet = "Observations-Extrapolation") |> 
  mutate(pctl_17 = mm2ft(`Observation.Extrapolation.50th`),
         pctl_50 = mm2ft(`Observation.Extrapolation.17th`),
         pctl_83 = mm2ft(`Observation.Extrapolation.83rd`),
         .keep = "unused") |> 
  mutate(xmin = -Inf,
         xmax = Inf)

nh_obs <- read.xlsx("SeaLeveldotGOV/sealevel_explorer_data_NH.xlsx", sheet = "Observations") |>
  rename(Year = TG.Obs.Year) |> 
  mutate(TG_obs = mm2ft(TG.Obs)) |> 
  select(2,11)

# NASA_NE_OBS <- read_xlsx("C:/Users/Jamie.Carter/Documents/aa_Resilience/SLRTF/NASA Data/sl_taskforce_scenarios_NE.xlsx", sheet = "Observations", skip = 0) %>%
#   mutate(across(3:6, mm2m)) %>%
#   mutate(Units = "m") |> 
#   mutate(Date = as.Date(ym(paste0(as.character(Year), "-06"))))
# 
# NASA_POR_OBS <- read_xlsx("C:/Users/Jamie.Carter/Documents/aa_Resilience/SLRTF/NASA Data/sl_taskforce_scenarios_psmsl_id_183.xlsx", sheet = "Observations", skip = 0) %>%
#   mutate(across(3:6, mm2m)) %>%
#   mutate(Units = "m") |> 
#   mutate(Date = as.Date(ym(paste0(as.character(Year), "-06"))))
# 
# NASA_POR_SLR <- read_xlsx("C:/Users/Jamie.Carter/Documents/aa_Resilience/SLRTF/NASA Data/sl_taskforce_scenarios_psmsl_id_183.xlsx", sheet = "Total", skip = 0) |> 
#   mutate(across(6:19, mm2m)) %>%
#   mutate(Units = "m") %>%
#   unite("Scenario", scenario:quantile, remove = TRUE) %>%
#   mutate(`2000` = 0, .after = `Scenario`) %>%
#   pivot_longer(cols = 5:19, names_to = "year", values_to = "SLRm") %>%
#   pivot_wider(names_from = Scenario, values_from = SLRm) %>%
#   mutate(year = as.numeric(year)) |> 
#   mutate(Date = as.Date(ym(paste0(as.character(year), "-06"))))
# 
# NASA_BOS_OBS <- read_xlsx("C:/Users/Jamie.Carter/Documents/aa_Resilience/SLRTF/NASA Data/sl_taskforce_scenarios_psmsl_id_235.xlsx", sheet = "Observations", skip = 0) %>%
#   mutate(across(3:6, mm2m)) %>%
#   mutate(Units = "m") |> 
#   mutate(Date = as.Date(ym(paste0(as.character(Year), "-06"))))
# 
# NASA_BOS_SLR <- read_xlsx("C:/Users/Jamie.Carter/Documents/aa_Resilience/SLRTF/NASA Data/sl_taskforce_scenarios_psmsl_id_235.xlsx", sheet = "Total", skip = 0) |> 
#   mutate(across(6:19, mm2m)) %>%
#   mutate(Units = "m") %>%
#   unite("Scenario", scenario:quantile, remove = TRUE) %>%
#   mutate(`2000` = 0, .after = `Scenario`) %>%
#   pivot_longer(cols = 5:19, names_to = "year", values_to = "SLRm") %>%
#   pivot_wider(names_from = Scenario, values_from = SLRm) %>%
#   mutate(year = as.numeric(year)) |> 
#   mutate(Date = as.Date(ym(paste0(as.character(year), "-06"))))

# Seavey ----

SLR2022a <- read_csv("C:/Users/Jamie.Carter/Documents/aa_Resilience/SLRTF/Sea_Level_Rise_Datasets_2022/SLR_TF U.S. Sea Level Projections.csv", skip = 17)

addoffset <- function(x) (x + SLR2022a$`Offset 2000 to 2005 (cm)`)

SLR2022 <- SLR2022a |> 
  mutate(`RSL2000 (cm)` = 0, .after = `Offset 2000 to 2005 (cm)`) |> 
  mutate(across(15:29, addoffset)) |> 
  mutate(across(11:29, cm2m)) |> 
  pivot_longer(14:29, names_to = "year", values_to = "m") |> 
  separate_wider_delim(Scenario, delim = " - ", names = c("scenario", "pctl")) |> 
  mutate(year = str_extract(year, "[0-9]+")) |> 
  pivot_wider(names_from = pctl, values_from = m) |> 
  mutate(source = "2022 U.S. Technical Report") |> 
  mutate(scenario = factor(scenario, levels = c("0.3", "0.5", "1.0", "1.5", "2.0"),
                     labels = c("Low", "Intermediate-Low", "Intermediate", "Intermediate-High", "High"))) |> 
  rename("psmsl_id" = "PSMSL ID",
         "pctl_17" = "LOW",
         "pctl_50" = "MED",
         "pctl_83" = "HIGH") |> 
  mutate(year = as.numeric(year)) |> 
  select(2,10,14:18)

slr2022_seavey <- SLR2022 |> 
  # filter(psmsl_id == 1004702890) # grid location for Seavey
  filter(psmsl_id == 288) # Seavey Island

slr2022_seavey |> 
  filter(year %in% c(2030, 2050, 2070, 2100, 2150)) |> 
  ggplot(aes(x = scenario)) +
  geom_linerange(aes(ymin = pctl_17, ymax = pctl_83)) +
  geom_point(aes(y = pctl_50)) +
  facet_grid(rows = vars(year), scales = "free")

# Uncertainty plot for Seavey ----

# Read SLR data ----

PSMSL_NAMES <- read_csv("C:/Users/Jamie.Carter/Documents/aa_Resilience/SLRTF/Sea_Level_Rise_Datasets_2022/SLR_TF U.S. Sea Level Projections.csv", skip = 17) %>%
  select(1:4,8) %>%
  unique()

# Fig Extrapolation ----

NH_extrapolation <- slr2022_seavey |> 
  mutate(across(pctl_50:pctl_83, m2ft),
         scenario = fct_rev(scenario)) |> 
  ggplot() +
  # geom_line(data = NOAA_Seavey |> mutate(MSL = m2ft(MSL+0.01736),
  #                                        Year = replace_na(Year,20000),
  #                                        ts = cumsum(c(0, diff(Year)) > 1)),
  #           aes(datetime, MSL, size = "Tide Gauge Observations", group = ts),
  #           color = "magenta", alpha = .8, key_glyph = "timeseries") +
  geom_line(data = nh_obs, aes(Year, TG_obs, size = "Tide Gauge Observations"), 
            color = "#003087", alpha = .75, key_glyph = "timeseries") +
  # geom_line(data = NOAA_monthly_blend, aes(decimal_date(Date), m2ft(MSL_avg), size = "Tide Gauge Observations"), 
  #           color = "green", alpha = .8, key_glyph = "timeseries") +
  # geom_xspline(data = NASA_NE_OBS %>% filter(Year>=2020), spline_shape = -0.5, aes(Year, m2ft(`Obs Extrap. 50th`), size = "Observation Extrapolation"),
  #              linetype = "dashed", color = "black", alpha = .8) +
  # geom_ribbon(data = NASA_NE_OBS %>% filter(Year>=2020), aes(Year, ymin = m2ft(`Obs Extrap. 17th`), ymax = m2ft(`Obs Extrap. 83rd`), size = "Observation Extrapolation"), fill = "black", alpha = .2) +
  # geom_ribbon(aes(x = year, ymin = pctl_17, ymax = pctl_83, fill = scenario), alpha = 0.2) +
  stat_xspline(spline_shape = -0.5, aes(year, pctl_50, color = scenario), size = 1) +
  scale_color_scico_d(palette = "hawaii", direction = 1, 
                      # limits = c("Low", "Intermediate-Low", "Intermediate", "Intermediate-High", "High"),
                      aesthetics = c("color", "fill")) +
  geom_xspline(data = nh_obs_extrap %>% filter(Year>=1970), spline_shape = -0.5, aes(Year, pctl_50, size = "Observation Extrapolation"),
               linetype = "dashed", color = "black", alpha = .8) +
  geom_ribbon(data = nh_obs_extrap %>% filter(Year>=2020), aes(Year, ymin = pctl_17, ymax = pctl_83, size = "Observation Extrapolation"), fill = "black", alpha = .2) +
  scale_y_continuous(breaks = seq(-2, 12, 1),
                     sec.axis = sec_axis(~.*12, breaks = c(0, 6, seq(-3,18,3)), name = "Relative Sea Level (inches)")) +
  scale_x_continuous(breaks = seq(1970,2050,10), minor_breaks = seq(1970,2100,10)) +
  coord_cartesian(xlim = c(1970, 2050), ylim = c(-.5, 1.5), expand = FALSE) +
  scale_size_manual(breaks = c("Tide Gauge Observations", "Observation Extrapolation"),
                    values = c(0.25, 0.75)) +
  labs(
    title = "Observed and Projected Relative Sea Level for New Hampshire to 2050",
    # subtitle = "Based on Tide Gauge Observations and 2022 Sea-Level Rise Scenarios",
    size = "Observations",
    color = "Scenarios",
    fill = "Scenarios",
    x = NULL,
    y = "Relative Sea Level (feet)") +
  # guides(linewidth = guide_legend(ncol = 1, nrow = 2, order = 1),
  #   size = guide_legend(ncol = 1, nrow = 2, order = 1),
  #   fill = guide_legend(ncol = 2, order = 3),
  #   color = guide_legend(ncol = 2, order = 3)) +
  theme_bw() +
  theme(text = element_text(family = "Roboto Condensed", size = 12),
        plot.title = element_text(face = 'bold', family = "Roboto Condensed"),
        plot.subtitle = element_text(face = "italic", family = "Roboto Condensed"),
        # panel.grid = element_blank(),
        # panel.grid = element_line(
        #   colour = NULL,
        #   linewidth = 0.1),
        # legend.margin = margin(0,0,0,0),
        # legend.background = element_blank(),
        # legend.title = element_blank(),
        # legend.position = "bottom",
        # legend.box = "horizontal",
        # legend.box.just = "top",
        # legend.box.margin = margin(0,0,0,0),
        # legend.box.background = element_blank(),
        # legend.box.spacing = unit(.1, "in"),
        # legend.key.height = unit(.15, "in"),
        legend.key.width = unit(0.75, "cm")
        # legend.spacing.x = unit(.1, "in"),
        # legend.spacing.y = unit(0.3, "in")
        )

NH_extrapolation

## Save extrapolation figure ----
ggsave(plot = NH_extrapolation,
       filename = "Sea Level Rise Scenarios for Seavey - Extrapolation.png", 
       width = 6.5, height = 3, dpi = 300, units = "in")

ggsave(plot = NH_extrapolation,
       filename = "Sea Level Rise Scenarios for Seavey - Extrapolation.pdf", 
       device = cairo_pdf, 
       width = 6.5, height = 3, units = "in")

# Fig Uncertainty ----

NH_uncertainty <- slr2022_seavey |> 
  mutate(across(pctl_50:pctl_83, m2ft),
         scenario = fct_rev(scenario)) |> 
  ggplot() +
  geom_line(data = nh_obs, aes(Year, TG_obs, size = "Tide Gauge Observations"), 
            color = "#003087", alpha = .75, key_glyph = "timeseries") +
  geom_ribbon(aes(x = year, ymin = pctl_17, ymax = pctl_83, fill = scenario), alpha = 0.2) +
  stat_xspline(spline_shape = -0.5, aes(year, pctl_50, color = scenario), size = 1) +
  scale_color_scico_d(palette = "hawaii", direction = 1, 
                      # limits = c("Low", "Intermediate-Low", "Intermediate", "Intermediate-High", "High"),
                      aesthetics = c("color", "fill")) +
  geom_xspline(data = nh_obs_extrap %>% filter(Year>=1970), spline_shape = -0.5, aes(Year, pctl_50, size = "Observation Extrapolation"),
               linetype = "dashed", color = "black", alpha = .8) +
  geom_ribbon(data = nh_obs_extrap %>% filter(Year>=2020), aes(Year, ymin = pctl_17, ymax = pctl_83, size = "Observation Extrapolation"), fill = "black", alpha = .2) +
  scale_y_continuous(breaks = seq(-2, 12, 1),
                     sec.axis = sec_axis(~.*1, breaks = seq(-2,12,1), name = "Relative Sea Level (feet)")) +
  scale_x_continuous(breaks = seq(2000,2100,10), minor_breaks = seq(1970,2100,10)) +
  coord_cartesian(xlim = c(2000, 2100), ylim = c(-1, 8), expand = FALSE) +
  scale_size_manual(breaks = c("Tide Gauge Observations", "Observation Extrapolation"),
                    values = c(0.25, 0.75)) +
  labs(
    title = "Observed and Projected Relative Sea Level for New Hampshire to 2100",
    # subtitle = "Based on Tide Gauge Observations and 2022 Sea-Level Rise Scenarios",
    size = "Observations",
    color = "Scenarios",
    fill = "Scenarios",
    x = NULL,
    y = "Relative Sea Level (feet)") +
  # guides(linewidth = guide_legend(ncol = 1, nrow = 2, order = 1),
  #   size = guide_legend(ncol = 1, nrow = 2, order = 1),
  #   fill = guide_legend(ncol = 2, order = 3),
  #   color = guide_legend(ncol = 2, order = 3)) +
  theme_bw() +
  theme(text = element_text(family = "Roboto Condensed", size = 12),
        plot.title = element_text(face = 'bold', family = "Roboto Condensed"),
        plot.subtitle = element_text(face = "italic", family = "Roboto Condensed"),
        # panel.grid = element_blank(),
        # panel.grid = element_line(
        #   colour = NULL,
        #   linewidth = 0.1),
        # legend.margin = margin(0,0,0,0),
        # legend.background = element_blank(),
        # legend.title = element_blank(),
        # legend.position = "bottom",
        # legend.box = "horizontal",
        # legend.box.just = "top",
        # legend.box.margin = margin(0,0,0,0),
        # legend.box.background = element_blank(),
        # legend.box.spacing = unit(.1, "in"),
        # legend.key.height = unit(.15, "in"),
        legend.key.width = unit(0.75, "cm")
        # legend.spacing.x = unit(.1, "in"),
        # legend.spacing.y = unit(0.3, "in")
  )

NH_uncertainty

## Save uncertainty figure ----
ggsave(plot = NH_uncertainty,
       filename = "Sea Level Rise Scenarios for Seavey - Uncertainty.png", 
       width = 6.5, height = 3, dpi = 300, units = "in")

ggsave(plot = NH_uncertainty,
       filename = "Sea Level Rise Scenarios for Seavey - Uncertainty.pdf", 
       device = cairo_pdf, 
       width = 6.5, height = 3, units = "in")

# SLR table ----
# SLR PROJECTION TABLE ----
## create SLR projection table ----

nh_obs_extrap_table <- nh_obs_extrap |> 
  mutate(across(pctl_17:pctl_83, ~formatC(., digits = 1, format = "f", flag = "#"))) |> 
  # mutate(across(pctl_17:pctl_83, ~format(round(.x,1), nsmall=1))) |> 
  filter(Year %in% c(2030,2050,2100,2150)) |> 
  # select(2,3,4:6) |> 
  mutate(Year = as.character(Year), .keep = "unused") |> 
  mutate(`Observation Extrapolation` = paste0(pctl_50, "\n(", pctl_17, " to ", pctl_83, ")")) |> 
  select(Year, `Observation Extrapolation`)
  
slr2022_table <- slr2022_seavey |> 
  mutate(scenario = factor(scenario, labels = c("Low", "Int-Low", "Intermediate", "Int-High", "High"))) |> 
  mutate(across(pctl_50:pctl_83, m2ft),
         across(pctl_50:pctl_83, ~formatC(., digits = 1, format = "f", flag = "#")),
         # across(pctl_50:pctl_83, ~format(round(.x,1), nsmall=1)),
         scenario = fct_rev(scenario)) |> 
  filter(year %in% c(2030,2050,2100,2150)) |> 
  select(2,3,4:6) |> 
  mutate(Year = as.character(year), .keep = "unused") |> 
  mutate(slr = paste0(pctl_50, "\n(", pctl_17, " to ", pctl_83, ")")) |> 
  select(1,5,6) |> 
  pivot_wider(names_from = scenario, values_from = slr) |> 
  left_join(nh_obs_extrap_table)

tslr2022 <- slr2022_table |> 
  gt() |> 
  # tab_header(
  #   title = md("**2022 Interagency Task Force relative sea-level rise scenarios for New Hampshire.** Tabular presentation of relative sea-level rise values at Seavey Island for five scenarios. The values shown are medians and, in parentheses, 17th to 83rd percentile ranges. Units are feet relative to a baseline of 2000.")
  # ) |> 
  sub_missing(missing_text = "---") |> 
  tab_style(
    style = cell_text(whitespace = "pre"),
    locations = cells_body(columns = everything())
  ) |> 
  tab_style(
    style = cell_text(v_align = "top"),
    locations = cells_body(columns = 1)
  ) |> 
  tab_style(
    location = cells_column_labels(columns = c("Observation Extrapolation")),
    style = cell_fill("#80bcd8", .5)
  ) |>
  gt_highlight_cols(
    columns = c("Observation Extrapolation"),
    fill = "#80bcd8",
    alpha = .5
  ) |> 
  cols_align(
    align = "center",
    columns = everything()
  ) |> 
  opt_align_table_header(align = "left") |>
  opt_table_font(size = 11, font = "Roboto Condensed") |>
  tab_options(table.width = px(600)) |> 
  opt_table_font(size = 11) |>
  opt_row_striping()

tslr2022
gtsave(tslr2022, "SLR table - 2022.png")
gtsave(tslr2022, "SLR table - 2022.html")

# FLEXTABLE ----

t_22 <- slr2022_table |> 
  flextable() |> 
  colformat_char(na_str = "–") |> 
  style(pr_t = fp_text(font.family = "Roboto Condensed", font.size = 9),
        part = "all") |>
  bold(j=1) |>
  bg(j = 7, part = "header", bg = "lightblue") |> 
  bg(j = 7, part = "body", bg = "lightblue") |> 
  theme_vanilla() |>
  flextable::align(j = c(2:7), align = "center", part = "header") |>
  flextable::align(align = "center", part = "all") |>
  flextable::align(j = 1, align = "left", part = "all") |> 
  flextable::width(width = .92)

t_22

save_as_docx(t_22, path = "SLR values - 2022.docx")

## create complete SLR projection table ----

nh_obs_extrap_table_full <- nh_obs_extrap |> 
  mutate(across(pctl_17:pctl_83, ~formatC(., digits = 1, format = "f", flag = "#"))) |> 
  # mutate(across(pctl_17:pctl_83, ~format(round(.x,1), nsmall=1))) |> 
  # filter(Year %in% c(2030,2050,2100,2150)) |> 
  # select(2,3,4:6) |> 
  mutate(Year = as.character(Year), .keep = "unused") |> 
  mutate(`Observation Extrapolation` = paste0(pctl_50, "\n(", pctl_17, " to ", pctl_83, ")")) |> 
  select(Year, `Observation Extrapolation`)

slr2022_table_full <- slr2022_seavey |> 
  mutate(scenario = factor(scenario, labels = c("Low", "Int-Low", "Intermediate", "Int-High", "High"))) |> 
  mutate(across(pctl_50:pctl_83, m2ft),
         across(pctl_50:pctl_83, ~formatC(., digits = 1, format = "f", flag = "#")),
         # across(pctl_50:pctl_83, ~format(round(.x,1), nsmall=1)),
         scenario = fct_rev(scenario)) |> 
  # filter(year %in% c(2030,2050,2100,2150)) |> 
  select(2,3,4:6) |> 
  mutate(Year = as.character(year), .keep = "unused") |> 
  mutate(slr = paste0(pctl_50, "\n(", pctl_17, " to ", pctl_83, ")")) |> 
  select(1,5,6) |> 
  pivot_wider(names_from = scenario, values_from = slr) |> 
  left_join(nh_obs_extrap_table_full)

t_22_full <- slr2022_table_full |> 
  flextable() |> 
  colformat_char(na_str = "–") |> 
  style(pr_t = fp_text(font.family = "Roboto Condensed", font.size = 9),
        part = "all") |>
  bold(j=1) |>
  bg(j = 7, part = "header", bg = "lightblue") |> 
  bg(j = 7, part = "body", bg = "lightblue") |> 
  theme_vanilla() |>
  flextable::align(j = c(2:7), align = "center", part = "header") |>
  flextable::align(align = "center", part = "all") |>
  flextable::align(j = 1, align = "left", part = "all") |> 
  flextable::width(width = .92)

t_22_full

save_as_docx(t_22_full, path = "SLR values - 2022 complete.docx")
