library(tidyverse)
# library(splines)
library(mgcv)
library(scico)
library(ggsci)
library(plotly)
library(gt)
library(gtExtras)
library(openxlsx)
library(ggnewscale)

## Add functions ----

mm2ft <- function(x) (x * 0.00328084)
cm2ft <- function(x) (x * 0.0328084)
m2ft <- function(x) (x * 3.28084)
m2in <- function(x) (x * 39.3701)
m2mm <- function(x) (x * 1000)
mm2m <- function(x) (x / 1000)
cm2m <- function(x) (x * .01)
ft2m <- function(x) (x * 0.3048)
ft2in <- function(x) (x * 12)
addoffset <- function(x) (x + SLR2022a$`Offset 2000 to 2005 (cm)`)

slr_sets <- bind_rows(k17_seavey |> mutate(source = "Kopp 2014"),
                      ar6_seavey,
                      slr2022_seavey) |> 
  mutate(name = "Seavey Island", .before = psmsl_id) |> 
  mutate(across(c(5:13), m2ft))

nh_obs <- read.xlsx("SeaLeveldotGOV/sealevel_explorer_data_NH.xlsx", sheet = "Observations-Extrapolation") |> 
  mutate(pctl_17 = mm2ft(`Observation.Extrapolation.50th`),
         pctl_50 = mm2ft(`Observation.Extrapolation.17th`),
         pctl_83 = mm2ft(`Observation.Extrapolation.83rd`),
         .keep = "unused") |> 
  mutate(xmin = -Inf,
         xmax = Inf) |> 
  rename(year = Year) |> 
  filter(year %in% c(2050, 2100))

# Plot different SLR projections for 2030, 2050, and 2100 ----

yintercept_low <- slr_sets |> filter(scenario == "Low" & year %in% c(2050, 2100, 2150)) |> select(year, pctl_50)
yintercept_high <- slr_sets |> filter(scenario == "High" & year %in% c(2050, 2100, 2150)) |> select(year, pctl_50)

z.cols <- c("#C85E37FF", "#C53CE1FF", "#2A90BAFF", "#6F9016FF", # K17 colors
            "#FACA90FF", "#EE855EFF", "#CF4456FF", "#952063FF", "#511653FF","lightgray", "lightgray", # AR6 colors
            "#B2F2FD", "#6BD48C", "#9B951B", "#964D3E", "#8C0172") # 2022 colors

z <- ggplot(slr_sets |> filter(year %in% c(2050, 2100, 2150)) |> 
              mutate(scenario = factor(scenario, levels = c("RCP-2.6", "RCP-4.5", "RCP-6.0", "RCP-8.5", "SSP1-1.9", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5", "SSP1-2.6 + LC", "SSP5-8.5 + LC", "Low", "Intermediate-Low", "Intermediate", "Intermediate-High", "High"))),
       aes(x = scenario, group = source)) +
  geom_rect(data = nh_obs, aes(ymin = pctl_17, ymax = pctl_83, xmin = xmin, xmax = xmax),
            alpha = 0.15, fill = '#0072B2', inherit.aes = FALSE) +
  geom_hline(data = nh_obs, aes(yintercept = pctl_50), alpha = 0.75, color = '#0072B2') +
  geom_hline(data=yintercept_low, aes(yintercept = pctl_50), linetype = "dashed", color = "#003087") +
  geom_hline(data=yintercept_high, aes(yintercept = pctl_50), linetype = "dashed", color = "#003087") +
  geom_point(aes(y = pctl_99.5), shape = 2, stroke = .25) +
  geom_point(aes(y = pctl_99.9), shape = 4, stroke = .25) +
  geom_point(aes(y = pctl_01), shape = 1, stroke = .25) +
  geom_point(aes(y = pctl_99), shape = 1, stroke = .25) +
  geom_boxplot(aes(middle = pctl_50, lower = pctl_17, upper = pctl_83, ymin = pctl_05, ymax = pctl_95, group = scenario, fill = scenario),
               stat = "identity", staplewidth = .2, width = .6, linewidth = .25) +
  scale_fill_manual(values = z.cols) +
  facet_grid(rows = vars(year),
             cols = vars(source),
             scales = "free",
             space = "free_x") +
  labs(title = "Relative Sea Level Rise Scenarios for New Hampshire", 
       y = "Relative Sea Level Rise (ft)", 
       x = "Scenario") +
  guides(x = guide_axis(angle = 45)) +
  theme_bw() +
  theme(text = element_text(family = "Roboto Condensed", size = 12),
        plot.title = element_text(face = 'bold', family = "Roboto Condensed"),
        plot.subtitle = element_text(face = "italic", family = "Roboto Condensed"),
        legend.position = "none",
        axis.text=element_text(size=9),
        axis.title=element_text(size=11),
        legend.text=element_text(size=11))

z
ggsave(z, filename = "Relative Sea Level Rise Scenarios for Seavey Island.png", 
       width = 6.5, height = 5, dpi = 300)

# Plot different SLR projections over time ----

p <- ggplot(slr_sets |> filter(year <= 2150)) +
  geom_ribbon(aes(x = year, ymin = ft2in(pctl_05), ymax = ft2in(pctl_95), fill = scenario, color = scenario), alpha = .1) +
  geom_ribbon(aes(x = year, ymin = ft2in(pctl_17), ymax = ft2in(pctl_83), fill = scenario, color = scenario), alpha = .25) +
  geom_line(aes(x = year, y = ft2in(pctl_50), colour = scenario), linewidth = 1) +
  geom_point(aes(x = year, y = ft2in(pctl_50), colour = scenario), size = 2) +
  labs(y = "SLR (in)") +
  facet_wrap(~source)

ggplotly(p, dynamicTicks = T)


# Create models for SLR crosswalk ----

## Expand data to include 0 values for 2000 ----

slr_2000 <- tibble(
  name = rep("Seavey Island", 15),
  psmsl_id = rep(288, 15),
  year = rep(2000, 15),
  scenario = c("Low", "Intermediate-Low", "Intermediate", "Intermediate-High", "High",
               "SSP1-1.9", "SSP1-2.6", "SSP2-4.5", "SSP3-7.0", "SSP5-8.5", "SSP5-8.5 + LC",
    "RCP-2.6", "RCP-4.5", "RCP-6.0", "RCP-8.5"),
  pctl_50 = rep(0, 15),
  pctl_17 = rep(0, 15),
  pctl_83 = rep(0, 15),
  source = c(rep("2022 U.S. Technical Report", 5), rep("IPCC AR6", 6), rep("Kopp 2017", 4))
)

slr_sets_2000 <- slr_sets |> 
  bind_rows(slr_2000) |>
  arrange(source, scenario, year) |> 
  distinct()

## Plot spline fits to test spline model is working ----
pyear <- 2090
pslr <- 2.5 #+ m2ft(.09) # adjusted for SLR from 1992 to 2011

## Year on x-axis

p <- ggplot(slr_sets |> filter(year <= 2150)) +
  geom_hline(aes(yintercept = pslr)) +
  geom_vline(aes(xintercept = pyear)) +
  geom_line(aes(x = year, y = pctl_50, colour = scenario), linewidth = 1) +
  geom_smooth(aes(x = year, y = pctl_50, group = scenario),
              method = mgcv::gam,
              formula = y ~ s(x, k = 6, bs = "cr"),
              linewidth = .5,
              se = F) +
  facet_wrap(~ source)

ggplotly(p, dynamicTicks = T)

## Create models for data extraction ----

slr_sets_2000_long <- slr_sets |> 
  pivot_longer(c(5:13), names_to = "pctl", values_to = "ft") |> 
  drop_na(ft) |> 
  mutate(source = factor(source))

### Using GAM cubic regression for interpolation ----

# lm_slr_by_year <- slr_sets_2000_long |> 
#   group_by(scenario, source, scenario_pctl, pctl) |> 
#   summarize(pred = round(predict(gam(ft ~ s(year, k = 6, bs = "cr")), list(year = pyear)), 1), .groups = "drop")

pslr <- c(1.3, 2.4, 4.2, 7.7)

lm_slr_by_feet <- function(pslr) {
  slr_sets_2000_long |>
  group_by(scenario, source, pctl) |>
  # summarize(pred = round(predict(gam(year ~ s(ft, k = 6, bs = "cr")), list(ft = pslr))/5)*5, .groups = "drop") |> 
    summarize(pred = round(predict(gam(year ~ s(ft, k = 6, bs = "cr")), list(ft = pslr))), .groups = "drop") |> 
    mutate(pred = case_when(pred < 2000 ~ NA,
                          pred > 2150 ~ NA,
                          .default = pred)) |>
  pivot_wider(names_from = pctl, values_from = pred) |> 
    mutate(SLR = pslr, .before = everything())
}

whg_cfrm_scenarios <- pslr |> 
  map(lm_slr_by_feet) |> 
  bind_rows()

# MILESTONE PLOTS ----

# Boxplots for all ----

ggplot(whg_cfrm_scenarios, aes(x = scenario, group = source)) +
  geom_point(aes(y = pctl_99.5), shape = 2) +
  geom_point(aes(y = pctl_99.9), shape = 4) +
  geom_point(aes(y = pctl_01), shape = 1) +
  geom_point(aes(y = pctl_99), shape = 1) +
  geom_boxplot(aes(middle = pctl_50, lower = pctl_17, upper = pctl_83, ymin = pctl_05, ymax = pctl_95, group = scenario),
               stat = "identity", staplewidth = .2, width = .6, fill = "darkgray", linewidth = .33) +
  coord_flip() +
  scale_y_continuous(name = "Year (rounded to decade)",
                     breaks = seq(2000,2150,10)) +
  facet_wrap(~SLR)

# Bar plots for SSPs ----

whg_cfrm_scenarios_LC <- whg_cfrm_scenarios |> 
  mutate(SLR = fct_rev(paste0(SLR, " ft"))) |> 
  filter(source == "IPCC AR6") |> 
  separate_wider_delim(scenario, " + ", names = c("scenario", "LC"), too_few = "align_start") |> 
  mutate(pctl_17 = case_when(!is.na(pctl_83) & is.na(pctl_17) ~ 2150,
                             .default = pctl_17)) |> 
  mutate(pctl_05 = case_when(!is.na(pctl_95) & is.na(pctl_05) ~ 2150,
                             .default = pctl_05))

milestones_2022_low <- whg_cfrm_scenarios |> 
  mutate(SLR = fct_rev(paste0(SLR, " ft"))) |> 
  filter(scenario == "Low")
milestones_2022_intlow <- whg_cfrm_scenarios |> 
  mutate(SLR = fct_rev(paste0(SLR, " ft"))) |> 
  filter(scenario == "Intermediate-Low")
milestones_2022_int <- whg_cfrm_scenarios |> 
  mutate(SLR = fct_rev(paste0(SLR, " ft"))) |> 
  filter(scenario == "Intermediate")
milestones_2022_inthigh <- whg_cfrm_scenarios |> 
  mutate(SLR = fct_rev(paste0(SLR, " ft"))) |> 
  filter(scenario == "Intermediate-High")
milestones_2022_high <- whg_cfrm_scenarios |> 
  mutate(SLR = fct_rev(paste0(SLR, " ft"))) |> 
  filter(scenario == "High")

ssp_names <- whg_cfrm_scenarios_LC |> 
  filter(SLR == "1.3 ft",
         is.na(LC)) |> 
  select(1:2) |> 
  mutate(year = 2005)

ggplot(whg_cfrm_scenarios_LC) +
  geom_linerange(data=whg_cfrm_scenarios_LC |> filter(!is.na(LC)),
                 aes(y = scenario, xmin = pctl_95, xmax = pctl_83,
                     color = scenario), linewidth = 1, alpha = .5) +
  geom_linerange(data=whg_cfrm_scenarios_LC |> filter(!is.na(LC)),
                 aes(y = scenario, xmin = pctl_17, xmax = pctl_05,
                     color = scenario), linewidth = 1, alpha = .5) +
  geom_linerange(data=whg_cfrm_scenarios_LC |> filter(!is.na(LC)),
                 aes(y = scenario, xmin = pctl_83, xmax = pctl_17,
                     color = scenario), linewidth = 2, alpha = .5) +
  geom_linerange(data=whg_cfrm_scenarios_LC |> filter(is.na(LC)),
                 aes(y = scenario, xmin = pctl_83, xmax = pctl_17,
                     color = scenario), linewidth = 2) +
  geom_point(data=whg_cfrm_scenarios_LC |> filter(is.na(LC)),
                 aes(y = scenario, x = pctl_50, fill = scenario),
                     shape = 21, color = "white", size = 2.5, stroke = 1) +
  # geom_vline(data=milestones_2022_low, aes(xintercept = pctl_50), linetype = "dashed", color = "#003087") +
  # geom_vline(data=milestones_2022_intlow, aes(xintercept = pctl_50), linetype = "dashed", color = "#003087") +
  # geom_vline(data=milestones_2022_int, aes(xintercept = pctl_50), linetype = "dashed", color = "#003087") +
  # geom_vline(data=milestones_2022_inthigh, aes(xintercept = pctl_50), linetype = "dashed", color = "#003087") +
  # geom_vline(data=milestones_2022_high, aes(xintercept = pctl_50), linetype = "dashed", color = "#003087") +
  geom_text(data=ssp_names, aes(x=year, y=scenario, label = scenario, colour = scenario),
            hjust = "left") +
  scale_x_continuous(limits = c(2000, 2150), expand = c(0, 0),
                     minor_breaks = seq(2000,2150,10)) +
  scale_y_discrete(labels = NULL) +
  facet_grid(rows = vars(SLR)) +
  labs(title = "Projected Timing of Sea Level Rise Milestones for Seavey Island", 
       subtitle = "IPCC AR6 Sea Level Projections",
       y = NULL, 
       x = NULL) +
  theme_bw() +
  theme(text = element_text(family = "Roboto Condensed", size = 12),
        plot.title = element_text(face = 'bold', family = "Roboto Condensed"),
        plot.subtitle = element_text(face = "italic", family = "Roboto Condensed"),
        legend.position = "none",
        axis.text=element_text(size=9),
        axis.title=element_text(size=11),
        plot.margin = unit(c(0.25,0.5,0.25,0.5), "cm"),
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.text=element_text(size=11)) +
  scale_color_cmocean(name = "matter", discrete = T, start = .1, end = .9) +
  scale_fill_cmocean(name = "matter", discrete = T, start = .1, end = .9)

ggsave("Milestones bar plot - AR6.png", width = 6.5, height = 4, dpi = 300)

# Bar plots for 2022 Scenarios ----

whg_cfrm_scenarios_LC <- whg_cfrm_scenarios |> 
  mutate(SLR = fct_rev(paste0(SLR, " ft"))) |> 
  filter(source == "2022 U.S. Technical Report") |> 
  mutate(pctl_17 = case_when(!is.na(pctl_83) & is.na(pctl_17) ~ 2150,
                             .default = pctl_17)) |> 
  mutate(pctl_05 = case_when(!is.na(pctl_95) & is.na(pctl_05) ~ 2150,
                             .default = pctl_05))

scenario_names <- whg_cfrm_scenarios_LC |> 
  filter(SLR == "1.3 ft") |> 
  select(1:2) |> 
  mutate(year = 2005)

ggplot(whg_cfrm_scenarios_LC) +
  geom_linerange(data=whg_cfrm_scenarios_LC,
                 aes(y = scenario, xmin = pctl_83, xmax = pctl_17,
                     color = scenario), linewidth = 2) +
  geom_point(data=whg_cfrm_scenarios_LC,
             aes(y = scenario, x = pctl_50, fill = scenario),
             shape = 21, color = "white", size = 2.5, stroke = 1) +
  geom_text(data=scenario_names, aes(x=year, y=scenario, label = scenario, colour = scenario),
            hjust = "left") +
  scale_x_continuous(limits = c(2000, 2150), expand = c(0, 0),
                     minor_breaks = seq(2000,2150,10)) +
  scale_y_discrete(labels = NULL) +
  facet_grid(rows = vars(SLR)) +
  labs(title = "Projected Timing of Sea Level Rise Milestones for Seavey Island", 
       subtitle = "2022 Interagency Technical Report Scenarios",
       y = NULL, 
       x = NULL) +
  theme_bw() +
  theme(text = element_text(family = "Roboto Condensed", size = 12),
        plot.title = element_text(face = 'bold', family = "Roboto Condensed"),
        plot.subtitle = element_text(face = "italic", family = "Roboto Condensed"),
        legend.position = "none",
        axis.text=element_text(size=9),
        axis.title=element_text(size=11),
        plot.margin = unit(c(0.25,0.5,0.25,0.5), "cm"),
        panel.grid.major.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.text=element_text(size=11)) +
  scale_color_scico_d(palette = "hawaii", begin = .2, end = .8, direction = -1,
                      aesthetics = c("fill", "color"))

ggsave("Milestones bar plot - 2022 Scenarios.png", width = 6.5, height = 4, dpi = 300)

# MILESTONE TABLES ----
## create milestones table AR6 ----

milestones <- whg_cfrm_scenarios |> 
  mutate(SLR = paste0(SLR, " ft")) |> 
  mutate(across(4:12, ~as.character(.))) |> 
  mutate(across(4:12, ~replace_na(., "n.c."))) |> 
  mutate(yr = paste0(pctl_50, "\n(", pctl_83, " to ", pctl_17, ")")) |> 
  select(1,2,3,13) |> 
  filter(source == "IPCC AR6") |> 
  pivot_wider(names_from = scenario, values_from = yr)

t <- milestones |> 
  select(-2) |> 
  gt() |> 
  # tab_header(
  #   title = md("**IPCC AR6 milestones for relative sea-level rise values used in the New Hampshire Coastal Flood Risk Model.** Tabular presentation of ‘medium confidence’ relative sea-level rise milestones at Seavey Island for five SSP scenarios and ‘low confidence’ projections for SSP5-8.5. The values shown are medians and, in parentheses, ‘likely’ ranges, except for the ‘low confidence’ projections, where the presented ranges are 17th to 83rd percentiles with no formal likelihood assessed. An entry n.c. means that the target sea-level rise value is not crossed during the period 2000–2150.")
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
    location = cells_column_labels(columns = "SSP5-8.5 + LC"),
    style = cell_fill("#80bcd8", .5)
    ) |>
  gt_highlight_cols(
    columns = "SSP5-8.5 + LC",
    fill = "#80bcd8",
    alpha = .5
  ) |> 
  opt_align_table_header(align = "left") |>
  opt_table_font(size = 11, font = "Roboto Condensed") |>
  tab_options(table.width = px(600)) |> 
  opt_table_font(size = 11) |>
  opt_row_striping()

t
gtsave(t, "Milestones - AR6.png")
gtsave(t, "Milestones - AR6.html")

## create milestones table 2022 ----

milestones_22 <- whg_cfrm_scenarios |> 
  mutate(SLR = paste0(SLR, " ft")) |> 
  mutate(across(4:12, ~as.character(.))) |> 
  mutate(across(4:12, ~replace_na(., "n.c."))) |> 
  mutate(yr = paste0(pctl_50, "\n(", pctl_83, " to ", pctl_17, ")")) |> 
  select(1,2,3,13) |> 
  filter(source == "2022 U.S. Technical Report") |> 
  pivot_wider(names_from = scenario, values_from = yr)

t22 <- milestones_22 |> 
  select(-2) |> 
  gt() |> 
  # tab_header(
  #   title = md("**2022 Interagency Techical Report milestones for relative sea-level rise values used in the New Hampshire Coastal Flood Risk Model.** Tabular presentation of relative sea-level rise milestones at Seavey Island for five sea-level rise scenarios. The values shown are medians and, in parentheses, the 17th to 83rd percentiles. An entry n.c. means that the target sea-level rise value is not crossed during the period 2000–2150.")
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
  opt_align_table_header(align = "left") |>
  opt_table_font(size = 11, font = "Roboto Condensed") |>
  tab_options(table.width = px(600)) |> 
  opt_table_font(size = 11) |>
  opt_row_striping()

t22
gtsave(t22, "Milestones - 2022.png")
gtsave(t22, "Milestones - 2022.html")


# FLEXTABLE VERSIONS work better for Word docx format ----

library(flextable)
library(officer)

## create milestones table AR6 ----

milestones_ar6 <- whg_cfrm_scenarios |> 
  mutate(SLR = paste0(SLR, " ft")) |> 
  mutate(across(4:12, ~as.character(.))) |> 
  mutate(across(4:12, ~replace_na(., "n.c."))) |> 
  mutate(yr = paste0(pctl_50, "\n[", pctl_83, " to ", pctl_17, "]")) |> 
  select(1,2,3,13) |> 
  filter(source == "IPCC AR6") |> 
  pivot_wider(names_from = scenario, values_from = yr)

tar6 <- milestones_ar6 |> 
  select(1,3,5,6,7,8,4,9) |> 
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

tar6

save_as_docx(tar6, path = "Milestones - AR6.docx")

## create milestones table 2022 ----

milestones_22 <- whg_cfrm_scenarios |> 
  mutate(SLR = paste0(SLR, " ft")) |> 
  mutate(across(4:12, ~as.character(.))) |> 
  mutate(across(4:12, ~replace_na(., "n.c."))) |> 
  mutate(yr = paste0(pctl_50, "\n(", pctl_83, " to ", pctl_17, ")")) |> 
  select(1,2,3,13) |> 
  filter(source == "2022 U.S. Technical Report") |> 
  pivot_wider(names_from = scenario, values_from = yr)

tm22 <- milestones_22 |> 
  select(-2) |> 
  flextable() |> 
  theme_vanilla() |>
  flextable::align(align = "center", part = "all") |>
  style(pr_t = fp_text(font.family = "Roboto Condensed", font.size = 9),
        part = "all") |>
  bold(j=1) |> 
  bold(part = "header") |> 
  set_table_properties(width = 1, layout = "fixed")

tm22

save_as_docx(tm22, path = "Milestones - 2022.docx")
