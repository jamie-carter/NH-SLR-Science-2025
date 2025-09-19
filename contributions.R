library(tidync)
library(readxl)

font_add_google("Roboto Condensed")
showtext_auto()
showtext_opts(dpi = 300)

# functions ----

read_nh_contributions <- function(x) {
  read.xlsx("SeaLeveldotGOV/sealevel_explorer_data_NH.xlsx", sheet = x) |>
  mutate(across(6:19,~mm2ft(.x))) |>
  pivot_longer(cols = 6:19, names_to = "Year", values_to = "slr_ft") |> 
    mutate(Year = as.numeric(Year)) |> 
  select(-1)
}

read_ne_contributions <- function(x) {
  read.xlsx("SeaLeveldotGOV/sealevel_explorer_data_NE.xlsx", sheet = x) |>
    mutate(across(6:19,~mm2ft(.x))) |>
    pivot_longer(cols = 6:19, names_to = "Year", values_to = "slr_ft") |> 
    mutate(Year = as.numeric(Year)) |> 
    select(-1)
}
read_usa_contributions <- function(x) {
  read.xlsx("SeaLeveldotGOV/sealevel_explorer_data_USA.xlsx", sheet = x) |>
    mutate(across(6:19,~mm2ft(.x))) |>
    pivot_longer(cols = 6:19, names_to = "Year", values_to = "slr_ft") |> 
    mutate(Year = as.numeric(Year)) |> 
    select(-1)
}

# get NH data ----

sheets <- c(4:10)

nh_contributions <- sheets |> 
  map(read_nh_contributions) |> 
  bind_rows() |> 
  mutate(scenario = factor(scenario, levels = c("Low", "IntLow", "Int", "IntHigh", "High"),
                           labels = c("Low", "Intermediate-Low", "Intermediate", "Intermediate-High", "High"))) |> 
  mutate(process = factor(process, levels = c("verticallandmotion", "landwaterstorage", "glaciers", "GIS", "AIS", "oceandynamics", "total"),
                          labels = c("Vertical Land Motion", "Land Water Storage", "Glaciers", "Greenland Ice Sheet", "Antarctic Ice Sheet", "Ocean Dynamics", "Total"))) |> 
  mutate(geo = "NH")

contributions_Seavey <- nh_contributions %>%
  filter(process != "Total", quantile == 50, Year %in% c(2050,2100,2150)) %>%
  ggplot() +
  geom_col(aes(Units, slr_ft, fill = process)) +
  scale_fill_scico_d(palette = "batlow", direction = -1) +
  labs(title = "Individual Process Contributions",
       subtitle = "New Hampshire",
       x = NULL,
       y = "Sea level (ft)") +
  # guides(color = "none", size = "none", alpha = "none") +
  theme_bw() +
  facet_grid(rows = vars(Year), cols = vars(scenario), scales = "free_y") +
  # facet_wrap(~scenario, nrow = 1) +
  theme(text = element_text(family = "Roboto Condensed", size = 12),
        plot.title = element_text(face = 'bold', family = "Roboto"),
        plot.subtitle = element_text(face = "italic", family = "Roboto"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.key.width = unit(0.75, "cm"))

contributions_Seavey

#### Save Seavey contributions ----
ggsave(plot = contributions_Seavey,
       filename = "Contributions - Seavey.png", 
       width = 5, height = 4, units = "in", dpi = 300)

# ne contributions ----

ne_contributions <- sheets |> 
  map(read_ne_contributions) |> 
  bind_rows() |> 
  mutate(scenario = factor(scenario, levels = c("Low", "IntLow", "Int", "IntHigh", "High"),
                           labels = c("Low", "Intermediate-Low", "Intermediate", "Intermediate-High", "High"))) |> 
  mutate(process = factor(process, levels = c("verticallandmotion", "landwaterstorage", "glaciers", "GIS", "AIS", "oceandynamics", "total"),
                          labels = c("Vertical Land Motion", "Land Water Storage", "Glaciers", "Greenland Ice Sheet", "Antarctic Ice Sheet", "Ocean Dynamics", "Total"))) |> 
  mutate(geo = "NE")


# usa contributions ----

sheets <- c(4:10)

usa_contributions <- sheets |> 
  map(read_usa_contributions) |> 
  bind_rows() |> 
  mutate(scenario = factor(scenario, levels = c("Low", "IntLow", "Int", "IntHigh", "High"),
                           labels = c("Low", "Intermediate-Low", "Intermediate", "Intermediate-High", "High"))) |> 
  mutate(process = factor(process, levels = c("verticallandmotion", "landwaterstorage", "glaciers", "GIS", "AIS", "oceandynamics", "total"),
                          labels = c("Vertical Land Motion", "Land Water Storage", "Glaciers", "Greenland Ice Sheet", "Antarctic Ice Sheet", "Ocean Dynamics", "Total"))) |> 
  mutate(geo = "USA")

contributions <- bind_rows(usa_contributions, ne_contributions, nh_contributions) |>   mutate(geo = factor(geo, levels = c("NH", "NE", "USA")))

p.contributions <- contributions |> 
  filter(process != "Total", quantile == 50, Year == 2100, scenario == "Intermediate") |> 
  ggplot() +
  geom_hline(aes(yintercept = m2ft(1))) +
  geom_col(aes(geo, slr_ft, fill = process)) +
  scale_fill_scico_d(palette = "batlow", direction = -1, guide = guide_legend("Process")) +
  scale_y_continuous(sec.axis = sec_axis(~ . * 0.3048, breaks = 1, labels = "1m")) +
  labs(title = "Individual Process Contributions",
       subtitle = "Intermediate Scenario in 2100",
       x = NULL,
       y = "Sea level (ft)") +
  theme_bw() +
  # facet_grid(rows = vars(Year), cols = vars(scenario), scales = "free_y") +
  # facet_wrap(~scenario, nrow = 1) +
  theme(text = element_text(family = "Roboto Condensed", size = 12),
        plot.title = element_text(face = 'bold', family = "Roboto"),
        # plot.subtitle = element_text(face = "italic", family = "Roboto"),
        panel.grid.major.x = element_blank(),
        # axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        legend.key.width = unit(0.75, "cm"))

p.contributions

#### Save NH contributions ----
ggsave(plot = p.contributions,
       filename = "Contributions - NH and USA.png", 
       width = 6, height = 3, units = "in", dpi = 300)
