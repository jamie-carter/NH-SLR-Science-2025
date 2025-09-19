library(tidyverse)
library(scico)
library(cmocean)
library(sysfonts)
library(showtext)
library(ggstream)

mm2ft <- function(x) (x * 0.00328084)
cm2ft <- function(x) (x * 0.0328084)
m2ft <- function(x) (x * 3.28084)
m2in <- function(x) (x * 39.3701)
m2mm <- function(x) (x * 1000)
mm2m <- function(x) (x / 1000)
cm2m <- function(x) (x * .01)
ft2m <- function(x) (x * 0.3048)

# Add fonts ----
font_add_google("Roboto")
font_add_google("Roboto Condensed")
showtext_auto()
showtext_opts(dpi = 300)

# PLOT historis SLR ----

gmsl_20c <- read_delim("Frederikse/GMSL_20c.txt", col_names = c("year", "median", "lower", "upper"), delim = ";") |> mutate(across(2:4, ~ m2ft(.)))

usa_20c <- read_delim("Frederikse/USA_20c.txt", col_names = c("year", "median"), delim = ";") |> 
  mutate(median = m2ft(median))

p.historic <- ggplot() +
  geom_line(data=gmsl_20c, aes(year, median, color = "Global"), linewidth = 1) +
  geom_line(data=usa_20c, aes(year, median, color = "Contiguous U.S."), linewidth = 1) +
  scale_y_continuous(breaks = seq(-2, 2, .5),
                     sec.axis = sec_axis(~.*12, breaks = seq(-12,12,1), name = "Relative Sea Level (inches)")) +
  scale_x_continuous(breaks = seq(1900,2022,10)) +
  coord_cartesian(xlim = c(1900, 2020), expand = FALSE) +
  scale_color_manual(values = c("Global" = "blue", "Contiguous U.S." = "black")) +
  labs(
    # title = "Historic Sea-Level Rise",
    color = NULL,
    x = NULL,
    y = "Relative Sea Level (feet)") +
  theme_bw() +
  theme(text = element_text(family = "Roboto Condensed", size = 12),
        plot.title = element_text(face = 'bold'),
        plot.subtitle = element_text(face = "italic"),
        legend.key.width = unit(1, "cm"),
        legend.position = "bottom"
  )

p.historic

ggsave(plot = p.historic,
       filename = "Historic Sea Level Rise.png", 
       width = 6.5, height = 3, dpi = 300, units = "in")

# plot timeseries for interaction ----

plotly::ggplotly(p.historic)

# get linear rates from timeseries ----

rateGMSL = lm(median ~ year, data = gmsl_20c |> filter(year >= 1920) |> mutate(median = ft2m(median) * 1000))$coefficients[2]
rateUSA = lm(median ~ year, data = usa_20c |> mutate(median = ft2m(median) * 1000))$coefficients[2]

# check long-term GMSL rise rate ----

gm <- gmsl_20c |> 
  # filter(year >= 1926) |> 
  mutate(mm = m2mm(ft2m(median)))

tp <- GMSL_TPJAOS_5.2 |> 
  select(3,mm=12) |> 
  group_by(as.integer(year)) |> 
  summarise(mm = mean(mm)) |> 
  rename(year = 1)

gmtp <- full_join(gm, tp, by = "year") |> 
  rowwise() |> 
  mutate(diff = mm.x - mm.y)

mean(gmtp$diff, na.rm = T)

gmtp2 <- gmtp |> 
  mutate(mm.y = mm.y + 13.71) |> 
  # rowwise() |> 
  mutate(mm = case_when(is.na(mm.y) ~ mm.x,
                        .default = mm.y))

ggplot() +
  geom_line(data = gm, aes(year,mm)) +
  geom_line(data = tp, aes(year,mm)) +
  geom_line(data = gmtp2, aes(year, mm))

lm_gm <- lm(mm ~ year, data = gmtp2)
trend_gm <- lm_gm$coefficients[2]
trend_gm

# PLOT contributions ----

ais <- read_delim("Frederikse/Contributions/AIS.txt", col_names = c("year", "median", "lower", "upper"), delim = ";") |> mutate(across(2:4, ~ m2ft(.))) |> 
  mutate(process = "Antarctic Ice Sheet")
tws <- read_delim("Frederikse/Contributions/tws.txt", col_names = c("year", "median", "lower", "upper"), delim = ";") |> mutate(across(2:4, ~ m2ft(.))) |> 
  mutate(process = "Land-Water Storage")
steric <- read_delim("Frederikse/Contributions/steric.txt", col_names = c("year", "median", "lower", "upper"), delim = ";") |> mutate(across(2:4, ~ m2ft(.))) |> 
  mutate(process = "Thermosteric Expansion")
glac <- read_delim("Frederikse/Contributions/glac.txt", col_names = c("year", "median", "lower", "upper"), delim = ";") |> mutate(across(2:4, ~ m2ft(.))) |> 
  mutate(process = "Glaciers")
gris <- read_delim("Frederikse/Contributions/GrIS.txt", col_names = c("year", "median", "lower", "upper"), delim = ";") |> mutate(across(2:4, ~ m2ft(.))) |> 
  mutate(process = "Greenland Ice Sheet")
sum <- read_delim("Frederikse/Contributions/budget.txt", col_names = c("year", "median", "lower", "upper"), delim = ";") |> mutate(across(2:4, ~ m2ft(.))) |> 
  mutate(process = "Sum of processes")
gmsl <- read_delim("Frederikse/Contributions/GMSL.txt", col_names = c("year", "median", "lower", "upper"), delim = ";") |> mutate(across(2:4, ~ m2ft(.))) |> 
  mutate(process = "Observed GMSL")

processes <- bind_rows(ais, tws, steric, glac, gris, gmsl, sum) |> 
  mutate(process = factor(process, levels = c("Glaciers", "Antarctic Ice Sheet", "Greenland Ice Sheet", "Land-Water Storage", "Thermosteric Expansion", "Sum of processes", "Observed GMSL")))

p.contributions <- ggplot(processes) +
  geom_ribbon(aes(year, ymin = lower, ymax = upper, fill = process), alpha=.25) +
  geom_line(aes(year, median, color = process), linewidth = 1) +
  scale_y_continuous(breaks = seq(-2, 2, .5),
                     sec.axis = sec_axis(~.*12, breaks = seq(-12,12,1), name = "Relative Sea Level (inches)")) +
  scale_x_continuous(breaks = seq(1900,2022,10)) +
  coord_cartesian(xlim = c(1920, 2020), expand = FALSE) +
  scale_color_cmocean(name = "thermal", direction = -1, discrete = TRUE) +
  scale_fill_cmocean(name = "thermal", direction = -1, discrete = TRUE) +
  # scale_color_scico_d(palette = "lapaz", aesthetics = c("color", "fill")) +
  # scale_color_viridis_d(option = "F", direction = -1, aesthetics = c("colour", "fill")) +
  labs(
    # title = "Historic Sea-Level Rise",
    # color = NULL,
    x = NULL,
    y = "Relative Sea Level (feet)") +
  theme_bw() +
  theme(text = element_text(family = "Roboto Condensed", size = 12),
        plot.title = element_text(face = 'bold'),
        plot.subtitle = element_text(face = "italic"),
        legend.key.width = unit(1, "cm"),
        # legend.position = "inside",
        # legend.position.inside = c(.8,.2),
        legend.title = element_blank()
  )

p.contributions

ggsave(plot = p.contributions,
       filename = "Contributions to Global Mean Sea Level.png", 
       width = 6.5, height = 3, dpi = 300, units = "in")

# playing with area and ggstream ----

p.contributionsStacked <- ggplot(processes |> filter(!process %in% c("Observed GMSL", "Sum of processes"))) +
  geom_area(aes(year, median, fill = process), linewidth = 1) +
  # geom_stream(aes(year, median, fill = process), type = "ridge", bw = .25, sorting = "inside_out") +
  geom_line(data = processes |> filter(process == "Observed GMSL"), aes(year, median)) +
  scale_y_continuous(breaks = seq(-2, 2, .5),
                     sec.axis = sec_axis(~.*12, breaks = seq(-12,12,1), name = "Relative Sea Level (inches)")) +
  scale_x_continuous(breaks = seq(1900,2022,10)) +
  coord_cartesian(xlim = c(1920, 2020), expand = FALSE) +
  scale_color_cmocean(name = "thermal", direction = -1, discrete = TRUE) +
  scale_fill_cmocean(name = "thermal", direction = -1, discrete = TRUE) +
  # scale_color_scico_d(palette = "lapaz", aesthetics = c("color", "fill")) +
  # scale_color_viridis_d(option = "F", direction = -1, aesthetics = c("colour", "fill")) +
  labs(
    # title = "Historic Sea-Level Rise",
    # color = NULL,
    x = NULL,
    y = "Relative Sea Level (feet)") +
  theme_bw() +
  theme(text = element_text(family = "Roboto Condensed", size = 12),
        plot.title = element_text(face = 'bold'),
        plot.subtitle = element_text(face = "italic"),
        legend.key.width = unit(1, "cm"),
        # legend.position = "bottom",
        legend.title = element_blank()
  )

p.contributionsStacked

ggsave(plot = p.contributionsStacked,
       filename = "Contributions to Global Mean Sea Level Stacked.png", 
       width = 6.5, height = 3, dpi = 300, units = "in")
