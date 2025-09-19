library(tidyverse)

mm2in <- function(x) (x * 0.0393701)

column_names <- c(
  'altimeter_type',
  'cycle', 
  'year',
  'observation_count', 
  'observation_count_weighted',
  'gmsl_variation',
  'gmsl_variation_std',
  'gmsl_variation_smooth',
  'gmsl_variation_with_gia',
  'gmsl_variation_with_gia_std',
  'gmsl_variation_with_gia_smooth',
  'gmsl_variation_with_gia_smooth_and_signals_removed',
  'gmsl_variation_without_gia_smooth_and_signals_removed')

GMSL_TPJAOS_5.2 <- read_table("MERGED_TP_J1_OSTM_OST_GMSL_ASCII_V52_5.2-20250318_190504/GMSL_TPJAOS_5.2.txt",
                              skip = 52, col_names = column_names, na = "99900.000")

p.GMSL_TPJAOS_5.2 <- ggplot(GMSL_TPJAOS_5.2) +
  geom_smooth(aes(year, gmsl_variation_with_gia_smooth_and_signals_removed), formula = y ~ poly(x, 2), se = F,
              linewidth = .5) +
  geom_line(aes(year, gmsl_variation_with_gia_smooth_and_signals_removed), linewidth = 1) +
  # geom_line(aes(year, gmsl_variation_with_gia_smooth), linewidth = 1) +
  scale_y_continuous(sec.axis = sec_axis(~mm2in(.x), breaks = seq(-12,12,1), name = "Sea Height Variation (inches)")) +
  scale_x_continuous(breaks = c(1993, seq(2000,2030,5)), minor_breaks = seq(1990,2030,1)) +
  coord_cartesian(xlim = c(1993, 2025), ylim = c(-40,80), expand = FALSE) +
  # scale_color_cmocean(name = "thermal", direction = -1, discrete = TRUE) +
  # scale_fill_cmocean(name = "thermal", direction = -1, discrete = TRUE) +
  # scale_color_scico_d(palette = "glasgow", aesthetics = c("color", "fill"), direction = -1) +
  # scale_color_brewer(palette = "PRGn") +
  # scale_color_ordinal(direction = -1) +
  labs(
    # title = "Historic Sea-Level Rise",
    # color = NULL,
    x = NULL,
    y = "Sea Height Variation (mm)") +
  theme_bw() +
  theme(text = element_text(family = "Roboto Condensed", size = 12),
        plot.title = element_text(face = 'bold'),
        plot.subtitle = element_text(face = "italic"),
        legend.key.width = unit(1, "cm"),
        # legend.position = "bottom",
        legend.title = element_blank()
  )

p.GMSL_TPJAOS_5.2
plotly::ggplotly(p.GMSL_TPJAOS_5.2)
ggsave(plot = p.GMSL_TPJAOS_5.2,
       filename = "GMSL from satellite altimetry GMSL_TPJAOS_5.2.png", 
       width = 6.5, height = 3, dpi = 300, units = "in")
