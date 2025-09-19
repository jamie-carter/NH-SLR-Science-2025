library(readxl)
library(openxlsx)
library(tidyverse)
library(gt)
library(gtExtras)
library(flextable)
library(officer)

gmsl1 <- readxl::read_xlsx("AR6/GMSL contributions.xlsx", sheet = "Sheet1")
gmsl2 <- readxl::read_xlsx("AR6/GMSL contributions.xlsx", sheet = "Sheet2")

gmsl_contributions <- bind_rows(gmsl1, gmsl2)

# GT ----

tgmsl_contributions <- gt(gmsl_contributions) |> 
  # tab_header(
  #   title = md("**Observed contributions to global mean sea level (GMSL) for three time periods.** Contributions (mm) to GMSL from discrete sources, along with their respective percent contributions (%) and rates over time (mm/yr). Table adapted from Fox-Kemper et al., 2021.")
  # ) |> 
  tab_spanner(columns = starts_with("1901"), label = "1901-2018") |> 
  tab_spanner(columns = starts_with("1993"), label = "1993-2018") |> 
  tab_spanner(columns = starts_with("2006"), label = "2006-2018") |> 
  cols_label(c(2,5,8) ~ "mm") |> 
  cols_label(c(3,6,9) ~ "%") |> 
  cols_label(c(4,7,10) ~ "mm/yr") |> 
  sub_missing(missing_text = "---") |> 
  tab_style(
    style = cell_borders(sides = "top",
                         color = "darkgray",
                         weight = px(2)),
    locations = cells_body(columns = everything(), rows = 6)
  ) |> 
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
    columns = where(~is.numeric(.x)) 
  ) |> 
  gt_highlight_rows(
    rows = c(6,7),
    fill = "#80bcd8",
    alpha = .5
  ) |> 
  opt_align_table_header(align = "left") |>
  opt_table_font(size = 11, font = "Roboto Condensed") |>
  tab_options(table.width = px(600)) |> 
  opt_row_striping()

tgmsl_contributions

gtsave(tgmsl_contributions, "GMSL contributions.png")
gtsave(tgmsl_contributions, "GMSL contributions.html")

# FLEXTABLE ----

tgmsl_contributions_flex <- gmsl_contributions |> 
  flextable() |> 
  separate_header(split = "_") |> 
  set_header_labels(values = c(`1901–2018_mm` = "mm", `1901–2018_percent` = "%", `1901–2018_mm/year` = "mm/yr",
                               `1993–2018_mm` = "mm", `1993–2018_percent` = "%", `1993–2018_mm/year` = "mm/yr",
                               `2006–2018_mm` = "mm", `2006–2018_percent` = "%", `2006–2018_mm/year` = "mm/yr")) |> 
  style(pr_t = fp_text(font.family = "Roboto Condensed", font.size = 9),
        part = "all") |>
  bold(j=1) |>
  bg(i = c(6:7), part = "body", bg = "lightblue") |> 
  colformat_double(na_str = "–") |> 
  theme_vanilla() |>
  # set_table_properties(width = 1, layout = "autofit") |>
  flextable::align(j = c(2:10), align = "center", part = "header") |>
  flextable::align(align = "center", part = "all") |>
  flextable::align(j = 1, align = "left", part = "all") |>
  flextable::width(j = 1, width = 1.5) |> 
  flextable::width(j = c(2:10), width = .55) |> 
  vline(j = c(1,4,7)) |> 
  hline(i = 5, border = fp_border(width = 2, color = "darkgray"))

tgmsl_contributions_flex

save_as_docx(tgmsl_contributions_flex, path = "GMSL contributions.docx")
