
library(magick)
library(ggpath)
library(tidyverse)
library(grid)
library(crayon)
library(glue)
library(ggforce)

flame <- "dev/images/flame.png"

pal <- eyedroppeR::eyedropper(3, flame)

ggplot() +
  geom_from_path(aes(0, 0, path = flame))

df_torch <- record_coords("flame")

df_torch |>
  mutate(
    x = scale_coords(x),
    y = scale_coords(y)
  ) |>
  ggplot(aes(x, y)) +
  geom_point() +
  geom_polygon(fill = NA, colour = "black") +
  geom_bspline0() +
  xlim(-1, 2)


df1 <- map_dfr(1:10, ~{
  df |>
    mutate(
      x = (x-0.5)*1/.x,
      y = (y-0.5)*1/.x,
      name = .x
    )
})

df1 |>
  ggplot(aes(x, y, group = name)) +
  geom_bspline_closed0(fill = NA, colour = "black")



# get fire data -----------------------------------------------------------

library(rvest)
library(janitor)
library(tidyverse)

url <- "https://www.iii.org/fact-statistic/facts-statistics-wildfires"

page <- read_html(url)
tbls <- page |>
  html_table()

df_state <- tibble(
  state = state.name,
  code = state.abb,
  area = state.area
)

df_bushfires <- tbls[[4]] |>
  clean_names() |>
  left_join(df_state, by = "state") |>
  rename(
    n_fires = number_of_fires,
    n_acres_burned = number_of_acres_burned
  ) |>
  mutate(
    n_fires = as.numeric(str_remove_all(n_fires, ",")),
    n_acres_burned = as.numeric(str_remove_all(n_acres_burned, ",")),
    p_burned = n_acres_burned/(area/0.0015625),
    p_burned_scl = p_burned/max(p_burned, na.rm = TRUE),
    cat_burned = case_when(
      p_burned > 0.02 ~ 5,
      p_burned > 0.015 ~ 4,
      p_burned > 0.01 ~ 3,
      p_burned > 0.005 ~ 2,
      TRUE ~ 1
    )
  ) |>
  drop_na()

# flame -------------------------------------------------------------------

library(geofacet)

pal <- c(Outer = '#f3705a', Inner = '#ffd15c')

df_flame1 <- df_flame |>
  mutate(
    id = 1:n(),
    x = x-0.5,
    layer = "Outer"
  ) |>
  bind_rows(
    df_flame |>
      mutate(
        id = 1:n(),
        x = (x-0.5)*0.6,
        y = y*0.6,
        layer = "Inner"
      )
  ) |>
  mutate(layer = factor(layer, levels = c("Outer", "Inner")))

df_base <- map_dfr(df_state$code, ~{
  dat <- df_bushfires |>
    filter(code == .x)
  df_flame1 |>
    mutate(
      x = x*dat$cat_burned,
      y = y*dat$cat_burned,
      code = dat$code,
      state = dat$state
    )
})

df_legend <- tribble(
  ~x, ~y, ~id, ~layer, ~code, ~state,
  NA, NA, NA, NA, "LG", "Legend"
)

df_facets <- us_state_grid1 |>
  bind_rows(
    tibble(
      row = 8,
      col = 6,
      code = "LG",
      name = "Legend"
    )
  )

df_base |>
  ggplot() +
  geom_bspline_closed0(aes(x, y, fill = layer), colour = NA) +
  # geom_text(aes(0, 2.5, label = "LEGEND"), df_legend) +
  facet_geo(~state, grid = "us_state_grid1") +
  scale_fill_manual(values = pal) +
  xlim(-4, 4) +
  theme_void() +
  theme(
    plot.margin = margin(t=60, b=60, l=60, r=60),
    legend.position = "none"
  )

