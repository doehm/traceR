
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
