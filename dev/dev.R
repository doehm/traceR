
# click strat
library(magick)
library(imager)
library(tidyverse)
library(scales)
library(grid)
library(crayon)
library(glue)
library(cowplot)
library(ggforce)

torch <- load.image("C:/Users/danie/Google Drive/R Code/my-packages/torch/torch-dots.png")
end <- load.image("C:/Users/danie/Google Drive/R Code/my-packages/torch/click-to-end.png")

record_coords <- function(name, scale = FALSE) {
  df <- NULL
  x <- 10
  y <- 10
  k <- 0
  while(x > 9 & y > 9) {
    k = k + 1
    coords <- grid.locator()
    x <- as.numeric(str_extract(coords$x, "[:digit:]+"))
    y <- as.numeric(str_extract(coords$y, "[:digit:]+"))
    cat(green(glue("counter : {k}\r")))
    df <- df %>%
      bind_rows(tibble(x = x, y = -y))
  }
  cat(green(glue("trace killed / {k} traced points\n")))
  df %>%
    filter(x > 9 & y < -9) %>%
    mutate(name = name) %>%
    if(scale) {
      mutate(.,
             x = scale_01(x),
             y = scale_01(y)
      )
    } else .
}

plot(torch)

torch_df <- record_coords("flame")
handle <- record_coords(name = "handle")
notch <- record_coords(name = "notch")

scale_01 <- function(x, a = 1) {
  a*(x - min(x))/(max(x) - min(x))
}

input_df <- torch_df %>%
  bind_rows(handle) %>%
  bind_rows(notch) %>%
  mutate(
    x = scale_01(x),
    y = scale_01(y, 2),
    x_inner = x/2 + 0.25,
    y_inner = y/2 + 0.3
  )

handle_col <- rgb(181, 115, 26, maxColorValue = 255)
handle_col_accent <- rgb(181, 105, 26, maxColorValue = 255)
handle_col_accent <- rgb(255, 176, 32, maxColorValue = 255)
ggplot() +
  # geom_point(aes(x = x, y = y)) +
  # geom_bspline(aes(x = x, y = y), n = 200) +
  geom_bspline_closed(
    data = filter(input_df, name == "flame"),
    mapping = aes(x = x, y = y),
    fill = "darkorange2"
  ) +
  geom_bspline_closed(
    data = filter(input_df, name == "flame"),
    aes(x = x_inner, y = y_inner),
    fill = "yellow"
  ) +
  geom_polygon(
    data = filter(input_df, name == "handle"),
    mapping = aes(x = x, y = y),
    fill = handle_col
  ) +
  geom_polygon(
    data = filter(input_df, name == "notch"),
    mapping = aes(x = x + 0.1, y = y + 0.4),
    fill = "black"
  ) +
  geom_polygon(
    data = filter(input_df, name == "notch"),
    mapping = aes(x = x + 0.15, y = y + 0.3),
    fill = "black"
  ) +
  geom_segment(
    mapping = aes(x = 0.1, xend = 0.9, y = 0.53, yend = 0.53),
    colour = handle_col_accent, size = 20
  ) +
  geom_point(
    data = tibble(x0 = c(0.1, 0.9), y = c(0.53, 0.53)),
    mapping = aes(x = x0, y = y),
    colour = handle_col_accent, size = 19
  ) +
  coord_cartesian(clip = "off") +
  theme_void()
