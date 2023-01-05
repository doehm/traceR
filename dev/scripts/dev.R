
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
library(survivoR)

torch <- load.image("C:/Users/Dan/Google Drive/R Code/my-packages/torch/torch-dots.png")
survivor_torch <- load.image("C:/Users/Dan/Google Drive/R Code/my-packages/torch/survivor-torch.jpg")
end <- load.image("C:/Users/danie/Google Drive/R Code/my-packages/torch/click-to-end.png")

plot(torch)
plot(survivor_torch)

torch_df <- record_coords("flame")
handle <- record_coords(name = "handle")
notch <- record_coords(name = "notch")
real_torch <- record_coords("real")
real_torch_dec <- record_coords("decorations")
pole <- record_coords("pole")
shadow <- record_coords("shadow")

input_df <- torch_df %>%
  bind_rows(handle) %>%
  # bind_rows(notch) %>%
  mutate(
    x = scale_coords(x),
    y = scale_coords(y),
    x_inner = x/2 + 0.25,
    y_inner = y/2 + 0.3
  )

handle_col <- rgb(181, 115, 26, maxColorValue = 255)
handle_col_accent <- rgb(181, 105, 26, maxColorValue = 255)
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
  # geom_polygon(
  #   data = filter(input_df, name == "notch"),
  #   mapping = aes(x = x + 0.1, y = y + 0.4),
  #   fill = "black"
  # ) +
  # geom_polygon(
  #   data = filter(input_df, name == "notch"),
  #   mapping = aes(x = x + 0.15, y = y + 0.3),
  #   fill = "black"
  # ) +
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


vote_history %>%
  inner_join(
    season_summary %>%
      select(season, winner),
    by = c("season", "vote" = "winner")
  ) %>%
  count(season, vote) %>%
  arrange(desc(n))

