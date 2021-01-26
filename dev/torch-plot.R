
torch_data <- list(
  torch = torch,
  handle = handle,
  shadow = shadow
)
write_rds(torch_data, "C:/Users/Dan/Google Drive/R Code/my-packages/torch/torch-data.rds")

torch_data <- read_rds("C:/Users/Dan/Google Drive/R Code/my-packages/torch/torch-data.rds")
torch <- torch_data$torch
handle <- torch_data$handle
shadow <- torch_data$shadow

torch <- record_coords("flame")

library(extrafont)
loadfonts()

cast_df <- castaways %>%
  mutate(
    total_votes_received = replace_na(total_votes_received, 0),
    immunity_idols_won = replace_na(immunity_idols_won, 0)
  ) %>%
  filter(result == "Sole Survivor")

loc_plot <- file.path("C:/Users", ifelse(str_detect(getwd(), "Dan"), "Dan", "danie"), "Google Drive/R Code/my-packages/torch/torch-plot")


max_immunity <- 5
flame_bottom <- 7
handle_top <- 7.4
flame_ht <- 2
rope_left <- -0.15
rope_right <- 0.17
rope_size <- 2
rope_wt <- 0.35
rope_size_l <- 5

handle_col <- rgb(73, 55, 58, maxColorValue = 255)
handle_dec_col <- rgb(152, 125, 80, maxColorValue = 255)

cap <- function(x, lower, upper) {
  x <- as.double(x)
  case_when(
    x < lower ~ lower,
    x > upper ~ upper,
    TRUE ~ x
  )
}

immunity_vec <- cast_df$immunity_idols_won
season_vec <- cast_df$season

input_df <- map_dfr(season_vec, ~{
  torch %>%
    # mutate(n_immunity = .x) %>%
    bind_cols(
      cast_df %>%
        select(season_name, season, total_votes_received, immunity_idols_won, castaway, full_name) %>%
        filter(season == .x)
      )
}) %>%
  group_by(season) %>%
  mutate(
    x = (scale_coords(x)-0.5)*max(immunity_idols_won, 3)/5,
    y = scale_coords(y)*immunity_idols_won*flame_ht,
    # cap = cap(n_immunity/4, 1.1, 1.15),
    # x = x*cap,
    x_inner = x/2,
    y_inner = y/2,
    y = y + flame_bottom,
    y_inner = y_inner + flame_bottom
  )

handle_df <- map_dfr(season_vec, ~{
  handle %>%
    mutate(
      x = scale_coords(x)-0.5,
      y = handle_top*scale_coords(y),
      x = 0.7*x
    ) %>%
    bind_cols(
      cast_df %>%
        select(season_name, season, total_votes_received, immunity_idols_won, castaway, full_name) %>%
        filter(season == .x)
    )
})

# pole_df <- map_dfr(immunity_vec, ~{
#   pole %>%
#     mutate(
#       n_immunity = .x,
#       x = scale_coords(x)-0.5,
#       y = 4*scale_coords(y)-4,
#       x = 0.5*x
#     ) %>%
#     bind_cols(
#       cast_df %>%
#         select(season_name, season, total_votes_received, immunity_idols_won) %>%
#         filter(season == .x)
#     )
# })

ropes <- map_dfr(season_vec, ~{
  votes <- cast_df$total_votes_received[which(cast_df$season == .x)]
  if(votes == 0) {
    NULL
  } else {
    tibble(
      x = rope_left,
      xend = rope_right,
      y = (seq(votes, 1, -1) - votes - 0.2)/1.5
    ) %>%
      bind_cols(
        cast_df %>%
          select(season_name, season, total_votes_received, immunity_idols_won, castaway, full_name) %>%
          filter(season == .x)
      )
  }
})

ropes_top <- ropes %>%
  group_by(season) %>%
  slice(1)

shadow_df <- map_dfr(season_vec, ~{
  shadow %>%
    mutate(
      x = (scale_coords(x)-3)*0.1 + 0.05,
      y = scale_coords(y)*6,
      season = .x
    )
})


ft <- "Verdana Pro Light"
ft_b <- "Verdana Pro Cond Black"

# ft <- "survivor"
ft_b <- "survivor"
ft_size_title <- 18
ft_size_subtitle <- 20

titles <- str_extract(str_to_title(cast_df$season_name), "(?<=Survivor: ).+")
names(titles) <- 40:1

ropes %>%
  filter(season == 22) %>%
  filter(y == max(y) | y == min(y))

input_df %>%
  filter(
    season == 22,
    y == max(y) | y == min(y)
    ) %>%
  distinct(y)

bars <- tribble(
  ~x, ~xend, ~y, ~yend,
  0.5, 0.6, 7, 7,
  0.5, 0.6, 15, 15,
  0.6, 0.6, 7, 15,

  0.5, 0.6, -5.6, -5.6,
  0.5, 0.6, -0, -0,
  0.6, 0.6, -5.6, -0,
) %>%
  mutate(season = 22)

legend_text <- tribble(
  ~x, ~y, ~label,
  0.75, 12, str_wrap("The height of the flame indicates the number of individual immunity challenges won", 20),
  0.75, -2.7, str_wrap("The number of rope bindings indicates the number of votes received throughout the game including nullified votes from hidden immunity idols", 20)
) %>%
  mutate(season = 22)

subtitle_text <- "There are many paths to victory and being crowned the Sole Survivor. Some castaways are dominant in
immunity challenges, some value loyalty where they take some heat but always have the numbers on their side. Others fly well
under the radar, start fires from afar and don't receive a single vote. Perhaps most Sole Survivors are some
combination of the above. The dynamic nature of the game is what makes it so great." %>%
  str_wrap(60)

legend <- input_df %>%
  filter(season == 22) %>%
  ggplot(aes(x = x, y = y, group = season)) +
  geom_bspline_closed(fill = "darkorange2") +
  geom_bspline_closed(aes(x = x_inner, y = y_inner), fill = "yellow") +
  geom_rect(mapping = aes(xmin = rope_left, xmax = rope_right, ymin = -7, ymax = 1), fill = handle_col, colour = "black") +
  geom_bspline_closed(data = filter(handle_df, season == 22), fill = handle_col, colour = "black") +
  geom_segment(data = filter(ropes, season == 22), mapping = aes(x = x, xend = xend, y = y, yend = y), colour = "tan", size = rope_size_l) +
  geom_point(data = filter(ropes, season == 22), mapping = aes(x = x, y = y), colour = "tan", size = rope_size_l-1) +
  geom_point(data = filter(ropes, season == 22), mapping = aes(x = xend, y = y), colour = "tan", size = rope_size_l-1) +
  geom_segment(data = filter(ropes, season == 22), mapping = aes(x = x, xend = xend, y = y-rope_wt, yend = y-rope_wt)) +
  geom_segment(data = filter(ropes_top, season == 22), mapping = aes(x = x, xend = xend, y = y+rope_wt, yend = y+rope_wt)) +
  geom_polygon(data = shadow_df, fill = rgb(169, 153, 133, maxColorValue = 255)) +
  geom_segment(data = bars, mapping = aes(x = x, xend = xend, y = y, yend = yend)) +
  geom_text(data = legend_text, mapping = aes(x = x, y = y, label = label), family = ft, size = 4, lineheight = 0.9, hjust = 0) +
  geom_text(x = -0.6, y = 24, label = "Sole Survivors", family = ft_b, size = ft_size_title, hjust = 0) +
  geom_text(x = -0.6, y = 20, label = subtitle_text, family = ft, size = 4, hjust = 0, lineheight = 0.9) +
  theme_void() +
  coord_cartesian(clip = "off")

legend_small_df <-input_df %>%
  filter(season %in% c(38, 37, 23, 40, 10))

legend_small <- legend_small_df %>%
  ggplot(aes(x = x, y = y, group = season)) +
  geom_bspline_closed(fill = "darkorange2") +
  geom_bspline_closed(aes(x = x_inner, y = y_inner), fill = "yellow") +
  facet_wrap(~immunity_idols_won, ncol = 5, strip.position = "bottom") +
  theme_void() +
  theme(
    strip.text = element_text(family = ft)
  ) +
  coord_cartesian(clip = "off")


main <- input_df %>%
  ggplot(aes(x = x, y = y, group = season)) +
  geom_bspline_closed(fill = "darkorange2") +
  geom_bspline_closed(aes(x = x_inner, y = y_inner), fill = "yellow") +
  geom_rect(mapping = aes(xmin = rope_left, xmax = rope_right, ymin = -7, ymax = 1), fill = handle_col, colour = "black") +
  geom_bspline_closed(data = handle_df, fill = handle_col, colour = "black") +
  geom_segment(data = ropes, mapping = aes(x = x, xend = xend, y = y, yend = y), colour = "tan", size = rope_size) +
  geom_point(data = ropes, mapping = aes(x = x, y = y), colour = "tan", size = rope_size-1) +
  geom_point(data = ropes, mapping = aes(x = xend, y = y), colour = "tan", size = rope_size-1) +
  geom_segment(data = ropes, mapping = aes(x = x-0.01, xend = xend+0.01, y = y-rope_wt, yend = y-rope_wt)) +
  geom_segment(data = ropes_top, mapping = aes(x = x-0.01, xend = xend+0.01, y = y+rope_wt, yend = y+rope_wt)) +
  geom_polygon(data = shadow_df, fill = rgb(169, 153, 133, maxColorValue = 255)) +
  geom_text(data = cast_df, mapping = aes(x = 1.8, y = 5, label = str_wrap(full_name, 10)), family = ft_b, size = 5) +
  geom_text(data = cast_df, mapping = aes(x = 1.8, y = -3.7, label = str_wrap(titles, 15)), family = ft, size = 4, lineheight = 0.9) +
  coord_cartesian(clip = "off", xlim = c(-0.5, 3.5)) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = rgb(245, 221, 169, maxColorValue = 255)),
    plot.title = element_blank(),
    plot.subtitle = element_blank(),
    plot.caption = element_text(family = ft, size = 12, margin = margin(t = 20)),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 450),
    strip.text = element_blank()
  ) +
  labs(
    title = "Sole Survivors",
    subtitle = str_wrap("There are many paths to victory. Some castaways win immunity challenges, some don't receive a single vote. Temporary title for scale and quantity", width = 90),
    caption = "@danoehm / source: {survivoR} R package"
    ) +
  facet_wrap(~season, ncol = 8, labeller = labeller(season = titles))

{
  k <- max(str_pad(as.numeric(str_extract(list.files(loc_plot), "[:digit:]+")) + 1, width = 3, pad = "0"))
  path <<- glue("{loc_plot}/torch-plot {k}.png")
  cat(path, "\n")
  ggdraw(main) +
  # draw_image("C:/Users/Dan/Google Drive/R Code/my-packages/torch/background/old-paper-vector-texture-background.jpg", scale = 1.7) +
  # draw_plot(main) +
    draw_plot(legend, 0.03, 0.1, 0.15, 0.6) +
    draw_plot(legend_small, 0.05, 0.05, 0.11, 0.08) +
    ggsave(path, height = 12, width = 23)
  }
