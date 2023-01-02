
# dino --------------------------------------------------------------------

library(ggpath)
library(showtext)

font_add_google("Permanent Marker", "perm")
font_add_google("Space Mono", "mm")
showtext_auto()

ggplot() +
  geom_from_path(aes(0, 0, path = "dev/images/dino.png"))

df_body <- trace_image(scale = FALSE)
df_leg1 <- trace_image(scale = FALSE)
df_leg2 <- trace_image(scale = FALSE)
df_arm1 <- trace_image(scale = FALSE)
df_arm2 <- trace_image(scale = FALSE)
df_mouth <- trace_image(scale = FALSE)
df_mouth1 <- trace_image(scale = FALSE)
df_eyes <- trace_image(scale = FALSE)
df_nose <- trace_image(scale = FALSE)

df_base <- bind_rows(
  "body" = df_body,
  "leg1" = df_leg1,
  "leg2" = df_leg2,
  "arm1" = df_arm1,
  "arm2" = df_arm2,
  "mouth" = df_mouth,
  # "mouth1" = df_mouth1,
  .id = "layer"
)

df_eyes <- df_eyes |>
  mutate(layer = "eyes")

df_nose <- df_nose |>
  mutate(layer = "nose")

df_mouth1 <- df_mouth1 |>
  mutate(layer = "mouth1")

df_base |>
  mutate(id = 1:n()) |>
  ggplot(aes(x, y, group = layer)) +
  geom_bspline_closed0(fill = "green4") +
  geom_bspline(size = 3) +
  geom_bspline(data = df_mouth1, size = 2) +
  geom_point(data = df_eyes, size = 5) +
  geom_point(data = df_nose, size = 2) +
  geom_point(size = 5) +
  geom_text(aes(label = id), size = 6, colour = "white") +
  geom_bspline_closed0(data = df_base |> filter(layer == "leg1"), fill = "green4") +
  geom_bspline(data = df_base |> filter(layer == "leg1"), size = 3) +
  geom_bspline_closed0(data = df_base |> filter(layer == "arm1"), fill = "green4") +
  geom_bspline(data = df_base |> filter(layer == "arm1"), size = 3) +
  annotate("text", x = 1410, y = 520, label = "RARR!!", family = "perm", size = 20, angle = 330) +
  xlim(300, 1800) +
  ylim(0, 1100) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#F5FCA6")
  )


cropcircles::hex_crop(
  "dev/images/ggdino.png",
  "dev/images/ggdino-hex.png"
)

cropcircles::hex_crop(
  "dev/images/black.png",
  "dev/images/black-hex.png"
)

library(hexSticker)

sticker(
  "dev/images/ggdino-hex.png",
  package = "traceR",
  p_family = "perm",
  p_x = 1,
  p_y = 1,
  filename = "dev/images/ggdino-hex-pkg.png",
)

ggplot() +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "black")
  )

ggplot() +
  geom_from_path(aes(0, 0, path = "dev/images/black-hex.png")) +
  geom_from_path(aes(0, 0, path = "dev/images/ggdino-hex.png"), width = 0.8) +
  annotate("text", x = 0.4, y = -0.67, label = "traceR", family = "mm", colour = "black", size = 64, angle = 30, fontface = "bold") +
  xlim(-1, 1) +
  ylim(-1, 1) +
  theme_void()

ggsave("dev/images/hex.png", width = 8, height = 8)


write_rds(df_base, "dev/data/df_base.rds")
write_rds(df_eyes, "dev/data/df_eyes.rds")
write_rds(df_nose, "dev/data/df_nose.rds")
write_rds(df_mouth1, "dev/data/df_mouth1.rds")
