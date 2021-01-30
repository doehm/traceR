
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
library(extrafont)
library(showtext)

font_add("coke", "C:/Users/Dan/Downloads/Fonts/loki_cola/LOKICOLA.ttf")
showtext_auto()

coke_bottle <- load.image("C:/Users/Dan/Pictures/R/coke.jpeg")
plot(coke_bottle)

font_import("C:/Users/Dan/Downloads/Fonts/loki_cola")


bottle <- record_coords("bottle")
cap <- record_coords("cap")
label <- record_coords("label")

df <- bottle %>%
  bind_rows(cap) %>%
  bind_rows(label) %>%
  mutate(
    x = scale_coords(x),
    y = scale_coords(y)
  )

coke_dark <- rgb(9, 16, 12, maxColorValue = 255)
coke_light <- rgb(140, 30, 13, maxColorValue = 255)

coke_ramp <- colorRampPalette(c(coke_light, coke_dark))(100)

coke %>%
  ggplot(aes(x = x, y = y)) +
  geom_bspline_closed0(
    data = dplyr::filter(coke, name == "bottle"),
    mapping = aes(x = x, y = y),
    alpha = 1, fill = coke_ramp[50]
  ) +
  geom_bspline_closed(data = filter(coke, name == "cap"), mapping = aes(x = x, y = y), colour = "black", fill = "red") +
  geom_polygon(data = filter(coke, name == "label"), mapping = aes(x = x, y = y), colour = "black", fill = "red") +
  geom_text(x = 0.5, y = 0.54, label = "Yeah Nah", family = "Loki Cola", size = 18, colour = "white") +
  theme_void()

n <- 1e5
shape <-
alpha <- runif(n, 0, 0.2)
x <- runif(n)
y <- alpha
k <- beta(n, )
bubble_colour <- c(rev(coke_ramp), coke_ramp)[]

bubbles <- tibble(
  alpha = alpha,
  x = x,
  y = y
)

