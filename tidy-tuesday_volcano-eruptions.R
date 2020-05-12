# Load packages

library(tidyverse)
library(viridis)
library(cowplot)
library(patchwork)

# Get data

volcano <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv")
eruptions <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv")

# Prepare data frames

df1 <- eruptions %>%
  select(
    volcano_number,
    vei
  ) %>%
  left_join(
    volcano,
    by = "volcano_number"
  ) %>%
  select(
    vei,
    latitude,
    longitude,
    elevation
  ) %>%
  na.omit() %>%
  pivot_longer(
    -vei,
    names_to = "metric",
    values_to = "value"
  )

df2 <- eruptions %>%
  select(
    volcano_number,
    vei
  ) %>%
  left_join(
    volcano,
    by = "volcano_number"
  ) %>%
  select(
    vei,
    latitude,
    longitude
  ) %>%
  na.omit()

# Create scatter plot facets

p1 <- df1 %>%
  ggplot(aes(x = value, y = vei)) +
  geom_jitter(alpha = .25, aes(color = vei)) +
  scale_color_viridis(option = "D") +
  stat_smooth() +
  facet_wrap(~metric, scales = "free_x") +
  labs(
    y = "Volcanic Explosivity Index",
    x = "",
    title = "Does Volcano Explosiveness Relate to Geographic Location?"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    plot.title = element_text(face = "bold", hjust = .5, size = 18)
  ) +
  background_grid(major = "none")

# Create map

p2 <- ggplot() +
  borders("world", color = "gray80", fill = "gray80") +
  geom_point(data = df2, aes(x = longitude, y = latitude, fill = vei), alpha = .7, size = 4, pch = 21) +
  scale_fill_viridis(option = "D", "Volcanic Explosivity Index") +
  labs(caption = "Source: @R4DScommunity | Chart: @_sethdobson") +
  theme_nothing() +
  theme(plot.caption = element_text(size = 10))

# Combine plots

p1 / p2