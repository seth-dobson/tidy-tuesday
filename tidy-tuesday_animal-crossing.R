# Load packages

library(tidyverse)
library(tidytext)
library(ggridges)
library(viridis)
library(cowplot)

# Get user reviews

df <- read_tsv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/user_reviews.tsv")

# Tokenize reviews

df_tokens <- df %>%
  unnest_tokens(
    output = word,
    input = text,
    token = "words"
  ) %>%
  anti_join(stop_words) %>%
  mutate(word = str_extract(word, "[a-z']+"))

# Get sentiment values

df_afinn <- df_tokens %>%
  inner_join(get_sentiments("afinn"))

# Get mean sentiment by user

df_final <- df_afinn %>%
  group_by(user_name) %>%
  summarise(mean_sentiment = mean(value)) %>%
  inner_join(df, by = "user_name") %>%
  select(-text)

# Create plot

p <- df_final %>%
  ggplot(aes(x = mean_sentiment, y = factor(grade))) +
  geom_density_ridges(aes(fill = grade), alpha = .75) +
  scale_fill_viridis() +
  theme_minimal_vgrid() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0),
    plot.caption = element_text(size = 8, face = "italic")
  ) +
  labs(
    title = "Do Positive Reviewers of Animal Crossing Grade Higher?",
    subtitle = "Based on 2,729 User Reviews",
    x = "Mean Sentiment of Review",
    y = "Reviewer Grade",
    caption = paste(
      "Source: @R4DScommunity, Villager DB | Chart: @_sethdobson",
      "Sentiment score based on AFINN lexicon",
      sep = "\n"
    )
  ) +
  xlim(-5, 5)

ggsave2("tidy-tuesday_animal-crossing.png", p, width = 7, height = 4, dpi = 300)
