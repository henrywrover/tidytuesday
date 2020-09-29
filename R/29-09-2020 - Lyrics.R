library(tidyverse)
library(tidytext)
library(patchwork)
library(extrafont)
library(ggdark)
theme_set(dark_theme_minimal())

beyonce_lyrics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-29/beyonce_lyrics.csv')

all_lyrics <- beyonce_lyrics %>%
  group_by(song_id) %>%
  mutate(lines_pct = song_line / max(song_line)*100,
         completed = round(lines_pct, -1)) %>%
  unnest_tokens(word, line) %>%
  filter(!word %in% stop_words$word) %>%
  inner_join(get_sentiments("afinn"))

### Beyonce's lyrics get more positive towards the end of the song

p1 <- beyonce_lyrics %>%
  group_by(song_id) %>%
  mutate(lines_pct = song_line / max(song_line)*100,
         completed = round(lines_pct, -1)/100) %>%
  unnest_tokens(word, line) %>%
  filter(!word %in% stop_words$word) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(completed) %>%
  summarise(avg = mean(value)) %>%
  ungroup() %>%
  mutate(rolling = cumsum(avg) / seq_along(avg)) %>%
  ggplot(aes(x = completed, y = rolling)) +
  geom_col(aes(x = completed, y = avg), alpha = 0.4, fill = "firebrick") +
  geom_line(colour = "deepskyblue3", size = 1.5) +
  labs(title = "Rolling average of positivity in Beyonce's lyrics",
       subtitle = "Columns show average posivity per percentage of song completed",
       x = "Percentage of song completed",
       y = "Mean positivity in lyrics") +
  scale_x_continuous(labels = scales::percent_format()) +
  theme(text = element_text(family = "Gadugi"),
        plot.title = element_text(face = "bold"),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = "black",
                                       colour = "black"))

### 100% Stacked column

p2 <- beyonce_lyrics %>%
  group_by(song_id) %>%
  mutate(lines_pct = song_line / max(song_line)*100,
         completed = round(lines_pct, -1)/100) %>%
  unnest_tokens(word, line) %>%
  filter(!word %in% stop_words$word) %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive", "negative")) %>%
  mutate(sentiment = str_to_sentence(sentiment)) %>%
  group_by(completed, sentiment) %>%
  tally() %>%
  group_by(completed) %>%
  mutate(as_pct = n / sum(n)) %>%
  ggplot(aes(x = completed, y = as_pct)) +
  geom_col(aes(fill = sentiment)) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Distribution of sentiments throughout Beyonce's songs",
       x = "Percentage of song completed",
       y = "Percentage of sentiments per completion %",
       fill = "") +
  theme(panel.grid = element_blank(),
        legend.position = "right") +
  theme(text = element_text(family = "Gadugi"),
        plot.title = element_text(face = "bold"),
        plot.background = element_rect(fill = "black",
                                       colour = "black"))

### Line chart to show different sentiments at song completion

p3 <- beyonce_lyrics %>%
  group_by(song_id) %>%
  mutate(lines_pct = song_line / max(song_line)*100,
         completed = round(lines_pct, -1)/100) %>%
  unnest_tokens(word, line) %>%
  filter(!word %in% stop_words$word) %>%
  inner_join(get_sentiments("nrc")) %>%
  filter(!sentiment %in% c("positive", "negative")) %>%
  mutate(sentiment = str_to_sentence(sentiment)) %>%
  group_by(completed, sentiment) %>%
  tally() %>%
  group_by(sentiment) %>%
  mutate(as_pct = n / sum(n)) %>%
  ggplot(aes(x = completed, y = as_pct)) +
  geom_col(aes(fill = sentiment), size = 1.5) +
  facet_wrap(~sentiment, ncol = 2) +
  scale_x_continuous(labels = scales::percent_format()) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Where is each sentiment located within beyonce's songs?",
       x = "Percentage of song completed",
       y = "Percentage of sentiment within completion %") +
  theme(legend.position = "none",
        text = element_text(family = "Gadugi"),
        plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        axis.text.y = element_blank(),
        plot.background = element_rect(fill = "black",
                                 colour = "black"))

### Patchwork Plot

plot <- (p1 / p2 | p3) +
  plot_annotation(title = "Beyonce Sentiment Analysis",
    subtitle = "Does the sentiment of Beyonce's lyrics change over the course of a song?",
    theme = theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 24, family = "Gadugi"),
                  plot.subtitle = element_text(hjust = 0.5, size = 12, family = "Gadugi"),
                  panel.background = element_rect(fill = "black")))

ggsave(plot, filename = "beyonce_plot.png", height = 8, width = 13, dpi = 120, type = "cairo-png")
