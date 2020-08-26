library(tidyverse)

avatar <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-11/avatar.csv') %>%
  mutate(episode = ifelse(book_num == 2, chapter_num +20, ifelse(book_num == 3, chapter_num +40, chapter_num)))

main_talkers <- avatar %>%
  filter(character != "Scene Description") %>%
  group_by(character) %>%
  summarise(lines = n()) %>%
  top_n(9, lines)

avatar %>%
  filter(character %in% main_talkers$character) %>%
  group_by(book_num, chapter_num, character) %>%
  summarise(rating = mean(imdb_rating), lines = n()) %>%
  ggplot(aes(x = lines, y = rating)) +
  geom_point() +
  geom_smooth(method = "lm") +
  scale_y_continuous(limits = c(5,10)) +
  facet_wrap(~character, scales = "free_x")

avatar %>%
  group_by(episode) %>%
  summarise(rating = mean(imdb_rating, na.rm = TRUE)) %>%
  ggplot(aes(x = episode, y = rating)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_y_continuous(limits = c(0,10)) +
  scale_x_continuous(limits = c(1,61))
