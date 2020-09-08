library(tidyverse)
library(wesanderson)
library(extrafont)
library(showtext)
theme_set(theme_minimal())

friends <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends.csv')
friends_emotions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_emotions.csv')
friends_info <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_info.csv')

showtext_auto()
font_add(family = "friends", regular = "GABRWFFR.TTF")
font_add(family = "montserrat", regular = "Montserrat-Regular.ttf")

normalizer <- max(friends_info$us_views_millions) / max(friends_info$imdb_rating)

plot <- friends_info %>%
  group_by(season) %>%
  summarise(mean_rating = mean(imdb_rating, na.rm = TRUE),
            mean_views = mean(us_views_millions, na.rm = TRUE)) %>%
  ggplot(aes(x = season, y = mean_rating)) +
  geom_hline(yintercept = mean(friends_info$imdb_rating, na.rm = TRUE), colour = "deepskyblue4", alpha = 0.2, size = 0.8) +
  geom_point(data = friends_info, aes(x = season, y = imdb_rating, colour = imdb_rating), alpha = 0.8) +
  geom_line(size = 1.1) +
  scale_x_continuous(breaks = c(1:10)) +
  expand_limits(y = c(5,10)) +
  scale_colour_gradientn(colours = wes_palette("Zissou1", type = "continuous")) +
  theme(axis.ticks.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 36, family = "friends"),
        plot.subtitle = element_text(hjust = 0.5, size = 24, family = "montserrat", lineheight = 0.3),
        text = element_text(family = "montserrat", size = 24),
        plot.caption = element_text(size = 14, lineheight = 0.3)) +
  labs(title = "The One With The Incredible Consistency",
       subtitle = "First airing in 1994, Friends quickly cemented its place in history. While primarily a comedy, \n Friends left viewers gripped from week to week down to the excellent mixture of humour, tragedy and drama.",
       x = "Season",
       y = "IMDB Rating",
       caption = "Twitter: @henrywrover2 \n Code: github.com/henrywrover \n Data comes from the friends package via TidyTuesday")

ggsave("friends_two.png", plot = plot, type = "cairo-png", width = 7, height = 5)
