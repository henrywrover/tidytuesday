library(tidyverse)
library(lubridate)
library(gganimate)
library(extrafont)
library(extrafontdb)
library(ggrepel)

wheels <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-09/wheels.csv') %>%
  select(-1)

### setting up the initial plot

plot <- wheels %>%
  filter(year(opened) >= 1984) %>%
  group_by(year(opened)) %>%
  summarise(avg_cap = mean(passengers_per_cabin, na.rm = TRUE)) %>%
  rename(year = 1) %>%
  filter(avg_cap >= 0) %>%
  ggplot(aes(x = year, y = 5)) +
  geom_hline(aes(yintercept = 5), size = 1, colour = "grey85") +
  geom_point(aes(size = avg_cap), colour = "#CC7D51") +
  theme_minimal() +
  theme(axis.text = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(size = 24, face = "bold", family= "Century Gothic", hjust = 0.5, colour = "#F8F8FF"),
        plot.background = element_rect(colour = "#08172E", fill = "#08172E"),
        plot.subtitle = element_text(size = 16, family= "Century Gothic", hjust = 0.5, colour = "#F8F8FF"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        legend.position = "none",
        plot.caption = element_text(size = 12, family= "Century Gothic", colour = "#F8F8FF")) +
  labs(x = "",
       y = "",
       title = "Have Ferris Wheel seating capacities changed\nin recent years?",
       subtitle = "\n\nEach point represents a construction year, with the size of the point\nshowing average passengers per Cabin\n\n\n{as.integer(frame_time)}",
       caption = "Data | @Emil_Hvitfeldt via TidyTuesday\nVisualisation | Henry Wakefield\nTwitter | @henrywrover2") +
  scale_size_continuous(range = c(2,12)) +
  coord_polar() +
  transition_time(year) +
  shadow_mark()

### creating animation

animate(plot, duration = 10, fps = 30, width = 600, height = 800, renderer = gifski_renderer(loop = TRUE), end_pause = 60)

### saving animation

anim_save("ferris.gif")
