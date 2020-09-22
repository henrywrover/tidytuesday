library(tidyverse)
library(extrafont)
library(ggdark)
theme_set(theme_minimal())

members <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/members.csv')
expeditions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/expeditions.csv')
peaks <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-22/peaks.csv')

members %>%
  filter(died == TRUE) %>%
  mutate(hired = ifelse(hired == TRUE, "Hired Staff", "Climber")) %>%
  ggplot(aes(x = peak_name, y = year)) +
  geom_point(aes(colour = hired), size = 1) +
  dark_theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = -0.1),
        text = element_text(colour = "white", family = "Gadugi"),
        plot.title = element_text(hjust = 0.5, size = 20, family = "Gadugi", face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 16, family = "Gadugi"),
        legend.position = "bottom") +
  scale_colour_manual(values = c("#1DA2DC", "#F9DF30")) +
  labs(title = "The Unfortunate Reality of Mountaineering",
       subtitle = "Over the course of 10,364 expeditions in the Himalayas 1,106 people have lost their lives. \n788 of these were part of the crew, while 318 deaths were from Sherpas hired by the expedition. Here they are \nrepresented by the year of their death and the peak on which they while attempting to climb.",
       colour = "",
       x = "Peak Name",
       y = "Year of Death") +
  ggsave(filename = "mountains.png", height = 8, width = 12, dpi = 300, type = "cairo-png")
