### loading the libraries and data

library(extrafont)
library(ggrepel)
library(maps)
library(tidyverse)
theme_set(theme_minimal())

awards <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-20/beer_awards.csv')

### exploring the data

summary(awards)

ggplot(awards, aes(x = year)) +
  geom_bar(stat = "count", aes(fill = medal)) +
  labs(title = "Number of medals awarded per year")

awards %>%
  group_by(year) %>%
  summarise(count = n_distinct(category)) %>%
  ggplot(aes(x = year, y = count)) +
  geom_col() +
  labs(title = "Number of categories per year")

awards %>%
  group_by(state, medal) %>%
  tally() %>%
  ggplot(aes(x = reorder(state, -n), y = n)) +
  geom_col(aes(fill = medal)) +
  labs(title = "States by medals received")

awards %>%
  group_by(brewery, medal) %>%
  tally() %>%
  ggplot(aes(x = reorder(brewery, -n), y = n)) +
  geom_col(aes(fill = medal)) +
  labs(title = "Brewery by medals received")

### visualisation

us_cities <- read_csv("uscities.csv")

MainStates <- map_data("state")

cities <- awards %>%
  mutate(points = ifelse(medal == "Gold", 3,
                         ifelse(medal == "Silver", 2, 1))) %>%
  group_by(city, state) %>%
  summarise(points = sum(points)) %>%
  rename("state_id" = "state") %>%
  inner_join(us_cities, by = c("city", "state_id")) %>%
  select(city, lat, lng, points, state_id)

cities %>%
  filter(lng > -130) %>%
  ggplot(aes(x = lng, y = lat)) +
  geom_polygon(data=MainStates, aes(x=long, y=lat, group=group),
                color="black", fill = "#d5985c", alpha = 0.8, size = 0.5) +
  geom_point(aes(alpha = points), colour = "#808000", size = 2) +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        plot.title = element_text(family = "Gadugi",
                                  size = 8,
                                  face = "bold"),
        plot.subtitle = element_text(family = "Gadugi",
                                  size = 6),
        text = element_text(family = "Gadugi"),
        plot.caption = element_text(family = "Gadugi",
                                    size = 4),
        plot.background = element_rect(fill = "grey95")) +
  geom_text_repel(aes(label = ifelse(points > 50, city, "")), size = 2) +
  labs(title = "Top beer producing cities of the United States",
         subtitle = "Each year, GABF represents the largest collection of U.S. beer ever served, in a public tasting event plus a private competition. Five\ndifferent three-hour judging sessions take place over the three-day period during the week of the festival. Judges are assigned beers\nto evaluate in their specific area of expertise and never judge their own product or any product in which they have a concern.\n\nCities are mapped here based on number of medals won. The strength of colour is determined by the number of medals won,\nwith greater weighting towards Gold Medals.",
       caption = "Source: https://github.com/rfordatascience\n Twitter: @henrywrover2 | Github: henrywrover\n 20th October 2020") +
  ggsave(filename = "beer_cities.png", height = 6, width = 8, dpi = 300, type = "cairo-png")
