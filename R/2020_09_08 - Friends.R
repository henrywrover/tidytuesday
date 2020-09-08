setwd("~/R/tidytuesday/R/friends")

library(ggtext)
library(extrafont)
library(ggthemes)
library(ggdark)
library(tidyverse)
library(showtext)
theme_set(theme_minimal())

friends <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends.csv')
friends_emotions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-08/friends_emotions.csv')
emotions <- merge(friends, friends_emotions, by = c("season", "episode", "scene", "utterance"))

showtext_auto()
font_add(family = "friends", regular = "GABRWFFR.TTF")
font_add(family = "montserrat", regular = "Montserrat-Regular.ttf")

coord_straightpolar <- function(theta = 'x', start = 0, direction = 1, clip = "on") {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") 
    "y"
  else "x"
  ggproto(NULL, CoordPolar, theta = theta, r = r, start = start,
          direction = sign(direction), clip = clip,
          # This is the different bit
          is_linear = function(){TRUE})
}

emotions %>%
  filter(speaker %in% c("Ross Geller", "Monica Geller", "Chandler Bing", "Joey Tribbiani", "Phoebe Buffay", "Rachel Green"),
         emotion != "Neutral") %>%
  group_by(speaker, emotion) %>%
  tally() %>%
  group_by(speaker) %>%
  mutate(emotions_percent = (n / sum(n))) %>%
  ggplot(aes(x=emotion, y=emotions_percent, group = 1)) + 
  geom_polygon(aes(colour = speaker), na.rm = FALSE, fill = NA, size = 1.2) +
  geom_point(aes(colour = speaker), size = 1.5) +
  facet_wrap(~speaker) +
  coord_straightpolar(theta = "x") + 
  dark_theme_minimal() +
  scale_colour_brewer(palette = "Set1") +
  theme(
    plot.caption = element_text(family = "montserrat", size = 10),
    axis.text.x = element_text(colour = "White", size = 10),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.x = element_blank(),
    legend.position = 'none',
    plot.title = element_text(vjust = 1.2,
                              hjust = 0.5,
                              size = 20),
    plot.subtitle = element_text(family = "montserrat",
                                 size = 12,
                                 hjust = 0.5),
    text = element_text(family = "friends"),
    strip.text = element_text(colour = "White", size = 16)) +
  labs(title = "Friends Character Emotions",
       subtitle = "Was Ross scared that often? Did Rachel really stay that mad? Here we can see the \n proportion of each characters' lines that are classified into six different emotions; \n Scared, Joyful, Sad, Mad, Powerful & Peaceful",
       caption = "Twitter: @henrywrover2 \n Code: github.com/henrywrover \n Data comes from the friends package via TidyTuesday")