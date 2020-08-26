library(tidyverse)
library(extrafont)
theme_set(theme_minimal())

chopped <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-25/chopped.tsv')

wrapper <- function(x, ...) paste(strwrap(x, ...), collapse = "\n")

#annotations etc
annotations <- data.frame(x1 = c(26,35,39,26.5,2.2),
                          x2 = c(28.8,33,40,25,1.1),
                          y1 = c(5.8,7,9.4,9,9.3),
                          y2 = c(5.5,7.4,8.9,8.4,8.9))
season33 <- "With an average episode rating of 7.46, season 33 is the lowest rated season of Chopped"
season29 <- "Season 29 Episode 13, \"Worst Cooks Challenge\", is regarded as the worst episode of the entire show"
season40 <- "Season 40 completed the redemption arc, boasting the best rating since Season 1"
season25 <- "Season 25 began a run of 9 successive seasons below the series average"
season1 <- "The first season still holds the highest average rating, with a score of 8.86"

#now for the actual exploration
plot <- chopped %>%
  filter(season > 0 & season < 41) %>%
  group_by(season) %>%
  summarise(rating = mean(episode_rating, na.rm = TRUE)) %>%
  ggplot(aes(x = season, y = rating)) +
  geom_point(data = chopped %>% filter(season < 41), aes(x = season, y = episode_rating, colour = episode_rating), alpha = 0.3) +
  geom_line(size = 1.1, colour = "grey35") +
  expand_limits(y = c(5,10),
                x = c(1,40)) +
  scale_x_continuous(breaks = 1:40, expand = c(0.01,0)) +
  labs(title = "Was Chopped heading for the chop?",
       subtitle = "In 2009 Chopped was first aired on US Television to positive acclaim. It would go on to receive excellent ratings throughout \n the first 24 seasons, and despite some rocky patches season ratings never dropped below 8. However, episode quality hit a \n steady decline in 2015 beginning with Season 25 and would go on for the next 8 seasons. In season 34 things began to turn around, \n and Chopped is now receiving ratings as high as it did when first released.",
       x = "Season",
       y = "IMDB Episode Rating",
       colour = "Episode Rating") +
  theme(axis.ticks.x = element_blank(),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5, size = 12, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 6),
        text = element_text(family = "Gadugi", size = 10)) +
  geom_curve(data = annotations,
             aes(x = x1, y = y1, xend = x2, yend = y2),
             curvature = -0.2, size =1,
             arrow = arrow(length = unit(0.03, "npc"))) +
  scale_colour_viridis_c() +
  geom_hline(yintercept = mean(chopped$episode_rating, na.rm = TRUE), colour = "deepskyblue4", alpha = 0.2, size = 0.8) +
  annotate("text", x = 35, y = 6.7, label = wrapper(season33, width = 35),family = "Gadugi", size = 2) +
  annotate("text", x = 22.5, y = 5.8, label = wrapper(season29, width = 35),family = "Gadugi", size = 2) +
  annotate("text", x = 36.5, y = 9.6, label = wrapper(season40, width = 35),family = "Gadugi", size = 2) +
  annotate("text", x = 26.5, y = 9.3, label = wrapper(season25, width = 35),family = "Gadugi", size = 2) +
  annotate("text", x = 4, y = 9.5, label = wrapper(season1, width = 35),family = "Gadugi", size = 2)

ggsave("chopped_ratings.png", plot = plot, type = "cairo-png", width = 8, height = 4)
