library(tidyverse)
library(ggthemes)
theme_set(theme_minimal())

tournament <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-06/tournament.csv') %>%
  filter(year >= 1994) %>%
  mutate(tourney_percent = tourney_w/(tourney_w+tourney_l),
         tourney_finish = case_when(tourney_finish == "NSF" ~ "Top 4",
                                    tourney_finish == "Champ" ~ "Winner",
                                    tourney_finish == "RSF" ~ "Top 16",
                                    tourney_finish == "RF" ~ "Top 8",
                                    tourney_finish == "N2nd" ~ "Runner-Up",
                                    tourney_finish == "1st" ~ "First Round",
                                    tourney_finish == "2nd" ~ "Second Round"))

expected_wins <- tibble(
  seed = c(1:16),
  exp_wins = c(3.3, 2.4, 1.8, 1.6, 1.1, 1.1, 0.9, 0.7, 0.6, 0.6, 0.6, 0.5, 0.3, 0.2, 0.1, 0)
)

df <- merge(tournament, expected_wins, by = "seed") %>%
  mutate(delta = tourney_w - exp_wins)

df %>%
  group_by(school) %>%
  summarise(mean = mean(delta),
            max = max(delta),
            min = min(delta),
            tourneys = n()) %>%
  filter(tourneys >= 12) %>%
  ggplot() +
  geom_col(aes(x = reorder(school, mean), y = max), fill = "deepskyblue4", colour = "grey20", alpha = 0.8) +
  geom_col(aes(x = reorder(school, mean), y = min), fill = "firebrick", colour = "grey20", alpha = 0.8) +
  geom_point(aes(x = reorder(school, mean), y = mean), size = 2, colour = "white") +
  coord_flip() +
  scale_y_continuous(breaks = -4:4) +
  labs(x = "School",
       y = "Actual Number of Wins vs Predicted",
       title = "NCAA Women's Basketball League: Best and worst performances of each school 1994 to 2018",
       subtitle = "Based on the teams' seeding we can rate each team's performance vs how they were expected to perform. The red bar shows the worst performance by each school, while the\nblue bar shows the best. The white points indicate the mean performance of each team relative to their expected performance. Minimum 12 appearances.",
       caption = paste("Twitter | @henrywrover2\n GitHub | henrywrover\n", Sys.Date())) +
  theme_few() +
  theme(plot.subtitle = element_text(size = 8)) +
  ggsave(filename = "basketball.png", height = 8, width = 12, type = "cairo-png")
