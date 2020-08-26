library(tidyverse)
library(ggdark)
library(RColorBrewer)
theme_set(theme_light())

astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

head(astronauts)
colnames(astronauts)

astronauts <- astronauts %>%
  mutate(age_at_selection = year_of_selection - year_of_birth,
         age_at_mission = year_of_mission - year_of_birth)

astronauts %>%
  group_by(name) %>%
  mutate(first_mission_age = min(age_at_mission), mission_number = row_number()) %>%
  select(name, year_of_mission, first_mission_age, mission_number) %>%
  filter(mission_number == 1) %>%
  ggplot(aes(x = year_of_mission, y = first_mission_age)) +
  geom_point(size = 2, colour = "deepskyblue3", alpha = 0.75)+
  geom_smooth(colour = "firebrick", alpha = 0.5) +
  expand_limits(y = c(20,60)) +
  labs(title = "Astronauts getting progressively older",
       subtitle = "Age of each astronaut at the time of their first mission",
       x = "Year of mission",
       y = "Age of Astronaut during first mission") +
  dark_theme_light() +
  ggsave(filename = "first_mission_age.png", dpi = 600, height = 4, width = 6, type = "cairo-png")

astronauts %>%
  group_by(name) %>%
  mutate(first_mission_age = min(age_at_mission), mission_number = row_number(),
         decade = (year_of_mission - year_of_mission %% 10)) %>%
  select(name, year_of_mission, first_mission_age, mission_number, decade) %>%
  filter(mission_number == 1) %>%
  ggplot(aes(x = first_mission_age)) +
  geom_density(aes(fill = as.factor(decade), colour = as.factor(decade)), alpha = 0.5) +
  facet_wrap(~decade) +
  dark_theme_light() +
  theme(legend.position = "none") +
  labs(x = "Age at first mission", y = "Percentage of Astronauts", title = "Distribution of ages at first mission by decade") +
  ggsave(filename = "age_distribution_decade.png", height = 5, width = 7, dpi = 600, type = "cairo-png")

astronauts %>%
  group_by(year_of_mission) %>%
  summarise(average_time = mean(total_hrs_sum), missions = n()) %>%
  gather(key = "key", value = "value", c("average_time", "missions")) %>%
  ggplot(aes(x = year_of_mission, y = value)) +
  geom_line(aes(colour = key), size = 1) +
  facet_wrap(~key, scales = "free") +
  theme(legend.position = "none") +
  dark_theme_light() +
  scale_colour_manual(values = c("firebrick", "deepskyblue3"))+
  labs(x = "Year of Mission", y = "Average Mission Hours / Total Missions",
       title = "Number of missions has been going down each year, but time spent on each mission increasing") +
  ggsave(filename = "time_spent_vs_missions.png", dpi = 600, height = 6, width = 10, type = "cairo-png")

astronauts %>%
  group_by(mission_title, year_of_mission) %>%
  summarise(count = n()) %>%
  select(year_of_mission) %>%
  group_by(year_of_mission) %>%
  tally() %>%
  ggplot(aes(x = year_of_mission, y = n)) +
  geom_line(size = 1.5, colour = "deepskyblue3") +
  theme(legend.position = "none") +
  dark_theme_light() +
  labs(x = "Year of Mission", y = "Total Missions",
       title = "Number of missions has been going down each year") +
  ggsave(filename = "missions_over_time.png", dpi = 600, height = 4, width = 6, type = "cairo-png")

astronauts %>%
  group_by(sex) %>%
  tally()

astronauts %>%
  mutate(decade = (year_of_selection - year_of_selection %% 10)) %>%
  group_by(sex, decade) %>%
  tally() %>%
  group_by(decade) %>%
  mutate(total = sum(n), percentage = n/total, ) %>%
  ggplot(aes(x = decade, y = percentage)) +
  geom_col(aes(fill = sex), position = "stack") +
  labs(title = "More female astronauts now than ever before",
       subtitle = "However men still make up nearly 75%",
       x = "Decade", y = "Percentage of each sex") +
  dark_theme_light() +
  scale_fill_manual(values = c("pink2", "deepskyblue3")) +
  ggsave(filename = "gender_by_decade.png", dpi = 600, height = 5, width = 5)

astronauts %>%
  select(nationality, year_of_selection)  %>%
  group_by(nationality, year_of_selection) %>%
  unique() %>%
  group_by(year_of_selection) %>%
  tally() %>%
  ggplot(aes(x = year_of_selection, y = round(n, 0))) +
  geom_col(fill = "deepskyblue3", colour = "black", size = 0.2) +
  geom_smooth(colour = "firebrick", se = FALSE) +
  expand_limits(y = c(0,10)) +
  dark_theme_light() +
  labs(x = "Year of Selection", y = "Distinct nationalities selected",
       title = "Nationality variety peaked in the 80s, but has since returned to lower levels",
       subtitle = "Distinct nationalities selected by year") +
  ggsave(filename = "nationalities_by_year.png", type = "cairo-png", dpi = 600, height = 5, width = 8)

astronauts %>%
  select(nationality, name) %>%
  unique() %>%
  group_by(nationality) %>%
  tally(sort = TRUE) %>%
  mutate(aspercent = n/sum(n))

astronauts %>%
  group_by(name) %>%
  mutate(mission_number = row_number()) %>%
  select(name, year_of_mission, age_at_selection, age_at_mission) %>%
  ggplot(aes(x = year_of_mission, y = age_at_mission)) +
  geom_jitter(aes(colour = age_at_selection), alpha = 0.8) +
  dark_theme_minimal() +
  scale_colour_viridis_c(option = "plasma") +
  labs(colour = "Age at selection") +
  geom_smooth(se = FALSE, method = "lm", colour = "firebrick") +
  expand_limits(y = c(20,70)) +
  labs(title = "Reaching Infinity - Age of astronauts continues to rise",
       x = "Year of Mission",
       y = "Age at Mission") +
  ggsave(filename = "ages_all_missions.png", type = "cairo-png", dpi = 800, height = 5, width = 8)
