#load data and packages
library(tidyverse)
theme_set(theme_minimal())

coffee <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

#looking at the data and some summaries
head(coffee)
colnames(coffee)

#top countries by total points
coffee %>%
  group_by(country_of_origin) %>%
  summarise(avgrating = mean(total_cup_points)) %>%
  top_n(10, avgrating) %>%
  ggplot(aes(x = reorder(country_of_origin, avgrating), y = avgrating)) +
  geom_col(fill = "#6f4e37", colour = "white") +
  coord_flip() +
  labs(x = "Country of Origin", y = "Average Rating (out of 100)", title = "Top 10 Countries by Coffee Rating") +
  ggsave(filename = "Top 10 Countries.png", type = "cairo-png", dpi = 600, height = 5, width = 5)

#Altitude effect on quality of coffee
coffee %>%
  filter(altitude_mean_meters < 3000, total_cup_points > 0) %>%
  ggplot(aes(x = altitude_mean_meters, y = total_cup_points)) +
  geom_point(colour = "#6f4e37", alpha = 0.5, size = 2) +
  geom_smooth(method = "lm") +
  labs(x = "Mean Altitude (Metres)", y = "Total Cup Points", title = "Is there a correlation between altitude grown and quality of coffee?") +
  ggsave(filename = "Altitude vs Quality.png", type = "cairo-png", dpi = 600, height = 7, width = 7)

#How about the colour of the bean?

coffee %>%
  group_by(color) %>%
  summarise(points = mean(total_cup_points))

#not really...
#processing method?

coffee %>%
  group_by(processing_method) %>%
  summarise(points = mean(total_cup_points))

#any particular years?
coffee %>%
  group_by(harvest_year) %>%
  tally() %>%
  filter(harvest_year %in% c(2000:2020))

coffee %>%
  group_by(harvest_year) %>%
  filter(harvest_year %in% c(2010:2018)) %>%
  summarise(points = mean(total_cup_points)) %>%
  ggplot(aes(x = harvest_year, y = points, group = 1)) +
  geom_line(colour= "#6f4e37", size = 1) +
  geom_point() +
  labs(x = "Year", y = "Average Points", title = "Coffee quality over the past decade", subtitle = "Slight decrease - no significant difference") +
  ggsave(filename = "Points by Year.png", type = "cairo-png", dpi = 600, height = 5, width = 7)
#creating coffee stats dataframe
coffee_stats <-
  coffee %>%
  filter(!is.na(country_of_origin), total_cup_points > 0) %>%
  select(country_of_origin, acidity, aftertaste, aroma, balance, body, flavor, cupper_points, sweetness, uniformity)

#distributions of each stat
profile_stats <- coffee_stats %>%
  select(-country_of_origin) %>%
  gather(key = "profile", value = "points", c(1:9)) %>%
  filter(points > -2.5)

profile_stats %>%
  ggplot(aes(x = points)) +
  geom_density(aes(fill = profile), alpha = 0.75, colour = "white") +
  facet_wrap(~profile, ncol = 3) +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        strip.background = element_blank(),
        legend.position = "none") +
  labs(x = "points", y = "Distribution of Points", title = "Distribution of points by profile") +
  ggsave(filename = "Coffee profile distributions.png", type = "cairo-png", dpi = 600, height = 7, width = 7)

#taking a look at countries
country_stats <- coffee_stats %>%
  group_by(country_of_origin) %>%
  gather(key = "profile", value = "points", c(2:10)) %>%
  filter(points > -2.5)

country_mean <- coffee_stats %>%
  group_by(country_of_origin) %>%
  gather(key = "profile", value = "points", c(2:10)) %>%
  group_by(country_of_origin, profile) %>%
  mutate_at(c(3), mean)

country_mean <- unique(country_mean)

international_mean <- coffee_stats %>%
  group_by(country_of_origin) %>%
  gather(key = "profile", value = "points", c(2:10)) %>%
  group_by(profile) %>%
  mutate_at(c(3), mean)

international_mean <- unique(international_mean[2:3])

country_merged <- merge(country_mean, international_mean, by = "profile") %>%
  mutate(diff = points.x - points.y)

country_merged %>%
  ggplot() +
  geom_point(aes(x = reorder(country_of_origin, diff), y = points.x, colour = profile)) +
  geom_point(data = country_stats, aes(x = country_of_origin, y = points), colour = "grey90", alpha = 0.1) +
  geom_hline(aes(yintercept = points.y, colour = profile), alpha = 0.5) +
  coord_flip() +
  facet_wrap(~profile, ncol = 3, scales = "free") +
  theme(panel.border = element_rect(colour = "black", fill = NA),
        strip.background = element_blank(),
        legend.position = "none") +
  labs(x = "Points", y = "Country", title = "Coffee profiles by Country of Origin compared to the international mean") +
  ggsave(filename = "countriescompared.png", type = "cairo-png", dpi = 600, height = 14, width = 15)
