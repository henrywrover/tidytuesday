#Loading data and libraries needed
individuals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/individuals.csv')
locations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/locations.csv')

library(tidyverse)
library(lubridate)
library(gganimate)
theme_set(theme_minimal())

#creating blank theme for ggplot
blank_theme<- theme(panel.grid.major=element_blank(),
                    panel.grid.minor=element_blank())

season_colours = c("#ffb4c3", "#dcdda3", "#e7bc9f", "#ade6e7")

#quick check of the data
head(individuals)
head(locations)

#some initial formatting/adding new columns
locations$date <- as.Date(locations$timestamp)
locations$month <- months(locations$date)
locations$month <- factor(locations$month, levels = month.name)
locations$year <- year(locations$timestamp)

locations <- locations %>%
  mutate(season = ifelse(month %in% c("March", "April", "May"), "Spring", 
                         ifelse(month %in% c("June", "July", "August"), "Summer",
                                ifelse(month %in% c("September", "October", "November"), "Autumn", "Winter"))))

#looking at different death causes
individuals %>%
  filter(!is.na(death_cause)) %>%
  group_by(death_cause) %>%
  tally(sort = TRUE) %>%
  top_n(5,n)

#adding predator column if there is one
individuals <- individuals %>%
  mutate(predator = ifelse(is.na(death_cause), "No Death",
                        ifelse(str_detect(tolower(death_cause),'wolf'),'wolf',
                            ifelse(str_detect(tolower(death_cause),'bear'),'bear',
                                  "Unknown"))))

#grouping the locations by animal and adding id
locations <- locations %>%
  group_by(animal_id) %>%
  mutate(id = row_number())

#looking at number of caribou killed by predators
individuals %>%
  group_by(predator) %>%
  tally(sort = TRUE)

#creating variable to show cairbou that have been killed by wolves
dead <- individuals %>%
  filter(deploy_off_type == "dead" & !is.na(deploy_off_latitude) & !is.na(deploy_on_latitude) & predator == "wolf")

#selecting the caribou killed by a wolf with the largest amount of location data
top1 <- locations %>%
  group_by(animal_id) %>%
  filter(animal_id %in% dead$animal_id) %>%
  tally() %>%
  top_n(1)

dead <- dead %>%
  filter(animal_id %in% top1$animal_id)

mycaribou <- locations %>%
  filter(animal_id %in% top1$animal_id)

#plotting lifetime of this caribou
mycaribou %>%
  arrange(timestamp) %>%
  ggplot(aes(x = longitude, y = latitude)) +
  geom_point(alpha = 0.3, size = 2, colour = "deepskyblue3") +
  blank_theme +
  labs(title = "Lifetime sightings of Caribou 'QU_car173' - 2013-03-25 to 2015-04-08", x = "Longitude", y = "Latitude", colour = "") +
  theme(legend.position = "bottom", legend.key.width = unit(1.5, "cm"), legend.key.height = unit(0.5, "cm")) +
  ggsave("caribou_location_scatter.png", type = "cairo-png", dpi = 600, height = 8, width = 10)


#plotting locations by season
locations %>%
  arrange(timestamp) %>%
  filter(animal_id %in% dead$animal_id) %>%
  ggplot(aes(x = longitude, y = latitude, colour = season)) +
  geom_point(alpha = 0.3, size = 2.5) +
  scale_colour_manual(values = c("#ffb4c3", "#dcdda3", "#e7bc9f", "#ade6e7")) +
  labs(title = "Lifetime sightings of Caribou 'QU_car173' with Seasonality", x = "Longitude", y = "Latitude") +
  blank_theme +
  ggsave(filename = "caribou_location_seasons.png", dpi = 600, height = 8, width = 10)

#plotting static plot of caribou
mycaribou %>%
  arrange(timestamp) %>%
  ggplot(aes(x = longitude, y = latitude, colour = date)) +
  geom_point(alpha = 0.3, size = 0.5) +
  geom_path(alpha = 0.7, size = 1.5) +
  blank_theme +
  ggsave(filename = "pathplot_caribou.png", dpi = 600, height = 8, width = 10, type = "cairo-png")


#plotting path of caribou
a <- mycaribou %>%
  arrange(timestamp) %>%
  ggplot(aes(x = longitude, y = latitude, colour = timestamp)) +
  geom_path(alpha = 0.5, size = 1.5) +
  geom_point(alpha = 0.3)
  blank_theme +
  transition_states(id, transition_length = 1, state_length = 1) +
  transition_reveal(id)

animate(a, nframes = 200) +
  anim_save("caribou_timeline_animation.gif")