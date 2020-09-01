library(tidyverse)
library(RColorBrewer)
library(extrafont)
theme_set(theme_minimal())
options(scipen = 999)

key_crop_yields <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv')
tractors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/cereal_yields_vs_tractor_inputs_in_agriculture.csv')

long_crops <- key_crop_yields %>% 
  pivot_longer(cols = 4:last_col(),
               names_to = "crop", 
               values_to = "crop_production") %>% 
  mutate(crop = str_remove_all(crop, " \\(tonnes per hectare\\)")) %>% 
  set_names(nm = names(.) %>% tolower())

yield <- long_crops %>%
  group_by(code, crop) %>%
  summarise(yield = sum(crop_production, na.rm = TRUE))

pop <- tractors %>%
  filter(!is.na(Code), Entity != "World") %>%
  group_by(Code) %>%
  filter(Year == max(Year)) %>%
  janitor::clean_names()

pop_crop <- merge (yield, pop, by = "code", all.x = TRUE) %>%
  filter(!is.na(entity)) %>%
  select(entity, yield, total_population_gapminder, crop) %>%
  rename(population = total_population_gapminder) %>%
  group_by(entity) %>%
  mutate(percentage = yield/sum(yield)*100)

top_crops <- long_crops %>%
  group_by(crop) %>%
  summarise(sum = sum(crop_production, na.rm = TRUE)) %>%
  top_n(6, sum)

pop_crop %>%
  filter(population > 1000000, crop %in% top_crops$crop) %>%
  ggplot(aes(x = population, y = percentage)) +
  geom_point(alpha = 0.5, colour = "grey75") +
  scale_x_log10() +
  geom_smooth(aes(colour = crop), se = FALSE) +
  facet_wrap(~crop, scales = "free_y") +
  theme(legend.position = "none",
        text = element_text(family = "Gadugi"),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  expand_limits(y = 0) +
  labs(title = "Do countries with differing populations rely on different crops?",
       subtitle = "Countries with over one million population, top 6 crops grown worldwide. Below we can see Potatoes, \n Maize and to a lesser extent Wheat are grown more frequently in countries with a lower population. \n Meanwhile Bananas and Cassava are grown in countries with a higher population.",
       x = "Population of Country (2019)",
       y = "Percentage of Total Crops Grown (2019)",
       caption = "@henrywrover2 - Data from Our World in Data via the TidyTuesday Project") +
  scale_colour_brewer(palette = "Set1") +
  ggsave(filename = "crop_final.png", height = 6, width = 8, type = "cairo-png")