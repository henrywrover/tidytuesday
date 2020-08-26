library(tidyverse)
library(RColorBrewer)
library(ggtext)

energy_types <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/energy_types.csv')
country_totals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-04/country_totals.csv')

energy <- energy_types %>%
  filter(level == "Level 1") %>%
  pivot_longer(cols = c("2016","2017","2018"), names_to = "year") %>%
  mutate(country_name = ifelse(country == "UK", "United Kingdom",
                               ifelse(country == "EL", "Greece", country_name))) %>%
  mutate(energy_type = ifelse(type == "Conventional thermal", "Conventional Thermal",
                       ifelse(type == "Nuclear", "Nuclear", "Renewable")),
         year = as.numeric(year)) %>%
  select(-c(country, level)) %>%
  janitor::clean_names()

top15 <- energy %>%
  filter(energy_type == "Renewable") %>%
  group_by(country_name) %>%
  summarise(value = sum(value)) %>%
  top_n(15, value)

energy %>%
  filter(type %in% c("Hydro", "Wind", "Solar"),
         country_name %in% top15$country_name) %>%
  group_by(country_name, year, type) %>%
  summarise(total = sum(value)) %>%
  ggplot(aes(x = reorder(year, -year), y = total)) +
  geom_col(aes(fill = type), position = "fill", alpha = 0.8) +
  scale_fill_manual(values = c("#98ddfc", "#FFB347", "#77dd77")) +
  facet_wrap(~country_name, nrow = 3) +
  coord_flip() +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_markdown(),
        legend.position = "bottom") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Which sources of renewable energy to Europeans rely on the most, <b style='color:#98ddfc'>**Hydro**</b>, <b style='color:#FFB347'>Solar</b>, or <b style='color:#77dd77'>Wind</b>?",
       subtitle = "Top 15 producers of renewable energy 2016 to 2018",
       x = "Year",
       y = "% of Renewable Energy Produced",
       fill = "Energy Type",
       caption = "@henrywrover2 - 05/08/2020 - Data comes from Eurostat via #tidytuesday") +
  ggsave(filename = "european_renewable_energy.png", dpi = 300, height = 6, width = 9, type = "cairo-png")
