library(tidyverse)
theme_set(theme_minimal())

transit <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-05/transit_cost.csv') %>%
  filter(!is.na(e)) %>%
  mutate(real_cost = as.numeric(real_cost),
         cost = as.numeric(cost),
         tunnel_per = as.numeric(gsub("%","", tunnel_per))/100,
         start_year = as.numeric(start_year),
         end_year = as.numeric(end_year)) %>%
  rename(id = e)

lines_per_year <- transit %>%
  group_by(start_year) %>%
  tally() %>%
  filter(n > 1,
         start_year >= 2000 & start_year <= 2020)

transit %>%
  filter(!is.na(start_year),
         start_year %in% lines_per_year$start_year) %>%
  select(start_year, stations, id, cost_km_millions) %>%
  group_by(start_year) %>%
  summarise(stations = round(mean(stations, na.rm = TRUE), 0)) %>%
  group_by(start_year) %>%
  complete(stations = 1:max(stations), fill = list(vol = 0)) %>%
  group_by(start_year) %>%
  mutate(id = min(start_year)) %>%
  ggplot(aes(x = stations, y = reorder(id, -start_year))) +
  geom_path(aes(colour = as.factor(id)), size = 1) +
  geom_point(size = 1, colour = "grey25") +
  labs(x = "",
       y = "",
       title = "Mean number of stations per transit line by start year of construction",
       caption = "Twitter @henrywrover2 | Github henrywrover\n 5th January 2021") +
  theme(panel.grid = element_blank(),
        legend.position = "none",
        axis.text = element_text(face = "bold",
                                 size = 6),
        plot.caption = element_text(size = 4, face = "bold"),
        plot.title = element_text(size = 6,
                                  face = "bold",
                                  hjust = 0.5)) +
  ggsave(filename = "transit_stations.png",
         height = 3,
         width = 5,
         type = "cairo-png")