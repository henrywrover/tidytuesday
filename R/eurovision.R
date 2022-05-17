library(tidyverse)
library(RColorBrewer)
library(extrafontdb)
library(extrafont)

df <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision.csv') %>%
  filter(section %in% c("final", "grand-final"),
         year != 2020,
         year > 1956) %>%
  select(-c("host_city", "host_country", "event", "event_url", "section", "artist_url", "image_url", "country_emoji", "rank_ordinal", "qualified")) %>%
  mutate(total_points = ifelse(is.na(total_points), 0, total_points)) %>%
  group_by(year) %>%
  mutate(participants = n(),
         loser = ifelse(participants == rank, TRUE, FALSE))

votes <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision-votes.csv") %>%
  filter(semi_final == "f")

votes1 <- votes %>%
  filter(jury_or_televoting == "J") %>%
  rowwise() %>%
  mutate(first_country = min(from_country, to_country),
         last_country = max(from_country, to_country),
         countries = paste0(first_country, " & ", last_country)) %>%
  group_by(from_country, to_country, countries) %>%
  summarise(mean = mean(points),
            n = n()) %>% 
  filter(n > 10) %>%
  group_by(countries) %>%
  mutate(sum_votes = sum(mean),
         index = row_number(),
         mean = ifelse(index == 1, mean * -1, mean),
         index = ifelse(index == 1, "Left Country", "Right Country"),
         countries = ) %>%
  ungroup() %>%
  top_n(30, sum_votes)

plot <-
  votes1 %>%
  ggplot(aes(x = mean, y = reorder(countries, sum_votes))) +
  geom_col(aes(fill = as.factor(index)), width = 0.5, colour = "grey25", size = 0.8) +
  geom_text(data = votes1 %>% filter(mean > 0), aes(label = round(mean, 1)), hjust = 2) +
  geom_text(data = votes1 %>% filter(mean < 0), aes(label = round(mean * -1, 1)), hjust = -2) +
  theme_minimal() +
  theme(plot.title = element_text(size = 32, family = "Century Gothic", face = "bold"),
        plot.background = element_rect(fill = "#F8F5E6", colour = "#F8F5E6"),
        plot.subtitle = element_text(size = 16, family = "Century Gothic"),
        legend.title = element_blank(),
        legend.text = element_text(size = 12, hjust = -0.1, family = "Century Gothic"),
        legend.position = "bottom",
        plot.margin=unit(c(1,1,1,1),"cm"),
        plot.caption = element_text(size = 10, family= "Century Gothic"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 10, family = "Century Gothic"),
        axis.title = element_text(size = 12, family = "Century Gothic")) +
  labs(x = "Number of points",
       y = "Country Partnership",
       title = "Ooh, friend!",
       subtitle = "\nMean votes given by countries to other countries at Eurovision\n",
       caption = "Min. 10 appearances, top 15 partnerships\nVisualisation | Henry Wakefield\nTwitter | @henrywrover2\n Data | eurovision.tv") +
  scale_fill_brewer(palette = "Paired")

ggsave(plot, filename = "euro_plot.png", width = 10, height = 10, dpi = 300)
