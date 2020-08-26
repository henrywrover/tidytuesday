library(tidyverse)
library(RColorBrewer)
library(extrafont)
theme_set(theme_light())

plants <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/plants.csv')
actions <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/actions.csv')
threats <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-08-18/threats.csv')

continents <- threats %>%
  mutate(year_last_seen = ifelse(
    year_last_seen == "Before 1900", "< 1900", year_last_seen
  )) %>%
  filter(threatened == 1) %>%
  group_by(continent, year_last_seen) %>%
  tally() %>%
  arrange(continent, year_last_seen)

continents %>%
  filter(!is.na(year_last_seen)) %>%
  ggplot(aes(x = reorder(continent, desc(continent)), y = year_last_seen, group = 1)) +
  geom_point(aes(colour = continent, size = n)) +
  scale_size_continuous(range = c(2,20)) +
  coord_flip() +
  labs(title = "Plants in Danger",
       y = "Period plant was last seen in the wild",
       x = "Continent",
       subtitle = "Plants are just as susceptible to extinction as animals, and have been facing their own crisis over the last \n century, so much so that over 500 species are considered extinct as of 2020. Here we can explore the \n loss in diversity by each continent over time. The larger the bubble, the more plants went extinct in this period.") +
  theme(text = element_text(family = "Times New Roman", colour = "grey25"),
        legend.position = "none",
        plot.title = element_text(size = 14,
                                  hjust = 0.5,
                                  face = "bold"),
        plot.subtitle = element_text(size = 10,
                                     hjust = 0.5),
        axis.title = element_text(size = 8)) +
  scale_colour_brewer(palette = "Pastel1") +
  ggsave(filename = "myplot.png", height = 5, width = 8, type = "cairo-png")

plot2 <- threats %>%
  filter(threatened == 1, threat_type != "Unknown") %>%
  group_by(continent, threat_type) %>%
  tally() %>%
  top_n(5, n) %>%
  mutate(perc = (n/sum(n)*100)) %>%
  ggplot(aes(x = continent, y = perc)) +
  geom_col(aes(fill = threat_type), position = position_stack(reverse = TRUE), colour = "grey80") +
  coord_flip() +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Which threats are affecting each continent the most?",
       x = "Continent",
       y = "Percentage of Threats") +
  theme(text = element_text(family = "Times New Roman", colour = "grey25"),
          legend.position = "right",
          plot.title = element_text(size = 14,
                                    hjust = 0.5,
                                    face = "bold"),
          plot.subtitle = element_text(size = 10,
                                       hjust = 0.5),
          axis.title = element_text(size = 8))

plot2 <- threats %>%
  filter(threatened == 1, !is.na(year_last_seen)) %>%
  mutate(year_last_seen = ifelse(
    year_last_seen == "Before 1900", "< 1900", year_last_seen
  )) %>%
  group_by(continent, year_last_seen) %>%
  tally() %>%
  mutate(losses = cumsum(n)) %>%
  ggplot(aes(x = year_last_seen, y = losses, group = 1)) +
  geom_line(aes(colour = continent), size = 1.5) +
  facet_wrap(~continent, scales = "free_y") +
  theme_minimal() +
  labs(title = "Cumulative losses in biodiversity during the 20th and early 21st century",
       x = "",
       y = "") +
  scale_colour_brewer(palette = "Pastel1") +
  theme(text = element_text(family = "Times New Roman", colour = "grey25"),
        legend.position = "none",
        plot.title = element_text(size = 14,
                                  hjust = 0.5,
                                  face = "bold"),
        plot.subtitle = element_text(size = 10,
                                     hjust = 0.5),
        axis.title = element_text(size = 8),
        axis.text.x = element_blank())
  ggsave(filename = "cumulative.png", type = "cairo-png", height = 5, width = 8)

threats %>%
  filter(threatened == 1) %>%
  group_by(continent, threat_type) %>%
  tally() %>%
  ggplot(aes(x = threat_type, y = continent)) +
  geom_tile(aes(fill = n)) +
  scale_fill_viridis_c()