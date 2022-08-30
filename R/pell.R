library(tidyverse)
library(extrafont)
options(scipen = 999)

pell <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-08-30/pell.csv') %>%
  janitor::clean_names()

caption <-
"Hossain A (2022). pell: Data About Historic Pell Grant Distribution in the US.
https://github.com/Curious-Joe/pell
https://curious-joe.github.io/pell/
Visualisation | Henry Wakefield"

# plotting

pell %>%
  group_by(year) %>%
  summarise(recipient = sum(recipient, na.rm = TRUE),
            award = sum(award, na.rm = TRUE)) %>%
  mutate(award_b = award / 1000000000,
         total_award = sum(award)) %>%
  ggplot(aes(x = year, y = award)) +
  geom_col(fill = "grey85",
           colour = "#08172E",
           size = 1.5) +
  geom_text(aes(label = scales::dollar(award_b, accuracy = 2L)),
            colour = "grey85",
            vjust = -1,
            family = "Century Gothic",
            size = 4) +
  labs(title = "Total funding awarded by the Federal Pell Grant Program",
       subtitle = "1999 to 2017 - Values in Billions of Dollars",
       caption = caption) +
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.text.x = element_text(colour = "grey85",
                                   family = "Century Gothic"),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(size = 18, face = "bold", family= "Century Gothic", colour = "#F8F8FF"),
        plot.background = element_rect(colour = "#08172E", fill = "#08172E"),
        plot.subtitle = element_text(size = 10, family= "Century Gothic", colour = "#F8F8FF"),
        plot.margin = unit(c(1.5,1.5,1.5,1.5), "cm"),
        legend.position = "none",
        plot.caption = element_text(size = 8, family= "Century Gothic", colour = "#F8F8FF"))

ggsave(filename = "pell.png",
       height = 8,
       width = 12,
       )
