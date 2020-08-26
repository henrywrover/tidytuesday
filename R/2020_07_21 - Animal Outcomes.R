#all the usual loading of packages and data
library(tidyverse)
library(RColorBrewer)
theme_set(theme_minimal())
outcomes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_outcomes.csv')

head(outcomes)

gathered <-
  gather(outcomes, key = "state", value = number, ACT:Total)

pos_neg <- gathered %>%
  mutate(pos_neg = ifelse(outcome %in% c("Reclaimed", "Rehomed"), "Positive", "Negative")) %>%
  filter(outcome %in% c("Reclaimed", "Rehomed", "Euthanized")) %>%
  mutate(number = as.numeric(number))

pos_neg$state <- gsub("Total", "Australia - Total", pos_neg$state)

pos_neg$state_order <- factor(pos_neg$state, levels = c("Australia - Total",
                                                        "ACT",
                                                        "NSW",
                                                        "NT",
                                                        "QLD",
                                                        "SA",
                                                        "TAS",
                                                        "VIC",
                                                        "WA"))

pos_neg %>%
  group_by(year, state_order, pos_neg) %>%
  summarise(total = sum(number)) %>%
  ggplot(aes(x = year, y = total, fill = pos_neg)) +
  geom_col(position = "fill", stat = "identity", alpha = 0.85, colour = "grey70") +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_brewer(palette = "YlGn") +
  facet_wrap(~state_order, scales = "free_x") +
  theme(panel.border = element_rect(colour = "grey60", fill = NA),
        strip.background = element_blank(),
        legend.position = "bottom") +
  labs(title = "Australian animals facing more positive outcomes than ",
       subtitle = "Postitive outcomes defined as Rehomed or Reclaimed. Negative outcomes defined as Euthanised",
       fill = "",
       x = "Year",
       y = "Percentage of Positive / Negative Outcomes",
       caption = "H. Wakefield - hwdatacentre.wordpress.com - 26th July 2020") +
  ggsave(filename = "outcomes_positive_negative.png", type = "cairo-png", dpi = 300, height = 6, width = 8)