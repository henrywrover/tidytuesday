library(extrafont)
library(RColorBrewer)
library(patchwork)
library(tidyverse)
theme_set(theme_minimal())

kids <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-15/kids.csv')

libraries <- kids %>%
  filter(variable == "lib") %>%
  mutate(inf_adj_perchild = inf_adj_perchild * 1000)

positive <- libraries %>%
  filter(year <= 2016, year >= 2006) %>%
  select(-c("raw", "inf_adj")) %>%
  pivot_wider(names_from = "year", values_from = "inf_adj_perchild") %>%
  mutate(pct_change = as.numeric((`2016`-`2006`)/`2006`)) %>%
  top_n(15, pct_change) %>%
  arrange(-pct_change)%>%
  pivot_longer(cols = `2006`:`2016`, names_to = "year", values_to = "inf_adj_perchild") %>%
  filter(year %in% c("2006", "2016")) %>%
  ggplot(aes(x = inf_adj_perchild, y = reorder(state, pct_change))) +
  geom_col(aes(fill = year), position = position_dodge2(reverse = TRUE)) +
  labs(x = "Inflation adjusted spend per child (USD)",
       y = "State",
       title = "States with the largest increase of \nlibrary funding between 2006 and 2016") +
  scale_fill_brewer(palette = "Greens") +
  theme(plot.title = element_text(hjust = 0.5,
                                  size = 6),
        text = element_text(family = "Gadugi"),
        axis.title = element_text(size = 6),
        axis.text = element_text(size = 6),
        legend.position = "bottom",
        legend.title = element_text(size= 6),
        legend.text = element_text(size = 5))

negative <- libraries %>%
  filter(year <= 2016, year >= 2006) %>%
  select(-c("raw", "inf_adj")) %>%
  pivot_wider(names_from = "year", values_from = "inf_adj_perchild") %>%
  mutate(pct_change = as.numeric((`2016`-`2006`)/`2006`)) %>%
  top_n(-15, pct_change) %>%
  arrange(-pct_change)%>%
  pivot_longer(cols = `2006`:`2016`, names_to = "year", values_to = "inf_adj_perchild") %>%
  filter(year %in% c("2006", "2016")) %>%
  ggplot(aes(x = inf_adj_perchild, y = reorder(state, -pct_change))) +
  geom_col(aes(fill = year), position = position_dodge2(reverse = TRUE)) +
  labs(x = "Inflation adjusted spend per child (USD)",
       y = "State",
       title = "States with the largest decrease of \nlibrary funding between 2006 and 2016") +
  scale_fill_brewer(palette = "Reds") +
  theme(plot.title = element_text(hjust = 0.5,
                                  size = 6),
        text = element_text(family = "Gadugi"),
        axis.title = element_text(size = 6),
        axis.text = element_text(size = 6),
        legend.position = "bottom",
        legend.title = element_text(size= 6),
        legend.text = element_text(size = 5))

kidsplot <- (positive|negative) +
  plot_annotation(title = "The biggest improvers and worst offenders for library funding between 2006 and 2016",
                  theme = theme(text = element_text(family = "Gadugi"),
                        plot.title = element_text(hjust = 0.5, size = 10)))

ggsave(kidsplot, filename = "kidsplot.png", type = "cairo-png", height = 4, width = 7)
