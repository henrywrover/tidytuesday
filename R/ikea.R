library(tidyverse)
library(extrafont)
theme_set(theme_minimal())

### loading data and converting to GBP (roughly)
ikea <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-03/ikea.csv') %>%
  select(-X1) %>%
  mutate(price = round(price * 0.21, 2),
         old_price = as.numeric(gsub("SR |,", "", old_price)),
         old_price = round(old_price * 0.21, 2),
         discounted = ifelse(old_price == "NA", "N", "Y"),
         discount_pct = (1 * (old_price - price) / old_price),
         category = ifelse(category == "Sideboards, buffets & console tables", "Sideboards", 
                           ifelse(category == "Chests of drawers & drawer units", "Drawer Units", category)))

### taking a look at the categories
ikea %>%
  group_by(category) %>%
  tally(sort = TRUE)

### removing one category to make the facets fit
ikea %>%
  filter(category != "Café furniture") %>%
  ggplot(aes(x = width, y = height)) +
  facet_wrap(~category, ncol = 4) +
  scale_x_continuous(limits = c(0,200)) +
  scale_y_continuous(limits = c(0,200)) +
  geom_rect(aes(xmin = 0, ymin = 0, xmax = width, ymax = height), alpha = 0.1, fill = "#FFDA1A", colour = "#0051ba", size = 1.2) +
  theme(legend.position = "none",
        panel.grid = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10, face = "bold"),
        axis.title = element_text(size = 12, face = "bold"),
        plot.title = element_text(hjust = 0.5,
                                  size = 36,
                                  face = "bold",
                                  family = "Gadugi"),
        text = element_text(family = "Gadugi"),
        plot.margin = unit(c(1,1,1,1), "cm"),
        plot.caption = element_text(size = 6)) +
  labs(title = "The many shapes of IKEA furniture",
       x = "Width (cm)",
       y = "Height (cm)",
       caption = "Source | https://www.kaggle.com/ahmedkallam/ikea-sa-furniture-web-scraping\nTwitter | @henrywrover2\nGithub | henrywrover\n 3rd November 2020") +
  ggsave(filename = "ikea.png", type = "cairo-png", height = 10, width = 12, dpi = 180)
