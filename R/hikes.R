library(tidyverse)
library(ggdark)

hikes <- readr::read_rds(url('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-11-24/hike_data.rds')) %>%
  mutate(highpoint = as.numeric(highpoint),
         rating = as.numeric(rating),
         round_rating = round(rating, 0))

hikes %>%
  ggplot(aes(x = highpoint)) +
  geom_histogram(aes(fill = factor(round_rating, levels = c(0,1,2,3,4,5)))) +
  scale_fill_manual(values = c("grey90",  "#c6ccd8", "#85a1c1", "#274b69", "grey20","#202022")) +
  labs(title = "Hiking in Washington",
       subtitle = "Trail highpoints and user ratings on Washington Trail Association",
       fill = "User Rating",
       y = "Number of trails",
       x = "Highpoint (feet above sea level)",
       caption = "Source | wta.org via @tidy_explained\nTwitter | @henrywrover2\n Github | henrywrover") +
  dark_theme_minimal() +
  guides(fill = guide_legend(nrow = 1)) +
  theme(panel.grid = element_blank(),
        plot.title = element_text(hjust = 0.5,
                                  size = 8,
                                  face = "bold"),
        plot.subtitle = element_text(hjust = 0.5,
                                     size = 6),
        axis.text = element_text(face = "bold",
                                 size = 6),
        axis.title = element_text(face = "bold",
                                  size = 6),
        legend.position = "bottom",
        legend.title = element_text(size = 6),
        legend.text = element_text(size = 6),
        legend.key.size = unit(0.25, "cm"),
        plot.caption = element_text(size = 4))
