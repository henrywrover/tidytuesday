library(extrafont)
library(tidyverse)
library(wesanderson)
theme_set(theme_minimal())

key_crops <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-09-01/key_crop_yields.csv')

long_crops <- key_crops %>% 
  pivot_longer(cols = 4:last_col(),
               names_to = "crop", 
               values_to = "crop_production") %>% 
  mutate(crop = str_remove_all(crop, " \\(tonnes per hectare\\)")) %>% 
  set_names(nm = names(.) %>% tolower()) %>%
  rename("country" = "entity")

long_crops %>%
  filter(country == "United Kingdom") %>%
  filter(crop %in% c("Maize", "Barley", "Wheat", "Potatoes", "Peas")) %>%
  group_by(year) %>%
  mutate(crop = fct_reorder(crop, crop_production),
         crop_production = ifelse(is.na(crop_production), 0, crop_production),
         percentage = crop_production/sum(crop_production)) %>%
  ggplot(aes(x = year, y = percentage)) +
  geom_area(aes(fill = crop), alpha = 0.6) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(x = "Year",
       y = "Percentage of Crops Grown",
       title = "Crops grown in the United Kingdom 1961 to 2019",
       caption = "@henrywrover2 - Data comes from Our World in Data via TidyTuesday - 02.09.2020") +
  scale_fill_manual(values = wes_palette("Darjeeling1")) +
  theme(panel.grid = element_blank(),
        plot.background = element_rect(fill = "#000220"),
        text = element_text(family = "Gadugi", colour = "White"),
        plot.title = element_text(size = 8, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 6),
        axis.text = element_text(colour = "White", size = 5, face = "bold"),
        legend.text = element_text(size = 6),
        legend.title = element_blank(),
        plot.caption = element_text(size = 3)) +
  ggsave(filename = "UK Crops.png", height = 2, width = 4, type = "cairo-png")
