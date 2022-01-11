library(extrafontdb)
library(tidyverse)
library(geojsonio)
library(RColorBrewer)
library(rgdal)
library(broom)
library(rgeos)

###

colony <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')

spdf <- geojson_read("us_states_hexgrid.geojson",  what = "sp")

spdf@data <- spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2)) %>%
  filter(!id %in% c("HI", "AK"))

spdf_fortified <- tidy(spdf, region = "google_name")

merge <- spdf@data %>%
  select(google_name, iso3166_2)

###

plot <- colony %>%
  filter(months %in% c("January-March", "October-December")) %>%
  group_by(state) %>%
  summarise(mean_loss = mean(colony_lost_pct, na.rm = TRUE)) %>%
  mutate(mean_loss = mean_loss / 100) %>%
  right_join(spdf_fortified, by = c("state" = "id")) %>%
  filter(!state %in% c("United States", "Other States", "Alaska", "Hawaii")) %>%
  left_join(merge, by = c("state" = "google_name")) %>%
  left_join(centers, by = c("iso3166_2" = "id")) %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = mean_loss), colour = "#312f17", size = 2) +
  theme_void() +
  coord_map() +
  geom_text(aes(x = x,
                y = y,
                label = ifelse(
                  is.na(mean_loss),
                  paste0(iso3166_2, "\n", NA),
                  paste0(iso3166_2, "\n", scales::percent(mean_loss, accuracy = 1L)))),
            color="black",
            size = 6,
            family = "Century Gothic") +
  scale_fill_stepsn(
  colours = c("#ffdf77", "#fccf3e","#e5ac3f", "#f55f20"),
    breaks = c(0.05, 0.1, 0.15, 0.2),
    na.value = "white",
    labels = scales::percent_format(accuracy = 5L)
  ) +
  theme(plot.title = element_text(size = 32, family = "Century Gothic", face = "bold"),
        plot.background = element_rect(fill = "#F8F5E6", colour = "#F8F5E6"),
        plot.subtitle = element_text(size = 16, family = "Century Gothic"),
        legend.title = element_text(size = 16, hjust = 0.5, vjust = 0.7, family = "Century Gothic"),
        legend.text = element_text(size = 12, hjust = -0.1, family = "Century Gothic"),
        legend.position = "bottom",
        legend.key.size = unit(1, 'cm'),
        plot.margin=unit(c(1,1,1,1),"cm"),
        plot.caption = element_text(size = 10, family= "Century Gothic")) +
  labs(title = "\n\nBeekeepers facing harsh colony losses over Winter Months",
       subtitle = "\nMean % of Bee Colonies lost over Winter Months (1st October to 31st March) from 2015 to 2021",
       fill = "Mean Loss %",
       caption = "Visualisation | Henry Wakefield\nTwitter | @henrywrover2\n Data | USDA\nSource | https://usda.library.cornell.edu/concern/publications/rn301137d\n\n")

ggsave(plot, filename = "bee_plot.png", width = 20, height = 15)
