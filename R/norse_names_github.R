library(extrafont)
library(tidyverse)
library(fuzzyjoin)
library(maps)
library(mapdata)
library(maptools)
library(rgdal)
library(ggmap)
library(ggplot2)
library(rgeos)
library(broom)
library(plyr)
library(showtext)

### load data

places <- read_csv("places_detailed.csv") %>%
  janitor::clean_names() %>%
  dplyr::rename("town" = place) %>%
  mutate(id = ifelse(id %in% c("West Northamptonshire",
                               "North Northamptonshire",
                               "South Northamptonshire",
                               "East Northamptonshire"), "Northamptonshire", id))

suffix <- read_csv("suffixes.csv") %>%
  janitor::clean_names() %>%
  dplyr::rename("town" = term)

mapdata <- tidy(shapefile, region = "LAD21NM") %>%
  mutate(id = ifelse(id %in% c("West Northamptonshire",
                               "North Northamptonshire",
                               "South Northamptonshire",
                               "East Northamptonshire"), "Northamptonshire", id),
         group = ifelse(id %in% c("West Northamptonshire.1",
                                  "North Northamptonshire.1",
                                  "South Northamptonshire.1",
                                  "East Northamptonshire.1"), "Northamptonshire.1", group))

### match data

df <- fuzzy_join(places, suffix, match_fun = str_detect, by = "town", mode = "full") %>%
  dplyr::rename("town" = town.x,
                "suffix" = town.y)

### analysis

# map of counties and % ON

df2 <- df %>%
  group_by(id, origin) %>%
  tally() %>%
  pivot_wider(names_from = origin,
              values_from = n) %>%
  mutate(ON = ifelse(is.na(ON), 0, ON),
         OE = as.integer(ifelse(is.na(OE), 0, OE)),
         pct_ON = ON / (ON + OE + `NA`),
         pct_ON = as.integer(ifelse(is.na(pct_ON), 0, pct_ON)),
         pct_OE = OE / (ON + OE + `NA`),
         pct_OE = as.integer(ifelse(is.na(pct_OE), 0, pct_ON))) %>%
  right_join(mapdata, on = "id") %>%
  filter(id %in% places$id)

p <- ggplot() +
  geom_polygon(data = df2, aes(x = long, y = lat, group = group, fill = pct_ON), alpha = 0.8, color = "grey25", size = 0.6) +
  coord_fixed(1) +
  scale_fill_gradient2(labels = scales::percent_format(accuracy = 1L), low = "grey95", high = "deepskyblue4") +
  theme_void() +
  theme(plot.title = element_text(size = 32, hjust = 0.5, family = "Century Gothic"),
        plot.background = element_rect(fill = "#F8F5E6", colour = "#F8F5E6"),
        plot.subtitle = element_text(size = 16, hjust = 0.5, family = "Century Gothic"),
        legend.title = element_text(size = 16, hjust = 0.5, vjust = 0.7, family = "Century Gothic"),
        legend.text = element_text(size = 12, hjust = -0.1, family = "Century Gothic"),
        legend.position = "bottom",
        legend.key.size = unit(1.5, 'cm'),
        plot.margin=unit(c(1,1,1,1),"cm"),
        plot.caption = element_text(size = 12)) +
  labs(fill = "% of ON Town Names within area",
       title = "Town names in England with Old Norse Origins\n",
       subtitle = "Beginning in the 8th Century, England was frequently raided by Vikings, and by the late 9th Century
       they had begun to settle in the North and North-East regions. These settlements are marked today by the presence
       of suffixes with Old Norse origins in the towns and villages in the area. Suffixes such as '-by' (Grimsby, Derby)
       and '-thorpe' (Scunthorpe, Mablethorpe) can be seen frequently throughout these areas. Meanwhile in the South and
       West of England suffixes with Old English origins, such as '-ton' (Brighton, Southampton) or '-ham' (Lewisham, Horsham),
       are much more prominent",
       caption = "Visualisation | Henry Wakefield\n Created | 31st December 2021\nSource | Office for National Statistics licensed under the Open Government Licence v.3.0")

ggsave(p, filename = "norse_plot_detailed.tiff", width = 15, height = 20)