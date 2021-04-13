library(tidyverse)

post_offices <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-04-13/post_offices.csv') %>%
  filter(established > 200,
         established < 2020)

discontinued <- post_offices %>%
  group_by(discontinued) %>%
  tally()

post_offices %>%
  group_by(established) %>%
  tally() %>%
  merge(discontinued,
        by.x = "established",
        by.y = "discontinued") %>%
  rename("established" = "n.x",
         "discontinued" = "n.y",
         "year" = "established") %>%
  mutate(discontinued = discontinued * -1) %>%
  pivot_longer(cols = established:discontinued) %>%
  ggplot(aes(x = year, y = value)) +
  geom_col(aes(fill = name), colour = "grey95", size = 0.1) +
  theme(axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom")

### animation

library(tidyverse)
library(gganimate)
library(gifski)
library(maps)

MainStates <- map_data("state")

anim <- post_offices %>%
  mutate(take_out = ifelse(discontinued < 1000, "Y", "N")) %>%
  arrange(established) %>%
  filter(longitude < 100,
         longitude > -140,
         latitude < 50,
         latitude > 22,
         take_out == "N" | is.na(take_out)) %>%
  mutate(is_discontinued = ifelse(!is.na(discontinued), "Discontinued", "Still in Operation"),
         year_col = plyr::round_any(established, 50, f = floor)) %>%
  ggplot(aes(x = longitude, y = latitude, group = established)) +
  geom_polygon(data=MainStates, aes(x=long, y=lat, group=group),
               color="grey75", fill = "white", alpha = 0.8, size = 0.5) +
  geom_point(alpha = 0.3, aes(colour = as.factor(year_col))) +
  transition_time(as.integer(established)) +
  labs(title = "Go West, Young Man",
       subtitle = "How Post Offices in the US made their way across the country",
       caption = "Year: {frame_time}",
       colour = "") +
  shadow_mark(alpha = 0.3) +
  theme_void() +
  scale_colour_viridis_d() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 14,
                                  hjust = 0.5,
                                  face = "bold"),
        plot.subtitle = element_text(size = 10,
                                     hjust = 0.5))

animate(anim, duration = 30, fps = 30,  width = 800, height = 600, renderer = gifski_renderer(loop = TRUE), end_pause = 15)
anim_save("post_offices.gif")
