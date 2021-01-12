library(tidyverse)

artwork <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-01-12/artwork.csv') %>%
  mutate(decade = round(year / 10) * 10,
         acquisition_decade = round(acquisitionYear / 10) * 10)
artists <- readr::read_csv("https://github.com/tategallery/collection/raw/master/artist_data.csv") %>%
  rename(artistId = id)

ggplot(artwork, aes(xmin = 0, ymin = 0, xmax = width, ymax = height)) +
  geom_rect(alpha = 0.1) +
  theme_void()

### custom function I stole from somewhere ages ago

wrapper <- function(x, ...) paste(strwrap(x, ...), collapse = "\n")

### setting up annotations

annotations <- data.frame(x1 = c(1838,1862,1889,1977, 2010),
                          x2 = c(1846,1856,1894,1986, 1999),
                          y1 = c(103,127,-15,249, 148),
                          y2 = c(65,92,-2,193, 190))

a1 <- "In 1847 we see the first large acquisition, as Robert Vernon gave his collection to the National Gallery"
a2 <- "9 years later we see an almighty 37,000 pieces donated to the gallery in the will of British painter J.M.W Turner"
a3 <- "In 1894 Henry Tate donated 65 pieces, before providing £80,000 for the new Tate Gallery to be built"
a4 <- "During the 1960s we start to see a higher percentage of female artists represented in the gallery"
a5 <- "J.M.W Turner turned provider again in 1986 and 1988 when Tate purchased another 1,265 of his pieces"
a6 <- "In 1996 Tate acquired over 3,000 pieces as part of the Oppé Collection"

### the plot

artwork %>%
  select(year, acquisitionYear, artistId) %>%
  filter(!is.na(year),
         !is.na(acquisitionYear)) %>%
  arrange(year) %>%
  mutate(id = row_number()) %>%
  mutate(age_at_acquisition = acquisitionYear - year) %>%
  merge(artists, by = "artistId") %>%
  group_by(age_at_acquisition, acquisitionYear, gender) %>%
  tally(sort = TRUE) %>%
  pivot_wider(names_from = gender, values_from = n) %>%
  mutate(Female = ifelse(is.na(Female), 0, Female)) %>%
  mutate(male_female = Female/sum(Male, Female),
         n = sum(Male, Female)) %>%
  ggplot(aes(x = acquisitionYear, y = age_at_acquisition)) +
  geom_point(aes(size = n, colour = male_female), alpha = 0.2) +
  theme_light() +
  scale_size(range = c(3,12)) +
  labs(title = "Trendspotting - Tate Gallery Collection",
       colour = "Percent of Female Artists",
       size = "Pieces Acquired",
       x = "Acquisition Year",
       y = "Age of piece at acquisition") +
  scale_colour_gradient(high = "pink", low =  "deepskyblue4", label = scales::percent) +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  hjust = 0.5)) +
  annotate("text", x = 1838, y = 120, label = wrapper(a1, width = 40),family = "Gadugi", size = 2) +
  annotate("text", x = 1868, y = 141, label = wrapper(a2, width = 40),family = "Gadugi", size = 2) +
  annotate("text", x = 1868, y = -19, label = wrapper(a3, width = 40),family = "Gadugi", size = 2) +
  annotate("text", x = 1960, y = -19, label = wrapper(a4, width = 40),family = "Gadugi", size = 2) +
  annotate("text", x = 1976, y = 265, label = wrapper(a5, width = 40),family = "Gadugi", size = 2) +
  annotate("text", x = 2010, y = 136, label = wrapper(a6, width = 30),family = "Gadugi", size = 2) +
  geom_curve(data = annotations,
             aes(x = x1, y = y1, xend = x2, yend = y2),
             curvature = 0.2, size = 0.6,
             arrow = arrow(length = unit(0.01, "npc"))) +
  ggsave(filename = "tate_gallery.png", type = "cairo-png", height = 7, width = 10, dpi = 150)
