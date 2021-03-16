library(tidyverse)
library(lubridate)
theme_set(theme_bw())

wrapper <- function(x, ...) paste(strwrap(x, ...), collapse = "\n")

a1 <- "Civ VI was released on Steam in October 2016"
a2 <- "In February 2018 the first major expansion, Rise and Fall, was released"
a3 <- "One year later Gathering Storm, the second major expansion, was released"
a4 <- "May 2020 saw the release of the New Frontier Pass, a bi-monthly DLC pass"

games <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-03-16/games.csv") %>%
  mutate(avg_peak_perc = as.numeric(str_remove(avg_peak_perc, "%")),
         date = dmy(paste("01", month, year, sep = "-")))

games %>%
  filter(gamename %in% c("Sid Meier's Civilization V", "Sid Meier's Civilization VI"),
         date >= "2016-01-01") %>%
  ggplot(aes(x = date, y = avg)) +
  geom_line(aes(colour = gamename), size = 1) +
  theme(panel.grid = element_blank()) +
  scale_colour_brewer(palette = "Paired") +
  labs(title = "Civilization V vs Civlization VI",
       subtitle = "In October 2016 Sid Meier's Civilization VI was released and as of February 2021 has 40,000 players at any one time. Despite the recent successes\nthe strategy game got off to a rocky start, facing fierce competition from its highly successful predecessor, Civilization V.It took multiple patches, updates and\ntwo major DLC releases before Civilization VI finally took the throne as the most popular Civilization game on Steam", 
       colour = "",
       x = "Date",
       y = "Average Concurrent Players") +
  annotate("text", x = as.Date("2016-10-01", "%Y-%m-%d"), y = 88000, label = wrapper(a1, width = 40), size = 2) +
  annotate("text", x = as.Date("2018-02-01", "%Y-%m-%d"), y = 36000, label = wrapper(a2, width = 40), size = 2) +
  annotate("text", x = as.Date("2019-02-01", "%Y-%m-%d"), y = 45000, label = wrapper(a3, width = 40), size = 2) +
  annotate("text", x = as.Date("2020-05-01", "%Y-%m-%d"), y = 48000, label = wrapper(a4, width = 40), size = 2) +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 12,
                                  face = "bold",
                                  hjust = 0.5),
        plot.subtitle = element_text(size = 8,
                                  hjust = 0.5)) +
  ggsave(filename = "civ.png", type = "cairo-png", height = 7, width = 10, dpi = 150)
