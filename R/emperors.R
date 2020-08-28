library(tidyverse)
library(RColorBrewer)
library(ggdark)
library(extrafont)

emperors <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-13/emperors.csv")

emperors <- emperors %>%
  separate(reign_start, 
           sep = "-", 
           into = c("reign_start_year", "reign_start_month", "reign_start_day"),
           remove = FALSE) %>%
  separate(reign_end, 
           sep = "-", 
           into = c("reign_end_year", "reign_end_month", "reign_end_day"),
           remove = FALSE)

emperors$reign_start_year  <- as.numeric(emperors$reign_start_year)
emperors$reign_start_month <- as.numeric(emperors$reign_start_month)
emperors$reign_start_day   <- as.numeric(emperors$reign_start_day)
emperors$reign_end_year    <- as.numeric(emperors$reign_end_year)
emperors$reign_end_month   <- as.numeric(emperors$reign_end_month)
emperors$reign_end_day     <- as.numeric(emperors$reign_end_day)

emperors$reign_start_year[1] <- -emperors$reign_start_year[1]
emperors$year <- emperors$reign_start_year
years <- data.frame(year = rep(-100:395))

df <- merge(years, emperors, all.x = TRUE, all.y = TRUE, by.x = "year", by.y = "year")

df <- df %>%
  fill(everything(), .direction = "down")

df = df[!duplicated(df$year),]

df <- df %>%
  mutate(century = ifelse(year < 0, "First Century BC",
                     ifelse(year < 100, "First Century",
                            ifelse(year < 200, "Second Century",
                                   ifelse(year < 300, "Third Century",
                                          ifelse(year < 400, "Fourth Century"))))))
df <- df %>%
  group_by(century) %>%
  mutate(century_year = row_number()) %>%
  mutate(century = factor(century, levels = c("First Century BC",
                                              "First Century",
                                              "Second Century",
                                              "Third Century",
                                              "Fourth Century")),
         dynasty = factor(dynasty, levels = c("Julio-Claudian",
                                              "Flavian",
                                              "Nerva-Antonine",
                                              "Severan",
                                              "Gordian",
                                              "Constantinian",
                                              "Valentinian",
                                              "Theodosian")))

df %>%
  filter(!is.na(name)) %>%
  ggplot(aes(x = century_year, y = reorder(century, desc(century)))) +
  geom_line(aes(colour = dynasty), size = 4) +
  dark_theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = "Gadugi",
                                    size = 10,
                                    face = "bold"),
        plot.title = element_text(family = "Gadugi",
                                  hjust = 0.4,
                                  face = "bold",
                                  size = 20),
        text = element_text(family = "Gadugi"),
        legend.position = "bottom",
        plot.subtitle = element_text(hjust = 0.4),
        plot.caption = element_text(size = 6, family = "Gadugi")) +
  labs(title = "Dynasties of the Roman Empire",
       subtitle = "In 27 BC Octavian was granted the title of Augustus by the Roman senate, making him the\n first Emporer and finalising the transition of Rome from a Republic into an Empire. During its reign the\n Roman Empire controlled as much as 5 million sq. km and was overseen by 68 emperors across \n 8 dynasties. It was finally split into East and West in 395AD, never to be united again.",
       colour = "Dynasty",
       caption = "Created by Henry Wakefield 28-08-2020 - Twitter:@henrywrover2") +
  scale_colour_brewer(palette = "Dark2") +
  ggsave(filename = "dynasties.png", height = 6, width = 10, dpi = 300, type = "cairo-png", units = "in")
