library(tidyverse)
library(lubridate)
library(EloRating)
library(ggrepel)
library(RColorBrewer)

matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-11-30/matches.csv') %>%
  mutate(match_date = ifelse(grepl("-", match_date) == TRUE, str_replace(match_date, pattern = "-\\d+", replacement = ""), match_date)) %>%
  mutate(date = mdy(match_date)) %>%
  filter(!winner %in% c("Asia XI", "Africa XI", "ICC World XI", "Pakistan awarded the match (opposition conceded)"))

elo <- matches %>%
  mutate(loser = ifelse(winner == team1, team2, team1),
         draw = ifelse(winner == "Match tied" | winner == "No result" | winner == "Match tied (D/L method)", TRUE, FALSE)) %>%
  select(date, winner, loser, draw) %>%
  filter(!winner %in% c("Match tied", "No result", "Match tied (D/L method)"))

res <- elo.seq(winner = elo$winner, loser = elo$loser, Date = elo$date, runcheck = TRUE)

elo$elo <- extract_elo(res, extractdate = elo$date, IDs = elo$winner)

df <- elo %>%
  filter(!winner %in% c("Kenya", "Bangladesh", "Canada", "Netherlands", "U.A.E.")) %>%
  mutate(year = year(date),
         mth_yr = as.Date(format(as.Date(date), "%Y-%m-01"))) %>%
  group_by(winner, mth_yr) %>%
  summarise(elo = round(mean(elo))) %>%
  ungroup()
  
df$winner <- with(df, reorder(winner, -elo))

### the plot

plot <- df %>%
  ggplot(aes(x = mth_yr, y = elo)) +
  geom_line(data = df %>%
              filter(winner != "Australia"), aes(colour = winner), size = 0.8, alpha = 0.2) +
  geom_line(data = df %>%
              filter(winner == "Australia"),
            colour = "ForestGreen", size = 1, alpha = 0.8) +
  scale_y_continuous() +
  scale_x_date() +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  labs(title = "Australia's rise to ODI dominance",
       subtitle = "Creating an ELO ranking system for ODI matches of the top nations between 1996 and 2005",
       x = "",
       y = "ELO Rating")
   
ggsave(plot, filename = "cricket.tiff", height = 6, width = 8)
