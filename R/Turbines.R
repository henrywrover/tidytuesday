library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
### reading in data and some renaming/cleaning for my own benefit

wind <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-10-27/wind-turbine.csv') %>%
  rename(year = commissioning_date,
         capacity = turbine_rated_capacity_k_w,
         diameter = rotor_diameter_m,
         height = hub_height_m,
         province = province_territory) %>%
  separate(col = turbine_number_in_project, sep = "/", into = c("number_in_project", "total_in_project")) %>%
  mutate(year = ifelse(project_name == "Castle River Wind Farm", 1997,
                       ifelse(project_name == "Summerview", 2004,
                              ifelse(project_name == "McBride Lake", 2003, year))),
         year = as.numeric(year),
         number_in_project = as.integer(number_in_project),
         total_in_project = as.integer(total_in_project))

wind %>%
  group_by(project_name, year) %>%
  summarise(project_capacity = mean(total_project_capacity_mw, na.rm= TRUE),
            turbines = n()) %>%
  mutate(capacity_per_turbine = project_capacity / turbines) %>%
  group_by(year) %>%
  summarise(capacity_per_turbine = mean(capacity_per_turbine)) %>%
  ggplot(aes(x = year, y = capacity_per_turbine)) +
  geom_col(fill = "#ff0000", colour = "grey25", size = 1.2) +
  geom_smooth(colour = "grey20", size = 0.8, se = FALSE) +
  labs(title = "Working smarter, not harder: Average Capacity (MW) per Turbine Continues to Increase in Canada",
       subtitle = "Wind Turbine Projects in Canada have seen a dramatic increase in productivity over the past 20 years,\nwith a higher capacity per Turbine in 2018 than ever before",
       y= "Capacity (MW) per Turbine",
       caption = "Source | https://open.canada.ca\nTwitter | @henrywrover2\nGithub | henrywrover\n 28th October 2020") +
  theme_minimal()+
  theme(plot.title = element_text(size = 9, face = "bold"),
        plot.subtitle = element_text(size = 7),
        plot.margin = unit(c(1,1,1,1), "cm"),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 7),
        axis.text = element_text(size = 6),
        plot.caption = element_text(size = 5)) +
  ggsave(filename = "turbines.png", height = 5, width = 7, dpi = 150, type = "cairo-png")
