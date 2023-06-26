# Plot half-hourly appended data

library(tidyverse)

psy4a <- read_csv("data_appended/psy_4a.csv")
psy4b <- read_csv("data_appended/psy_4b.csv")
psy2a <- read_csv("data_appended/psy_2a.csv")
psy2b <- read_csv("data_appended/psy_2b.csv")

ggplot() +
  geom_point(data = psy4a %>%
               filter(date >= as.Date("2023-04-15"),
                      date <= as.Date("2023-04-30")), 
             aes(x = dt,
                 y = corrected_water_potential_m_pa,
                 color = "4a")) +
  geom_point(data = psy4b %>%
               filter(date >= as.Date("2023-04-15"),
                      date <= as.Date("2023-04-30")), 
             aes(x = dt,
                 y = corrected_water_potential_m_pa,
                 color = "4b")) +
  scale_color_manual(values = c("forestgreen", "darkseagreen")) +
  scale_y_continuous(expression(paste(Psi, " (MPa)"))) +
  theme_bw(base_size = 14) +
  labs(color = "branch") +
  theme(axis.title.x = element_blank())

ggsave("figs/psi_4a_4b.png",
       height = 3,
       width = 8,
       units = "in")
