# Read in VOD data for Santa Rita
# Compare to Shrub 4

library(readxl)
library(tidyverse)

# Read manual data and summarize
manual <- read_csv("data_clean/pressure_chamber.csv")

manual_sum <- manual %>%
  group_by(date) %>%
  summarize(predawn_mean = mean(predawn_m_pa, na.rm = TRUE),
            predawn_sd = sd(predawn_m_pa, na.rm = TRUE),
            predawn_n = n())

# Read in VOD data
vod <- read_xlsx("data_vod/Para25km_AD_Santa_Rita_020123_052623.xlsx") %>%
  left_join(manual_sum, by = join_by(date))

vod %>%
  ggplot(aes(x = date)) +
  geom_pointrange(aes(ymin = predawn_mean - predawn_sd,
                      ymax = predawn_mean + predawn_sd,
                      y = predawn_mean,
                      color = "Manual - predawn")) +
  geom_point(aes(y = -(`VOD_1:30AM`)*14+7, color = "VOD - 1:30AM"))

# Read in branches a and b from shrub 4
branch_4a <- read_csv("data_appended/psy_4a.csv") %>%
  mutate(dt = paste(date, time) %>%
           as.POSIXct(format = "%Y-%m-%d %H:%M:%S", tz = "America/Phoenix")) %>%
  rename(WP = corrected_water_potential_m_pa)
branch_4b <- read_csv("data_appended/psy_4b.csv") %>%
  mutate(dt = paste(date, time) %>%
           as.POSIXct(format = "%Y-%m-%d %H:%M:%S", tz = "America/Phoenix")) %>%
  rename(WP = corrected_water_potential_m_pa)

ggplot() +
  geom_point(data = branch_4a, aes(x = dt, y = WP, color = "4a",
                                   shape = "psychrometer")) +
  geom_point(data = branch_4b, aes(x = dt, y = WP, color = "4b",
                                   shape = "psychrometer")) +
  geom_point(data = filter(manual, shrub_id == 4), 
             aes(x = date, y = predawn_m_pa, shape = "manual")) +
  labs(color = "Branch",
       shape = "Method")


# Match with 1:30 am values of VOD

branch_4a_am <- branch_4a %>%
  filter(time == hms("01:30:00"))

branch_4b_am <- branch_4b %>%
  filter(time == hms("01:30:00"))

vod_psy <- vod %>%
  left_join(branch_4a_am[, c(1, 6)], by = "date") %>%
  rename(WP_4a_1am = WP) %>%
  left_join(branch_4b_am[, c(1, 6)], by = "date") %>%
  rename(WP_4b_1am = WP)

vod_psy %>%
  ggplot(aes(x = date)) +
  geom_point(aes(y = `VOD_1:30AM`, shape = "VOD")) +
  geom_point(aes(y = WP_4a_1am*.09+1.1, color = "4a",
                 shape = "psychrometer")) +
  geom_point(aes(y = WP_4b_1am*.09+1.1, color = "4b",
                 shape = "psychrometer")) +
  geom_errorbar(data = manual_sum, aes(x = date,
                                       ymin = (predawn_mean - predawn_sd)*.09+1.1,
                                       ymax = (predawn_mean + predawn_sd)*.09+1.1),
                color = "gray30",
                width = 0) +
  geom_point(data = manual_sum, aes(x = date, 
                                         y = predawn_mean*.09+1.1,
                                         shape = "chamber"),
                  color = "gray30",
             size = 2) +
  scale_y_continuous(expression(paste(VOD["1:30 am"])), 
                     sec.axis = sec_axis(~(.-1.12)/.09,
                                         name = expression(paste(Psi["1:30 am"], " (MPa)")))) +
  labs(shape = "method", color = "branch") +
  theme_bw() +
  theme(legend.position = "bottom")
  


vod_psy %>%
  ggplot(aes(x = `VOD_1:30AM`)) +
  geom_point(aes(y = WP_4a_1am, color = date))

m1 <- lm(WP_4a_1am ~ `VOD_1:30AM`, data = vod_psy)
summary(m1)
