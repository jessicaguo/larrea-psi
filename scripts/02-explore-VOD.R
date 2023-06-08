# Read in VOD data for Santa Rita
# Compare to Shrub 4

library(readxl)
library(tidyverse)
library(janitor)
library(cowplot)

# Read manual data and summarize
manual <- read_csv("data_clean/pressure_chamber.csv")

manual_sum <- manual %>%
  group_by(date) %>%
  summarize(predawn_mean = mean(predawn_m_pa, na.rm = TRUE),
            predawn_sd = sd(predawn_m_pa, na.rm = TRUE),
            predawn_n = n())

# Read in gcc data
# 2 phenocams, 33 is landscape view, 42 is closeup of creosote only
gcc_33 <- read_csv("data_vod/NEON.D14.SRER.DP1.00033_SH_1000_1day.csv",
                   skip = 24)

gcc_42 <- read_csv("data_vod/NEON.D14.SRER.DP1.00042_SH_1000_1day.csv",
                   skip = 24)


# Read in VOD data
vod_ndvi <- read_xlsx("data_vod/VOD_NDVI.xlsx") %>%
  clean_names() %>%
  rename(vod130am = vod1_30am,
         vod130pm = vod1_30pm) %>%
  left_join(manual_sum, by = join_by(date))

# Plot VOD and derivatives
fig_top <- vod_ndvi %>%
  ggplot(aes(x = date)) +
  geom_point(aes(y = vod130am, color = "VOD")) +
  geom_point(aes(y = am_vod_ndvi/10, color = "VOD/NDVI")) +
  scale_y_continuous(expression(paste(VOD["0130"])),
                     sec.axis = sec_axis(~.*10, 
                                         expression(paste(frac(VOD["0130"], "NDVI"))))) +
  theme_bw() +
  theme(legend.position = "bottom")

fig_bot <- vod_ndvi %>%
  ggplot(aes(x = date)) +
  geom_point(aes(y = vod130am, color = "VOD")) +
  geom_point(aes(y = exp_vod_ndvi_1000_0*100-100, color = "exp(VOD/NDVI)")) +
  scale_y_continuous(expression(paste(VOD["0130"])),
                     sec.axis = sec_axis(~(.+100)/100, 
                                         expression(paste(e^frac("VOD", "NDVI"))))) +
  theme_bw() +
  theme(legend.position = "bottom")
  

plot_grid(fig_top, fig_mid, fig_bot, ncol = 1)


# Compare different indices of greenness/biomass
green <- vod_ndvi %>%
  select(date, starts_with("sentinel")) %>%
  left_join(select(gcc_33, date, gcc_mean, gcc_std), by = "date") %>%
  rename(gcc_mean_33 = gcc_mean,
         gcc_std_33 = gcc_std) %>%
  left_join(select(gcc_42, date, gcc_mean, gcc_std), by = "date") %>%
  rename(gcc_mean_42 = gcc_mean,
         gcc_std_42 = gcc_std)

# Plot greenness
green %>%
  ggplot(aes(x = date)) +
  geom_point(aes(y = sentinel2ndvi_interpolated, color = "NDVI-interp")) +
  geom_point(aes(y = sentinel2ndvi, color = "NDVI-raw")) +
  geom_point(aes(y = gcc_mean_33-0.3,
                 color = "gcc-landscape")) +
  geom_errorbar(aes(ymin = (gcc_mean_33 - gcc_std_33)-0.3,
                      ymax = (gcc_mean_33 + gcc_std_33)-0.3,
                    color = "gcc-landscape"),
                alpha = 0.5,
                width = 0) +
  geom_point(aes(y = gcc_mean_42-0.3,
                 color = "gcc-creosote")) +
  geom_errorbar(aes(ymin = (gcc_mean_42 - gcc_std_33)-0.3,
                      ymax = (gcc_mean_42 + gcc_std_33)-0.3,
                      color = "gcc-creosote"),
                alpha = 0.5,
                width = 0) +
  scale_y_continuous("NDVI",
                     sec.axis = sec_axis(~.+0.3, 
                                         "gcc")) +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(color = "Source") 
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
       shape = "Method") +
  theme_bw()

# Match with 1:30 am values of VOD

branch_4a_am <- branch_4a %>%
  filter(time == hms("01:30:00"))

branch_4b_am <- branch_4b %>%
  filter(time == hms("01:30:00"))

vod_psy <- vod_ndvi %>%
  left_join(branch_4a_am[, c(1, 6)], by = "date") %>%
  rename(WP_4a_1am = WP) %>%
  left_join(branch_4b_am[, c(1, 6)], by = "date") %>%
  rename(WP_4b_1am = WP)


# Plot AM VOD with psy
vod_psy %>%
  ggplot(aes(x = date)) +
  geom_point(aes(y = vod130am, shape = "VOD")) +
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
  

# Plot AM VOD / NDVI with psy

vod_psy %>%
  ggplot(aes(x = date)) +
  geom_point(aes(y = am_vod_ndvi, shape = "VOD")) +
  geom_point(aes(y = WP_4a_1am+13, color = "4a",
                 shape = "psychrometer")) +
  geom_point(aes(y = WP_4b_1am+13, color = "4b",
                 shape = "psychrometer")) +
  geom_errorbar(data = manual_sum, aes(x = date,
                                       ymin = (predawn_mean - predawn_sd)+13,
                                       ymax = (predawn_mean + predawn_sd)+13),
                color = "gray30",
                width = 0) +
  geom_point(data = manual_sum, aes(x = date, 
                                    y = predawn_mean+13,
                                    shape = "chamber"),
             color = "gray30",
             size = 2) +
  scale_y_continuous(expression(paste(frac(VOD["1:30 am"], "NDVI"))), 
                     sec.axis = sec_axis(~.-13,
                                         name = expression(paste(Psi["1:30 am"], " (MPa)")))) +
  labs(shape = "method", color = "branch") +
  theme_bw() +
  theme(legend.position = "bottom")


##### Explore linear relationship by period #####

vod_psy %>%
  filter(date >= as.Date("2023-03-15"),
         date < as.Date("2023-05-01")) %>%
  ggplot(aes(x = am_vod_ndvi)) +
  geom_point(aes(y = WP_4a_1am, color = "4a")) +
  geom_point(aes(y = WP_4b_1am, color = "4b")) +
  scale_x_continuous(expression(paste(frac(VOD["1:30 am"], "NDVI")))) +
  scale_y_continuous(expression(paste(Psi["1:30 am"], " (MPa)"))) +
  labs(color = "Branch") +
  theme_bw()

vod_psy %>%
  filter(date >= as.Date("2023-05-19")) %>%
  ggplot(aes(x = am_vod_ndvi)) +
  geom_point(aes(y = WP_4a_1am, color = "4a")) +
  geom_point(aes(y = WP_4b_1am, color = "4b")) +
  theme_bw()

m1 <- lm(WP_4a_1am ~ `VOD_1:30AM`, data = vod_psy)
summary(m1)
