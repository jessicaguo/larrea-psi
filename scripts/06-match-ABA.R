# Make plot to note ABA sampling dates and WP

library(readxl)
library(tidyverse)
library(janitor)
library(cowplot)
library(slider)

# Read manual data and summarize
manual <- read_csv("data_clean/pressure_chamber.csv",
                   locale = locale(tz = "America/Phoenix")) 

manual_sum <- manual %>%
  group_by(date) %>%
  summarize(predawn_mean = mean(predawn_m_pa, na.rm = TRUE),
            predawn_sd = sd(predawn_m_pa, na.rm = TRUE),
            predawn_n = n(),
            sampled = ifelse(all(!is.na(sampled)), TRUE, FALSE)) %>%
  mutate(date = as.Date(date, tz = "America/Phoenix"))

##### Read in all PSY timeseries, extract 1:30 am, and calculate mean #####

fn_appended <- list.files("data_appended")

appended_list <- list()

for(fn in fn_appended) {
  branch <- str_extract(fn, "\\d{1}[a-z]{1}")
  shrub <- str_extract(fn, "\\d{1}")
  temp <- read_csv(paste0("data_appended/", fn),
                   locale=locale(encoding="latin1", tz = "America/Phoenix")) %>%
    select(date, time, dt, corrected_water_potential_m_pa) %>%
    rename(WP = corrected_water_potential_m_pa) %>%
    filter(time == hms("05:00:00")) %>%
    drop_na() %>%
    mutate(branch_id = branch,
           shrub_id = shrub)
  
  appended_list[[fn]] <- temp
}

# Raw values for 1:30 am
appended_raw <- do.call(bind_rows, appended_list)

# Summarize to shrub, then to stand
appended_sum <- appended_raw %>%
  group_by(date, dt, shrub_id) %>%
  summarize(WP_mean = mean(WP),
            WP_sd = sd(WP),
            WP_n = length(!is.na(WP))) %>%
  ungroup() %>%
  group_by(date) %>%
  summarize(WP_m = mean(WP_mean),
            WP_sd = sd(WP_mean),
            WP_n_branch = sum(WP_n),
            WP_n_shrub = sum(!is.na(WP_mean))) %>%
  mutate(date = as.Date(date, tz = "America/Phoenix"))
  
ggplot() +
  geom_errorbar(data = appended_sum,
                aes(x = date, 
                 ymin = WP_m - WP_sd,
                 ymax = WP_m + WP_sd),
                alpha = 0.25,
                width = 0) +
  geom_point(data = appended_sum,
             aes(x = date, y = WP_m)) +
  geom_errorbar(data = manual_sum,
             aes(x = date, 
                 ymin = predawn_mean - predawn_sd,
                 ymax = predawn_mean + predawn_sd),
             alpha = 0.5,
             color = "forestgreen",
             width = 0,
             size = 1) +
  geom_point(data = manual_sum,
             aes(x = date, 
                 y = predawn_mean),
             color = "forestgreen",
             shape = 17) +
  geom_point(data = manual_sum %>% filter(sampled == TRUE),
             aes(x = date,
                 y = -1),
             shape = 8,
             color = "midnightblue") +
  scale_x_date(date_breaks = "2 weeks",
               date_labels = "%m-%d") +
  scale_y_continuous(expression(paste(Psi[PD], " (MPa)"))) +
  theme_bw()
