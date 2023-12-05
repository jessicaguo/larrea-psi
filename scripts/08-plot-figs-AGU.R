# Create figures for AGU ppt

library(tidyverse)
library(slider)

##### Figure of manual and psy water potentials ####
# Read manual data and summarize
manual <- read_csv("data_clean/pressure_chamber.csv") %>%
  mutate(month = lubridate::month(dt))

manual_sum <- manual %>%
  group_by(date) %>%
  summarize(predawn_mean = mean(predawn_m_pa, na.rm = TRUE),
            predawn_sd = sd(predawn_m_pa, na.rm = TRUE),
            predawn_n = n()) %>%
  mutate(date = as.Date(date))

# Read in site-level predawn and midday

psy_daily_site <- read_csv("data_clean/psy/psy_daily_site.csv")

fig1 <- ggplot() +
  geom_errorbar(data = psy_daily_site, 
                aes(x = date, 
                    ymin = PD - PD_sd,
                    ymax = PD + PD_sd,
                    color = "predawn"),
                width = 0,
                alpha = 0.3) +
  geom_point(data = psy_daily_site, 
             aes(x = date, y = PD,
                 color = "predawn")) +
  geom_errorbar(data = psy_daily_site, 
                aes(x = date, 
                    ymin = MD - MD_sd,
                    ymax = MD + MD_sd,
                    color = "midday"),
                width = 0,
                alpha = 0.3) +
  geom_point(data = psy_daily_site, 
             aes(x = date, y = MD,
                 color = "midday")) +
  geom_pointrange(data = manual_sum,
                  aes(x = date, y = predawn_mean,
                      ymin = predawn_mean - predawn_sd,
                      ymax = predawn_mean + predawn_sd,
                      color = "chamber")) +
  scale_y_continuous(expression(paste(Psi[plant], " (MPa)"))) +
  scale_color_manual(values = c("#2D55A9", "#455F51", "#B9CD93")) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        legend.position = c(0.1, 0.2),
        legend.title = element_blank())
fig1

ggsave(filename = "figs/psy-chamber-ts.png",
       fig1,
       width = 8,
       height = 3)

##### Figure of environmental variables #####


##### Bring in VOD data from Charlie #####
# LPRM = Land Parameter Retrieval Model
# a forward radiative transfer model to get VOD
vod <- read_csv("data_vod/xSR_LPRM_VOD_Daily_Nov.csv") |> 
  rename(date = Dates) |> 
  filter(date >= min(psy_daily_site$date) - 15,
         date <= max(psy_daily_site$date) + 10) |> 
  mutate(band = str_extract(Dataset, "C1|C2|X"),
         tod = str_extract(Dataset, "DAY|NIGHT"),
         tod = case_when(tod == "DAY" ~ "130PM",
                         tod == "NIGHT" ~ "130AM"))

lprm <- vod |> 
  select(date, Value, band, tod) |> 
  filter(band == "X") |> 
  arrange(date) |> 
  pivot_wider(values_from = Value,
              names_from = c(band, tod),
              names_glue = "LPRM_{band}_{tod}") |> 
  mutate(LPRM_X_130AM_7 = slide_vec(LPRM_X_130AM, ~mean(.x, na.rm = TRUE), 
                                     .before = 3, .after = 3,
                                     .complete = TRUE),
         LPRM_X_130AM_9 = slide_vec(LPRM_X_130AM, ~mean(.x, na.rm = TRUE), 
                                     .before = 4, .after = 4,
                                     .complete = TRUE),
         LPRM_X_130AM_11 = slide_vec(LPRM_X_130AM, ~mean(.x, na.rm = TRUE), 
                                                .before = 5, .after = 5,
                                                .complete = TRUE),
         LPRM_X_130AM_15 = slide_vec(LPRM_X_130AM, ~mean(.x, na.rm = TRUE), 
                                                .before = 7, .after = 7,
                                                .complete = TRUE),
         LPRM_X_130AM_31 = slide_vec(LPRM_X_130AM, ~mean(.x, na.rm = TRUE), 
                                                .before = 15, .after = 15,
                                                .complete = TRUE),
         LPRM_X_130PM_7 = slide_vec(LPRM_X_130PM, ~mean(.x, na.rm = TRUE), 
                                     .before = 3, .after = 3,
                                     .complete = TRUE),
         LPRM_X_130PM_9 = slide_vec(LPRM_X_130PM, ~mean(.x, na.rm = TRUE), 
                                     .before = 4, .after = 4,
                                     .complete = TRUE),
         LPRM_X_130PM_11 = slide_vec(LPRM_X_130PM, ~mean(.x, na.rm = TRUE), 
                                     .before = 5, .after = 5,
                                     .complete = TRUE),
         LPRM_X_130PM_15 = slide_vec(LPRM_X_130PM, ~mean(.x, na.rm = TRUE), 
                                     .before = 7, .after = 7,
                                     .complete = TRUE),
         LPRM_X_130PM_31 = slide_vec(LPRM_X_130PM, ~mean(.x, na.rm = TRUE), 
                                     .before = 15, .after = 15,
                                     .complete = TRUE)) |> 
  filter(date >= min(psy_daily_site$date),
         date <= max(psy_daily_site$date))


# Save out
write_csv(lprm, "data_clean/LPRM_X_VOD_smoothed.csv")

##### Figure of PD and VOD as timeseries #####
# Read in both datasets
lprm <- read_csv("data_clean/LPRM_X_VOD_smoothed.csv")

psy_daily_site <- read_csv("data_clean/psy/psy_daily_site.csv")
psy_30_site <- read_csv("data_clean/psy/psy_30_site.csv")
# Create PD timeseries
psy_130AM <- psy_30_site |> 
  mutate(date = as.Date(dt, tz = "America/Phoenix"),
         hour = lubridate::hour(dt),
         minute = lubridate::minute(dt)) |> 
  filter(hour == 13 & minute == 30) |> 
  select(-hour, -minute)

PD <- psy_daily_site |> 
  select(date, PD) |> 
  left_join(psy_130AM, by = join_by("date")) |> 
  left_join(select(lprm, date, contains("130AM")),
            by = join_by("date"))

fig2 <- PD |> 
  ggplot(aes(x = date)) +
  geom_point(aes(y = PD, color = "predawn")) +
  # geom_point(aes(y = WP_m, color = "130 AM")) +
  geom_point(aes(y = LPRM_X_130AM_7*17-8, color = "LPRM X - 07 day")) +
  geom_point(aes(y = LPRM_X_130AM_9*17-8, color = "LPRM X - 09 day")) +
  geom_point(aes(y = LPRM_X_130AM_11*17-8, color = "LPRM X - 11 day")) +
  scale_color_manual(values = c("#946b2d", "#726255", "#372e29", "#B9CD93")) +
  scale_y_continuous(expression(paste(Psi[plant], " (MPa)")), 
                     sec.axis = sec_axis(~(.+8)/17, "VOD (130AM)")) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        legend.position = c(0.15, 0.25),
        legend.title = element_blank())
fig2

ggsave(filename = "figs/psy-vod-ts.png",
       fig2,
       width = 8,
       height = 3)

##### 
PD |> 
  ggplot(aes(x = PD)) +
  geom_point(aes(y = LPRM_X_130AM_7*17-8, color = "VOD - 07 day")) +
  geom_point(aes(y = LPRM_X_130AM_9*17-8, color = "VOD - 09 day")) +
  geom_point(aes(y = LPRM_X_130AM_11*17-8, color = "VOD - 11 day")) +
  scale_color_manual(values = c("#B9CD93","#946b2d", "#726255", "#372e29"))
  # scale_y_continuous(expression(paste(Psi[plant], " (MPa)")), 
  #                    sec.axis = sec_axis(~(.+8)/17, "VOD"))  
  
