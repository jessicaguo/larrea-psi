# Create figures for AGU ppt

library(tidyverse)
library(slider)
library(harrypotter)
library(cowplot)

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
                    color = "psy - predawn"),
                width = 0,
                alpha = 0.3) +
  geom_point(data = psy_daily_site, 
             aes(x = date, y = PD,
                 color = "psy - predawn")) +
  geom_errorbar(data = psy_daily_site, 
                aes(x = date, 
                    ymin = MD - MD_sd,
                    ymax = MD + MD_sd,
                    color = "psy - midday"),
                width = 0,
                alpha = 0.3) +
  geom_point(data = psy_daily_site, 
             aes(x = date, y = MD,
                 color = "psy - midday")) +
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
        legend.position = c(0.12, 0.2),
        legend.title = element_blank())
fig1

ggsave(filename = "figs/psy-chamber-ts.png",
       fig1,
       width = 8,
       height = 3)

# R^2
manual_psy <- manual_sum |> 
  left_join(psy_daily_site)

m1 <- lm(PD ~ predawn_mean, data = manual_psy)
summary(m1) # R2 = 0.989
cf <- coef(m1)

fig_s1 <- manual_psy |> 
  ggplot(aes(x = predawn_mean,
             y = PD)) +
  # geom_abline(slope = 1, intercept = 0, lty = 1) +
  geom_abline(slope = cf[2], intercept = cf[1], lty = 2) +
  geom_errorbarh(aes(xmin = predawn_mean - predawn_sd,
                     xmax = predawn_mean + predawn_sd),
                 alpha = 0.25) +
  geom_errorbar(aes(ymin = PD - PD_sd,
                    ymax = PD + PD_sd),
                alpha = 0.25) +
  geom_point() +
  geom_text(x = -8, y = -1, label = "R^2 == 0.989",
            parse = TRUE,
            hjust = 0) +
  scale_x_continuous(expression(paste(Psi[PD]^chamber, " (MPa)")),
                     breaks = seq(-2, -6, by = -2),
                     limits = c(-8, -0.5)) +
  scale_y_continuous(expression(paste(Psi[PD]^psy, " (MPa)")),
                     breaks = seq(-2, -6, by = -2),
                     limits = c(-8, -0.5)) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank()) +
  coord_equal()

fig_s1

ggsave(filename = "figs/psy-chamber-r2.png",
       fig_s1,
       width = 3,
       height = 3)

##### Figure of environmental variables #####

swc <- read_csv(file = "data_clean/neon_swcdaily.csv") |> 
  filter(date >= min(psy_daily_site$date),
         date <= max(psy_daily_site$date))

atm <- read_csv(file = "data_clean/neon_atmdaily.csv") |> 
  filter(date >= min(psy_daily_site$date),
         date <= max(psy_daily_site$date))

fig_top <- atm |> 
  ggplot(aes(x = date)) +
  geom_col(aes(y = ppt_mm, fill = "ppt")) +
  geom_point(aes(y = Dmax, col = "Dmax")) +
  scale_y_continuous("Precip (mm)", 
                     sec.axis = sec_axis(~., expression(paste(D[max], " (kPa)")))) +
  scale_color_manual(values = c("#d3a625")) +
  scale_fill_manual(values = c("#222f5b")) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank()) +
  guides(color = "none",
         fill = "none")

fig_bottom <- swc |> 
  ggplot(aes(x = date)) +
  geom_point(aes(y = m_p34_6, col = "06 cm")) +
  geom_point(aes(y = m_p34_16, col = "16 cm")) +
  geom_point(aes(y = m_p34_26, col = "26 cm")) +
  scale_y_continuous(expression(paste(Theta, " (", cm^3, cm^-3, ")"))) +
  scale_color_hp_d(option = "NewtScamander") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        legend.position = c(0.9, 0.7),
        legend.title = element_blank()) 
  

fig_env <- plot_grid(fig_top, fig_bottom, ncol = 1,
                     align = "v")

ggsave(filename = "figs/neon-env.png",
       fig_env,
       width = 8,
       height = 4.5)

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

##### Add seasons and plot VOD-psy relationship #####

seasons <- data.frame(season = c("spring", "pre-monsoon", "monsoon", "fall"),
                      st = c("2023-02-17", "2023-05-01", "2023-07-01", "2023-10-01"),
                      en = c("2023-04-30", "2023-06-30", "2023-09-30", "2023-11-03")) |> 
  mutate(st = as.Date(st, tz = "America/Phoenix"),
         en = as.Date(en, tz = "America/Phoenix"),
         season = factor(season, levels = c("spring", "pre-monsoon", "monsoon", "fall")))

PD_seasons <- PD |> 
  left_join(seasons, by = join_by(date >= st, date <= en)) |> 
  left_join(select(swc, date, m_p34_6, m_p12_16, m_p34_16), by = join_by(date)) |> 
  left_join(select(atm, date, ppt_mm), by = join_by(date))


# timeseries
fig3 <- PD_seasons |> 
  filter(season != "fall") |> 
  ggplot(aes(x = date)) +
  geom_point(aes(y = PD, color = "predawn")) +
  geom_point(aes(y = LPRM_X_130AM_7*17-8, color = "VOD - 07 day")) +
  # geom_point(aes(y = LPRM_X_130AM*17-8, color = "VOD - raw"),
  #            size = 0.75) +
  geom_line(aes(y = m_p34_16*17-8, color = "SWC 16 cm")) +
  geom_line(aes(y = m_p34_6*17-8, color = "SWC 06 cm")) +
  geom_col(aes(y = ppt_mm/10)) +
  scale_y_continuous(expression(paste(Psi[plant], " (MPa) | precip (cm)")), 
                     sec.axis = sec_axis(~(.+8)/17, 
                                         expression(paste("VOD | SWC (", cm^3, cm^-3, ")")))) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  scale_color_manual(values = c("#B9CD93", "#FEDA26", "#D9D2AD", "#946b2d")) +
  facet_grid(cols = vars(season), scales = "free_x",
             space = "free_x",
             switch = "x") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "top",
        legend.title = element_blank()) +
  guides(color = guide_legend(override.aes = list(linetype = c(0, 1, 1, 0),
                                                  shape = c(19, NA, NA, 19))))
fig3b <- PD_seasons |> 
  filter(season != "fall") |> 
  ggplot(aes(x = date)) +
  geom_point(aes(y = PD, color = "predawn")) +
  geom_point(aes(y = LPRM_X_130AM_7*17-8, color = "VOD - 07 day")) +
  geom_point(aes(y = LPRM_X_130AM*17-8, color = "VOD - raw"),
             size = 0.75) +
  geom_line(aes(y = m_p34_16*17-8, color = "SWC 16 cm")) +
  geom_line(aes(y = m_p34_6*17-8, color = "SWC 06 cm")) +
  geom_col(aes(y = ppt_mm/10)) +
  scale_y_continuous(expression(paste(Psi[plant], " (MPa) | precip (cm)")), 
                     sec.axis = sec_axis(~(.+8)/17, 
                                         expression(paste("VOD | SWC (", cm^3, cm^-3, ")")))) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  scale_color_manual(values = c("#B9CD93", "#FEDA26", "#D9D2AD", "#946b2d", "gray40")) +
  facet_grid(cols = vars(season), scales = "free_x",
             space = "free_x",
             switch = "x") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "top",
        legend.title = element_blank()) +
  guides(color = guide_legend(override.aes = list(linetype = c(0, 1, 1, 0, 0),
                                                  shape = c(19, NA, NA, 19, 19))))
                                                  
ggsave(filename = "figs/psy-vod-swc-ts.png",
       fig3,
       width = 8,
       height = 3)
ggsave(filename = "figs/psy-vod-swc-raw-ts.png",
       fig3b,
       width = 8,
       height = 3)


PD_seasons |> 
  filter(season != "fall") |> 
  ggplot(aes(y = PD)) +
  geom_point(aes(x = LPRM_X_130AM_7, color = m_p34_6)) +
  # geom_point(aes(x = LPRM_X_130AM_9, color = "VOD - 09 day")) +
  # geom_point(aes(x = LPRM_X_130AM_11, color = "VOD - 11 day")) +
  # scale_color_manual(values = c("#B9CD93","#946b2d", "#726255", "#372e29")) +
  facet_wrap(~season, nrow = 1)

##### Use jumps in SWC to determine pulses #####

swc_diff <- swc |> 
  select(date, m_p34_6, m_p34_16) |> 
  mutate(m_6_lag1 = lag(m_p34_6),
         m_16_lag1 = lag(m_p34_16),
         diff_6 = m_p34_6 - m_6_lag1,
         diff_16 = m_p34_16 - m_16_lag1) |> 
  left_join(seasons, by = join_by(date >= st, date <= en)) 

swc_s <- swc_diff |> 
  filter(season == "spring")|> 
  filter(diff_6 > 0.01 & diff_16 > 0.01)

swc_pm <- swc_diff |> 
  filter(season == "pre-monsoon")|> 
  filter(diff_6 > 0.01 & diff_16 > 0)

swc_m <- swc_diff |> 
  filter(season == "monsoon")|> 
  filter(diff_6 > 0.01 & diff_16 > 0) |> 
  mutate(diff_days = date - lag(date)) |> 
  filter(is.na(diff_days) | diff_days > 7) # At least 7 days since last pulse

min(swc_m$diff_days, na.rm = TRUE) # pulses are at least 12 days long

pulses <- data.frame(pulse = 1:6,
                     season = c("spring", "pre-monsoon", rep("monsoon", 4)),
                     st = c(swc_s$date, swc_pm$date, swc_m$date)) |> 
  mutate(en = st + 12,
         season = factor(season, levels = c("spring", "pre-monsoon", "monsoon")))

fig3c <- PD_seasons |> 
  filter(season != "fall") |> 
  ggplot() +
  geom_rect(data = pulses,
            aes(xmin = st, xmax = en,
                ymin = -Inf, ymax = Inf),
            color ="gray", alpha = 0.2) +
  geom_text(data = pulses,
            aes(x = st + 6, y = -8,
                label = pulse)) +
  geom_point(aes(x = date, y = PD, color = "predawn")) +
  geom_point(aes(x = date, y = LPRM_X_130AM_7*17-8, color = "VOD - 07 day")) +
  geom_point(aes(x = date, y = LPRM_X_130AM*17-8, color = "VOD - raw"),
             size = 0.75) +
  geom_line(aes(x = date, y = m_p34_16*17-8, color = "SWC 16 cm")) +
  geom_line(aes(x = date, y = m_p34_6*17-8, color = "SWC 06 cm")) +
  geom_col(aes(x = date, y = ppt_mm/10)) +
  scale_y_continuous(expression(paste(Psi[plant], " (MPa) | precip (cm)")), 
                     sec.axis = sec_axis(~(.+8)/17, 
                                         expression(paste("VOD | SWC (", cm^3, cm^-3, ")")))) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  scale_color_manual(values = c("#B9CD93", "#FEDA26", "#D9D2AD", "#946b2d", "gray40")) +
  facet_grid(cols = vars(season), scales = "free_x",
             space = "free_x",
             switch = "x") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "top",
        legend.title = element_blank()) +
  guides(color = guide_legend(override.aes = list(linetype = c(0, 1, 1, 0, 0),
                                                  shape = c(19, NA, NA, 19, 19))))

ggsave(filename = "figs/psy-vod-swc-raw-ts-seasons.png",
       fig3c,
       width = 8,
       height = 3)

PD_pulses <- PD |> 
  left_join(select(pulses, -season), by = join_by(date >= st, date <= en)) |> 
  left_join(seasons, by = join_by(date >= st, date <= en)) |> 
  mutate(pulse = factor(pulse, levels = 1:6))

fig4 <- PD_pulses |> 
  filter(season != "fall") |> 
  ggplot(aes(y = PD)) +
  geom_point(aes(x = LPRM_X_130AM_7, color = pulse)) +
  scale_color_brewer(palette = "Dark2",
                     na.value = "gray") +
  scale_y_continuous(expression(paste(Psi[PD], " (MPa)"))) +
  scale_x_continuous("VOD - 07 day") +
  facet_wrap(~season, nrow = 1) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        legend.position = "top") +
  guides(color = guide_legend(nrow = 1))

ggsave(filename = "figs/psy-vod-bypulse.png",
       fig4,
       width = 8,
       height = 3)
