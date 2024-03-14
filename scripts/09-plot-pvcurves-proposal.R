# VOD figures for proposal

library(tidyverse)
library(ggh4x)

# Read in VOD, psy, and pv datasets
lprm <- read_csv("data_clean/LPRM_X_VOD_smoothed.csv")

psy_daily_site <- read_csv("data_clean/psy/psy_daily_site.csv")
psy_30_site <- read_csv("data_clean/psy/psy_30_site.csv")

pv <- read_csv("data_clean/pmass_comb_20240308.csv")

# Define seasons
seasons <- data.frame(season = c("spring", "pre-monsoon", "monsoon", "fall"),
                      st = c("2023-02-17", "2023-05-01", "2023-07-01", "2023-10-01"),
                      en = c("2023-04-30", "2023-06-30", "2023-09-30", "2023-11-03")) |> 
  mutate(st = as.Date(st, tz = "America/Phoenix"),
         en = as.Date(en, tz = "America/Phoenix"),
         season = factor(season, levels = c("spring", "pre-monsoon", "monsoon", "fall")),
         mid = st + floor((en-st)/2))

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


PD_seasons <- PD |> 
  left_join(seasons, by = join_by(date >= st, date <= en))

PD_seasons |>
  ggplot(aes(x = ))


#### Timeseries of psy and VOD by season ####
PD |> 
  ggplot() +
  geom_rect(data = seasons,
            aes(xmin = st, xmax = en, 
                ymin = -1, ymax = Inf,
                fill = season)) +
  geom_text(data = seasons,
            aes(x = mid,
                y = -0.5,
                label = season)) +
  geom_point(aes(x = date, y = PD, color = "predawn")) +
  geom_point(aes(x = date, y = WP_m, color = "130 AM")) +
  geom_point(aes(x = date, y = LPRM_X_130AM_7*17-8, color = "LPRM X - 07 day")) +
  geom_point(aes(x = date, y = LPRM_X_130AM_9*17-8, color = "LPRM X - 09 day")) +
  geom_point(aes(x = date, y = LPRM_X_130AM_11*17-8, color = "LPRM X - 11 day")) +
  scale_color_manual(values = c( "forestgreen", "#946b2d", "#726255", "#372e29", "#B9CD93")) +
  scale_y_continuous(expression(paste(Psi[plant], " (MPa)")), 
                     sec.axis = sec_axis(~(.+8)/17, "VOD (130AM)")) +
  scale_x_date(date_labels = "%b %d",
               breaks = "1 month") +
  theme_bw(base_size = 14) +
  guides(fill = "none") +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        legend.position = c(0.15, 0.25),
        legend.title = element_blank())

#### PV curves by season ####

PD_seasons |>
  filter(date >= as.Date("2023-03-06")) |>
  mutate(season2  = case_when(season %in% c("spring", "pre-monsoon") ~ "early",
                              season %in% c("monsoon", "fall") ~ "late")) |>
  ggplot() +
  geom_point(aes(x = 1-LPRM_X_130AM_7,
                 y = -1/PD,
                 color = date)) +
  scale_color_gradient2(low = "forestgreen", mid = "coral", high = "blue",
                        midpoint = as.numeric(as.Date("2023-07-01")),
                        trans = "date") +
  facet_wrap(~season2)

# Monsoon gets really messy, but not until auguts or so


figa <- PD_seasons |>
  mutate(season2  = case_when(season %in% c("spring", "pre-monsoon") ~ "early",
                              season %in% c("monsoon", "fall") ~ "late")) |>
  filter(date >= as.Date("2023-03-06"),
         season2 == "early") |>
  ggplot() +
  geom_point(aes(x = LPRM_X_130AM_7,
                 y = PD,
                 color = season)) +
  scale_x_continuous("VOD") +
  scale_y_continuous(expression(paste(Psi[PD], " (MPa)")),
                     limits = c(-7, -1),
                     minor_breaks = c(-7, -5, -3, -1),
                     guide = "axis_minor") +
  scale_color_brewer(type = "qual", palette = 2) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.position = c(0.77, 0.1),
        ggh4x.axis.ticks.length.minor = rel(1))

figb <- pv |>
  filter(keep == TRUE,
         P.MPa < 6) |>
  mutate(ID = factor(ID)) |>
  ggplot() +
  # add mean TLP from PV analysis
  geom_rect(aes(ymin = -3.31, ymax = -2.82,
                xmin = -Inf, xmax = Inf),
            fill = "gray",
            alpha = 0.25) +
  geom_hline(yintercept = -3.05) +
  geom_point(aes(x = mass_lost,
                 y = -1*P.MPa,
                 color = ID)) +
  scale_x_continuous(expression(paste(H[2], O, " lost (g)"))) +
  scale_y_continuous(expression(paste(Psi[leaf], " (MPa)")),
                     limits = c(-7, -1),
                     minor_breaks = c(-7, -5, -3, -1),
                     guide = "axis_minor") +
  scale_color_brewer(type = "div") +
  theme_bw(base_size = 14) +
  # guides(color = "none") +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.position = c(0.1, 0.25),
        ggh4x.axis.ticks.length.minor = rel(1))

plot_grid(figb, figa, ncol = 2,
          align = "h",
          labels = "auto")
