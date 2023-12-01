# Read in VOD data for Santa Rita
# Compare to select shrubs, or site-averaged 1:30 am

library(readxl)
library(tidyverse)
library(janitor)
library(cowplot)
library(slider)

# Read manual data and summarize
manual <- read_csv("data_clean/pressure_chamber.csv") %>%
  mutate(month = lubridate::month(dt))

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

#### LPDR 25 km ####
# Read in VOD data from Jinyang Du 6/7/2023
vod_ndvi <- read_xlsx("data_vod/VOD_NDVI.xlsx") %>%
  clean_names() %>%
  rename(vod130am = vod1_30am,
         vod130pm = vod1_30pm) %>%
  left_join(manual_sum, by = join_by(date)) %>%
  mutate(date = lubridate::force_tz(date, tzone = "America/Phoenix"))

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
  

plot_grid(fig_top, fig_bot, ncol = 1)


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
branch_6a <- read_csv("data_appended/psy_6a.csv",
                      locale=locale(encoding="latin1", tz = "America/Phoenix")) %>%
  rename(WP = corrected_water_potential_m_pa) %>%
  mutate(month = lubridate::month(dt))
branch_6c <- read_csv("data_appended/psy_6c.csv",
                      locale=locale(encoding="latin1", tz = "America/Phoenix")) %>%
  rename(WP = corrected_water_potential_m_pa) %>%
  mutate(month = lubridate::month(dt))
branch_4a <- read_csv("data_appended/psy_4a.csv",
                      locale=locale(encoding="latin1", tz = "America/Phoenix")) %>%
  rename(WP = corrected_water_potential_m_pa)
branch_4b <- read_csv("data_appended/psy_4b.csv",
                      locale=locale(encoding="latin1", tz = "America/Phoenix")) %>%
  rename(WP = corrected_water_potential_m_pa)

psy_top <- ggplot() +
  geom_point(data = branch_6c, aes(x = date, y = WP, color = "6c",
                                   shape = "psychrometer"),
             alpha = 0.5) +
  geom_point(data = branch_6a, aes(x = date, y = WP, color = "6a",
                                   shape = "psychrometer"),
             alpha = 0.5) +
  geom_point(data = filter(manual, shrub_id == 6), 
             aes(x = date, y = predawn_m_pa, 
                 color = "6",
                 shape = "manual"),
             size = 2) +
  scale_y_continuous(expression(paste(Psi, " (MPa)"))) +
  scale_color_brewer(palette = "PuBuGn", direction = -1) +
  labs(color = "Shrub/branch",
       shape = "Method") +
  theme_bw()

psy_mid <- ggplot() +
  geom_point(data = branch_4a, aes(x = dt, y = WP, color = "4a",
                                   shape = "psychrometer"),
             alpha = 0.5) +
  geom_point(data = branch_4b, aes(x = dt, y = WP, color = "4b",
                                   shape = "psychrometer"),
             alpha = 0.5) +
  geom_point(data = filter(manual, shrub_id == 4),
             aes(x = date, y = predawn_m_pa,
                 color = "4",
                 shape = "manual"),
             size = 2) +
  scale_y_continuous(expression(paste(Psi, " (MPa)"))) +
  scale_color_brewer(palette = "PuBuGn", direction = -1) +
  labs(color = "Shrub/branch",
       shape = "Method") +
  theme_bw()


plot_grid(psy_top, psy_mid, ncol = 1)

# July only
psy_july <- ggplot() +
  geom_point(data = filter(branch_6c,
                           date >= as.Date("2023-07-17"),
                           date <= as.Date ("2023-08-15")), 
             aes(x = dt, y = WP, 
                 color = "6c",
                 shape = "psychrometer")) +
  geom_point(data = filter(manual, shrub_id == 6, 
                           date >= as.Date("2023-07-17"),
                           date <= as.Date ("2023-08-15")), 
             aes(x = dt, y = predawn_m_pa, 
                 color = "6",
                 shape = "manual"),
             size = 2) +
  scale_y_continuous(expression(paste(Psi, " (MPa)"))) +
  scale_color_brewer(palette = "Paired", direction = -1) +
  guides(color = "none",
         shape = "none") +
  theme_bw()

psy_july

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
    filter(time == hms("01:30:00")) %>%
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
            WP_n_shrub = sum(!is.na(WP_mean)))

appended_sum %>%
  ggplot(aes(x = date)) +
  geom_pointrange(aes(y = WP_m,
                      ymin = WP_m - WP_sd,
                      ymax = WP_m + WP_sd),
                  size = 0.5)

# Match with 1:30 am values of VOD

# branch_4a_am <- branch_4a %>%
#   filter(time == hms("01:30:00")) %>%
#   mutate(date = as.Date(date))
# 
# branch_6a_am <- branch_6a %>%
#   filter(time == hms("01:30:00")) %>%
#   mutate(date = as.Date(date))

# vod_psy <- branch_4a_am %>%
#   select(date, WP) %>%
#   rename(WP_4a_1am = WP) %>%
#   left_join(branch_6a_am[, c(1, 6)], by = "date") %>%
#   rename(WP_6a_1am = WP) %>%
#   left_join(vod_ndvi, by = "date")

vod_psy <- appended_sum %>%
  left_join(vod_ndvi, by = "date")


# Plot AM VOD with psy
vod_psy %>%
  ggplot(aes(x = date)) +
  geom_point(aes(y = vod130am, color = "VOD_LPDR")) +
  # geom_point(aes(y = WP_4a_1am*.09+1.1, color = "4a",
  #                shape = "psychrometer")) +
  # geom_point(aes(y = WP_6a_1am*.09+1.1, color = "6a",
  #                shape = "psychrometer")) +
  geom_errorbar(aes(ymin = (WP_m - WP_sd)*.09+1.1,
                    ymax = (WP_m + WP_sd)*.09+1.1,
                    color = "psychrometer"),
                width = 0,
                alpha = 0.25) +
  geom_point(aes(y = WP_m*.09+1.1,
                 color = "psychrometer")) +
  geom_errorbar(data = manual_sum, aes(x = date,
                                       ymin = (predawn_mean - predawn_sd)*.09+1.1,
                                       ymax = (predawn_mean + predawn_sd)*.09+1.1,
                                       color = "chamber"),
                width = 0) +
  geom_point(data = manual_sum, aes(x = date, 
                                    y = predawn_mean*.09+1.1,
                                    color = "chamber"),
             # color = "gray30",
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
  geom_point(aes(y = am_vod_ndvi, color = "VOD/NDVI")) +
  geom_errorbar(aes(ymin = (WP_m - WP_sd)+13,
                    ymax = (WP_m + WP_sd)+13,
                    color = "psychrometer"),
                width = 0,
                alpha = 0.25) +
  geom_point(aes(y = WP_m+13, color = "psychrometer")) +
  geom_errorbar(data = manual_sum, aes(x = date,
                                       ymin = (predawn_mean - predawn_sd)+13,
                                       ymax = (predawn_mean + predawn_sd)+13,
                                       color = "chamber"),
                width = 0) +
  geom_point(data = manual_sum, aes(x = date, 
                                    y = predawn_mean+13,
                                    color = "chamber"),
             size = 2) +
  scale_y_continuous(expression(paste(frac(VOD["1:30 am"], "NDVI"))), 
                     sec.axis = sec_axis(~.-13,
                                         name = expression(paste(Psi["1:30 am"], " (MPa)")))) +
  labs(color = "Data stream") +
  theme_bw() +
  theme(legend.position = "bottom")

ggsave("figs/VOD_NDVI_psy.png",
       height = 4,
       width = 8,
       units = "in")


#### LPRM downscaled 10 km ####
# From Charlie 7/19/23
# Ascending 1:30 pm and descending 1:30 am for 3 bands
# C1, C2, and X
# X may be cleanest and most sensitive to changes in veg canopy WC
# shorter wavelength than C-band

lprm <- read_csv("data_vod/xSR_LPRM_VOD_Daily.csv",
                 locale = locale(tz = "America/Phoenix")) %>%
  pivot_wider(names_from = Dataset, values_from = Value) %>%
  filter(Year == 2023) %>%
  rename(date = Dates) %>%
  arrange(date)

# Compute a sliding window for descending x band


# slide(lprm$LPRM_X_VOD_DESCENDING_NIGHT, ~.x,
#           .before = 3, .after = 3,
#           .complete = TRUE)

lprm2 <- lprm %>%
  select(date, LPRM_X_VOD_DESCENDING_NIGHT) %>%
  mutate(X_VOD_DESCENDING_smooth_11 = slide_vec(LPRM_X_VOD_DESCENDING_NIGHT, ~mean(.x, na.rm = TRUE), 
                                                .before = 5, .after = 5,
                                                .complete = TRUE),
         X_VOD_DESCENDING_smooth_15 = slide_vec(LPRM_X_VOD_DESCENDING_NIGHT, ~mean(.x, na.rm = TRUE), 
                                                .before = 7, .after = 7,
                                                .complete = TRUE),
         X_VOD_DESCENDING_smooth_31 = slide_vec(LPRM_X_VOD_DESCENDING_NIGHT, ~mean(.x, na.rm = TRUE), 
                                                .before = 15, .after = 15,
                                                .complete = TRUE))
lprm2 %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = X_VOD_DESCENDING_smooth_11,
                 color = "11 day window")) +
  geom_line(aes(y = X_VOD_DESCENDING_smooth_15,
                 color = "15 day window")) +
  geom_line(aes(y = X_VOD_DESCENDING_smooth_31,
                 color = "31 day window")) +
  scale_y_continuous("X LPRM descending 1:30 AM") +
  theme_bw()

# seems like 15 day window captures the peak 

# let's compare to 30 day median of LPDR

vod_all <- vod_psy %>%
  left_join(lprm2, by = join_by("date"))

vod_all %>%
  ggplot(aes(x = date)) +
  geom_point(aes(y = vod130am, color = "LPDR 30 day median")) +
  geom_point(aes(y = am_vod_ndvi/10, color = "LPDR 30 day median / NDVI / 10")) +
  geom_point(aes(y = X_VOD_DESCENDING_smooth_31*2,
                 color = "LPRM X band 31 day mean *2")) +
  geom_point(aes(y = X_VOD_DESCENDING_smooth_15*2,
                 color = "LPRM X band 15 day mean *2")) +
  geom_point(aes(y = X_VOD_DESCENDING_smooth_11*2,
                 color = "LPRM X band 11 day mean *2")) +
  theme_bw(base_size = 14) +
  scale_y_continuous("VOD indices") +
  theme(legend.position = c(0.75 , 0.65),
        panel.grid = element_blank())

# Compare LPDR/NDVI with 11 day mean LPRM X band

vod_all %>%
  ggplot(aes(x = date)) +
  geom_point(aes(y = X_VOD_DESCENDING_smooth_11*25,
                 color = "LPRM X band 11 day mean x 25")) +
  geom_point(aes(y = am_vod_ndvi, 
                 color = "LPDR 30 day median / NDVI")) +
  geom_errorbar(aes(ymin = (WP_m - WP_sd)+13,
                    ymax = (WP_m + WP_sd)+13,
                    color = "1:30 AM psychrometer mean +/- sd"),
                width = 0,
                alpha = 0.25,
                color = "black") +
  geom_point(aes(y = WP_m + 13, color = "1:30 AM psychrometer mean +/- sd"),
             color = "black") +
  geom_errorbar(data = manual_sum, aes(x = date,
                                       ymin = (predawn_mean - predawn_sd)+13,
                                       ymax = (predawn_mean + predawn_sd)+13,
                                       color = "Predawn chamber mean +/- sd"),
                width = 0,
                color = "forestgreen") +
  geom_point(data = manual_sum, aes(x = date, 
                                    y = predawn_mean+13,
                                    color = "Predawn chamber mean +/- sd"),
             size = 2,
             color = "forestgreen") +
  scale_y_continuous("VOD", 
                     sec.axis = sec_axis(~.-13,
                                         name = expression(paste(Psi["1:30 am"], " (MPa)")))) +
  labs(shape = "method", color = "product") +
  theme_bw(base_size = 12) +
  theme(legend.position = "bottom")

# Quick calculation of R2

sub_df <- vod_all %>%
  select(date, WP_m, predawn_mean, X_VOD_DESCENDING_smooth_11, am_vod_ndvi, vod130am)

cor(sub_df$WP_m, sub_df$predawn_mean, use = "complete.obs") # 0.96

cor(sub_df$WP_m, sub_df$X_VOD_DESCENDING_smooth_11, use = "complete.obs") # 0.854

cor(sub_df$WP_m, sub_df$am_vod_ndvi, use = "complete.obs") # 0.515
cor(sub_df$WP_m, sub_df$vod130am, use = "complete.obs") # 0.327

sub_long <- sub_df %>%
  select(-predawn_mean, -vod130am) %>%
  pivot_longer(3:4, 
               names_to = "index",
               values_to = "VOD") %>%
  filter(!is.na(VOD)) %>%
  mutate(lab = case_when(index == "am_vod_ndvi" ~ "LPDR VOD/NDVI",
                         index == "X_VOD_DESCENDING_smooth_11" ~ "LPRM X band 11 day"),
         date = as.Date(date, tz = "America/Phoenix"))

sub_long %>%
  ggplot(aes(x = VOD,
             y = WP_m))+
  geom_point(aes(color = date)) +
  facet_wrap(~lab, scales = "free_x") +
  viridis::scale_colour_viridis(option = "mako", 
                                trans = "date",
                                direction = -1) +
  theme_bw(base_size = 12) +
  scale_y_continuous(expression(paste(Psi["1:30 am"], " (MPa)")))

#### Read in manual pv and compare to LPRM

sub_lprm <- sub_long %>%
  filter(index == "X_VOD_DESCENDING_smooth_11")


pv <- read_csv("data_clean/pv.csv") %>%
  select(date, WP, water_content) %>%
  rename(WP_m = WP,
         VOD = water_content) %>%
  mutate(index = "PV curve",
         lab = "Water content (manual)",
         date = as.Date(date, tz = "America/Phoenix"))

sub_pv <- bind_rows(sub_lprm, pv)

sub_pv %>%
  ggplot(aes(y = WP_m, 
             x = VOD))+
  geom_point(aes(color = date)) +
  facet_wrap(~lab, scales = "free_x") +
  viridis::scale_colour_viridis(option = "mako", 
                                trans = "date",
                                direction = -1) +
  theme_bw(base_size = 12) +
  scale_y_continuous(expression(paste(Psi["1:30 am"], " (MPa)")))


# from late October 2023, full rehydrated curves on 5 samples
# need to rerun with dried mass of leaves
pv_comb <- read_csv("../larrea-pv/data_clean/pv_comb.csv") %>%
  select(P.MPa, rwc) %>%
  rename(VOD = rwc) %>%
  mutate(WP_m = -P.MPa,
         index = "PV curve",
         lab = "Mass (g)")

sub_pv2 <- bind_rows(sub_pv, pv_comb)

sub_pv2 %>%
  ggplot(aes(y = -1/WP_m, 
             x = 1-VOD))+
  geom_point() +
  facet_wrap(~lab, scales = "free_x") +
  theme_bw(base_size = 12) +
  scale_y_continuous(expression(paste(Psi["1:30 am"], " (MPa)")))


#### Add on-site SWC to comparison ####
# read in half-hourly
swc30 <- read_csv("data_clean/neon_swc30.csv",
                  locale = locale(tz = "America/Phoenix")) %>%
  rename(date = startDateTime)

# Just plot SWC, grouped and cleaned to half-hourly

swc30 %>%
  ggplot(aes(x = date)) +
  geom_point(aes(y = p12_6, color = "plots 1 & 2, 06 cm")) +
  geom_point(aes(y = p12_16, color = "plots 1 & 2, 16 cm")) +
  geom_point(aes(y = p12_26, color = "plots 1 & 2, 26 cm")) +
  geom_point(aes(y = p34_6, color = "plots 3 & 4, 06 cm")) +
  geom_point(aes(y = p34_16, color = "plots 3 & 4, 16 cm")) +
  geom_point(aes(y = p34_26, color = "plots 3 & 4, 26 cm")) +
  scale_color_brewer(palette = "GnBu") +
  scale_y_continuous(expression(paste(Theta))) +
  theme_bw() +
  labs(color = "Plot & Depth")

# read in daily
swcdaily <- read_csv("data_clean/neon_swcdaily.csv",
                  locale = locale(tz = "America/Phoenix"))

swcdaily %>%
  ggplot(aes(x = date)) +
  geom_pointrange(aes(y = m_p12_6, 
                      ymin = m_p12_6 - sd_p12_6,
                      ymax = m_p12_6 + sd_p12_6,
                      color = "plots 1 & 2, 06 cm")) +
  geom_pointrange(aes(y = m_p12_16, 
                      ymin = m_p12_16 - sd_p12_16,
                      ymax = m_p12_16 + sd_p12_16,
                      color = "plots 1 & 2, 16 cm")) +
  geom_pointrange(aes(y = m_p12_26, 
                      ymin = m_p12_26 - sd_p12_26,
                      ymax = m_p12_26 + sd_p12_26,
                      color = "plots 1 & 2, 26 cm")) +
  geom_pointrange(aes(y = m_p34_6, 
                      ymin = m_p34_6 - sd_p34_6,
                      ymax = m_p34_6 + sd_p34_6,
                      color = "plots 3 & 4, 06 cm")) +
  geom_pointrange(aes(y = m_p34_16, 
                      ymin = m_p34_16 - sd_p34_16,
                      ymax = m_p34_16 + sd_p34_16,
                      color = "plots 3 & 4, 16 cm")) +
  geom_pointrange(aes(y = m_p34_26, 
                      ymin = m_p34_26 - sd_p34_26,
                      ymax = m_p34_26 + sd_p34_26,
                      color = "plots 3 & 4, 26 cm")) +
  scale_color_brewer(palette = "GnBu") +
  scale_y_continuous(expression(paste(Theta))) +
  theme_bw() +
  labs(color = "Plot & Depth")

# join together
vod_swc <- vod_all %>%
  left_join(swcdaily, by = join_by(date))

# select one VOD, 
vod_swc %>%
  ggplot(aes(x = date)) +
  geom_point(aes(y = X_VOD_DESCENDING_smooth_11*25,
                 color = "LPRM X band 11 day mean x 25",
                 shape = "VOD")) +
  geom_point(aes(y = am_vod_ndvi, 
                 color = "LPDR 30 day median / NDVI",
                 shape = "VOD")) +
  geom_pointrange(aes(y = m_p34_6 * 100, 
                      ymin = (m_p34_6 - sd_p34_6)* 100,
                      ymax = (m_p34_6 + sd_p34_6)* 100,
                      color = "plots 3 & 4, 06 cm",
                      shape = "VWC")) +
  geom_pointrange(aes(y = m_p34_16 * 100, 
                      ymin = (m_p34_16 - sd_p34_16)* 100,
                      ymax = (m_p34_16 + sd_p34_16)* 100,
                      color = "plots 3 & 4, 16 cm",
                      shape = "VWC")) +
  scale_y_continuous("VOD", 
                     sec.axis = sec_axis(~./100,
                                         name = expression(paste(Theta, " (%)")))) +
  labs(shape = "method", color = "branch") +
  theme_bw() 



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
