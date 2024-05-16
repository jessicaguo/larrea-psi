# Try another ecosystem wilting point plot
# This time with SWP (2 flavors of pits 3&4, averaged across 2 or 3 depths)
library(tidyverse)
library(ggh4x)
library(cowplot)
library(segmented)

# Load daily met vars
vpd <- read_csv("data_clean/neon_atmdaily.csv")

# Load predawns
psy <- read_csv(file = "data_clean/psy/psy_daily_site_swp.csv") 

# Load in MM's flux data + join with other daily products
flux_daily <- read_csv("data_clean/xSR_flux_daily.csv") |>
  left_join(vpd, by = join_by(date)) |>
  left_join(psy, by = join_by(date, ppt_mm)) |>
  mutate(ppt_plot = ifelse(ppt_mm == 0, NA, ppt_mm),
         date = as.Date(date))
# Note: flux_daily now doubled in length
# once for for 6_16, another for 6_16_26

##### Recalculating for spring and monsoon drydowns with SWP #####
# Same periods as final PD-based EWP calcluation from 13-explore-EWP.R

# Have to deal with precip and also increases in WP
# Develop algorithm to pick out sustained declines and remove bumps?
# Will also remove ET bumps from cumulative ET

# Divide into 2 time periods
p2 <- data.frame(st = as.Date(c("2023-03-23", "2023-09-14")),
                 en = as.Date(c("2023-05-15", "2023-10-16")),
                 drydown = factor(c("spring", "monsoon"),
                                  levels = c("spring", "monsoon")))

# Plot ET and precip
flux_daily |>
  filter(!is.na(depths)) |>
  ggplot() +
  geom_rect(data = p2,
            aes(xmin = st, xmax = en,
                ymin = -Inf, ymax = Inf),
            alpha = 0.25) +
  geom_col(aes(x = date, y = ppt_plot/10),
           color = "darkblue") +
  geom_line(aes(x = date, y = et_mm_day),
            color = "coral") +
  geom_point(aes(x = date, y = PD/3),
             color = "seagreen") +
  geom_point(aes(x = date, y = swp/3),
             color = "tan") +
  scale_y_continuous(expression(paste("ET (mm ", d^-1, ") | ppt (cm ", d^-1, ")")),
                     breaks = seq(0, 3, 1),
                     sec.axis = sec_axis(~.*3,
                                         expression(paste(Psi[PD], " (MPa)")),
                                         breaks = seq(-8, 0, 2))) +
  scale_x_date(date_breaks = "2 months",
               date_labels = "%b %d") +
  facet_wrap(~depths, ncol = 1) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y.left = element_text(color = "coral"),
        axis.title.y.right = element_text(color = "seagreen"))

# First pivot wider to make two swp columns, one for each depth
# Join and add cummin for swp as criteria for summing ET
# _2 indicates with swp_6_16, _3 indicates with swp_6_16_26
et_test <- flux_daily |>
  pivot_wider(names_from = depths,
              names_prefix = "swp_",
              values_from = swp) |>
  left_join(p2, by = join_by(between(date, st, en))) |>
  dplyr::select(-st, -en) |>
  filter(!is.na(drydown)) |>
  group_by(drydown) |>
  # Replace NA's with 0 - okay for taking cummin
  mutate(swp_2_temp = replace_na(swp_6_16, 0),
         swp_3_temp = replace_na(swp_6_16_26, 0),
         swp_min_2 = cummin(swp_6_16),
         swp_min_3 = cummin(swp_3_temp)) |>
  ungroup() |>
  mutate(et_to_add_2 = if_else(swp_6_16 > swp_min_2, 0, et_mm_day),
         et_to_add_3 = if_else(swp_6_16_26 > swp_min_3, 0, et_mm_day)) |>
  group_by(drydown) |>
  mutate(et_cum_2 = cumsum(replace_na(et_to_add_2, 0)),
         et_cum_3 = cumsum(replace_na(et_to_add_3, 0)),
         days_since = difftime(date, min(date), units = "days") |>
           as.numeric())

# Plot timeseries with ET and PD

et_test |>
  ggplot() +
  geom_point(aes(x = date, y = et_cum_2)) +
  geom_col(aes(x = date, y = ppt_mm)) +
  geom_point(aes(x = date, y = swp_6_16*5 + 25)) +
  scale_y_continuous("cum ET",
                     sec.axis = sec_axis(~(.-25)/5)) +
  facet_wrap(~drydown, scale = "free_x")

et_test |>
  ggplot() +
  geom_point(aes(x = date, y = et_cum_3)) +
  geom_col(aes(x = date, y = ppt_mm)) +
  geom_point(aes(x = date, y = swp_6_16_26*5 + 25)) +
  scale_y_continuous("cum ET",
                     sec.axis = sec_axis(~(.-25)/5)) +
  facet_wrap(~drydown, scale = "free_x")


##### Developing EWP plots #### 

# First with swp_6_16
# Add breakpoints
# spring
et_spring_2 <- et_test |>
  filter(drydown == "spring") |>
  mutate(y = -1/swp_min_2)

m1 <- lm(y ~ et_cum_2, data = et_spring_2)
seg1 <- segmented(m1,
                  seg.Z = ~et_cum_2,
                  psi = list(et_cum_2 = c(5)))
summary(seg1)

seg1$psi

slope(seg1)
intercept(seg1)

# monsoon
et_monsoon_2 <- et_test |>
  filter(drydown == "monsoon") |>
  mutate(y = -1/swp_min_2)

m2 <- lm(y ~ et_cum_2, data = et_monsoon_2)
seg2 <- segmented(m2,
                  seg.Z = ~et_cum_2,
                  psi = list(et_cum_2 = c(6)))
summary(seg2)

seg2$psi
slope(seg2)


seg_df_2 <- data.frame(drydown = rep(c("spring", "monsoon"), each = 2),
                     slopes = c(slope(seg1)$et_cum_2[,1], slope(seg2)$et_cum_2[,1]),
                     ints = c(intercept(seg1)$et_cum_2[,1], intercept(seg2)$et_cum_2[,1]),
                     psi = rep(c(seg1$psi[2], seg2$psi[2]), each = 2)) |>
  mutate(hline = slopes*psi + ints,
         EWP = -1/hline,
         lab = paste0("EWP:~", round(EWP, 2)),
         lab2 = paste0("TLP:~-3.05"),
         drydown = factor(drydown, levels = c("spring", "monsoon")),
         season = drydown)

et_test |>
  ggplot() +
  geom_abline(data = seg_df_2,
              aes(slope = slopes, intercept = ints),
              linetype = "dotted") +
  geom_hline(data = seg_df_2,
             aes(yintercept = hline),
             lty = "dashed",
             col = "tan") +
  geom_hline(yintercept = 1/3.05,
             lty = "solid") +
  geom_point(aes(x = et_cum_2,  y = -1/swp_min_2),
             alpha = 0.2) +
  geom_text(data = seg_df_2,
            aes(x = 6, y = 0.95, label = lab), parse = TRUE,
            color = "tan",
            hjust = 0) +
  geom_text(data = seg_df_2,
            aes(x = 6, y = 0.9, label = lab2), parse = TRUE,
            hjust = 0) +
  scale_x_continuous(expression(paste("Cumulative ET (mm)"))) +
  scale_y_continuous(expression(paste("1/|", Psi[soil], "| (-MPa)")),
                     guide = "axis_minor") +
  facet_wrap(~drydown, scale = "free_x") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.75, 0.85),
        ggh4x.axis.ticks.length.minor = rel(1))



# First with swp_6_16_26
# Add breakpoints
# spring
et_spring_3 <- et_test |>
  filter(drydown == "spring") |>
  mutate(y = -1/swp_min_3) |>
  filter(!is.infinite(y))

m3 <- lm(y ~ et_cum_3, data = et_spring_3)
seg3 <- segmented(m3,
                  seg.Z = ~et_cum_3,
                  psi = list(et_cum_3 = c(6)))
summary(seg3)

seg3$psi

slope(seg3)
intercept(seg3)

# monsoon
et_monsoon_3 <- et_test |>
  filter(drydown == "monsoon") |>
  mutate(y = -1/swp_min_3)

m4 <- lm(y ~ et_cum_3, data = et_monsoon_3)
seg4 <- segmented(m4,
                  seg.Z = ~et_cum_3,
                  psi = list(et_cum_3 = c(6)))
summary(seg4)

seg4$psi
slope(seg4)


seg_df_3 <- data.frame(drydown = rep(c("spring", "monsoon"), each = 2),
                       slopes = c(slope(seg3)$et_cum_3[,1], slope(seg4)$et_cum_3[,1]),
                       ints = c(intercept(seg3)$et_cum_3[,1], intercept(seg4)$et_cum_3[,1]),
                       psi = rep(c(seg3$psi[2], seg4$psi[2]), each = 2)) |>
  mutate(hline = slopes*psi + ints,
         EWP = -1/hline,
         lab = paste0("EWP:~", round(EWP, 2)),
         lab2 = paste0("TLP:~-3.05"),
         drydown = factor(drydown, levels = c("spring", "monsoon")))

et_test |>
  ggplot() +
  geom_abline(data = seg_df_3,
              aes(slope = slopes, intercept = ints),
              linetype = "dotted") +
  geom_hline(data = seg_df_3,
             aes(yintercept = hline),
             lty = "dashed",
             col = "tan") +
  geom_hline(yintercept = 1/3.05,
             lty = "dashed") +
  geom_point(aes(x = et_cum_3,  y = -1/swp_min_3),
             alpha = 0.2) +
  geom_text(data = seg_df_3,
            aes(x = 6, y = 0.95, label = lab), parse = TRUE,
            color = "tan",
            hjust = 0) +
  geom_text(data = seg_df_3,
            aes(x = 6, y = 0.9, label = lab2), parse = TRUE,
            hjust = 0) +
  scale_x_continuous(expression(paste("Cumulative ET (mm)"))) +
  scale_y_continuous(expression(paste("1/|", Psi[soil], "| (-MPa)")),
                     guide = "axis_minor") +
  facet_wrap(~drydown, scale = "free_x") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.75, 0.85),
        ggh4x.axis.ticks.length.minor = rel(1))


#### Testing relationship with GPP ####
# plot GPP as timeseries
flux_daily |>
  filter(depths == "6_16") |>
  ggplot() +
  geom_rect(data = p2,
            aes(xmin = st, xmax = en,
                ymin = -Inf, ymax = Inf),
            alpha = 0.25) +
  geom_col(aes(x = date, y = ppt_plot/100),
           color = "darkblue") +
  geom_line(aes(x = date, y = GPP_DT),
            color = "coral") +
  geom_point(aes(x = date, y = PD/30),
             color = "seagreen") +
  geom_point(aes(x = date, y = swp/30),
             color = "tan") +
  scale_y_continuous(expression(paste("GPP (mol ", CO[2], " ", m^-2, " ", d^-1, ") | ppt (dm ", d^-1, ")")),
                     breaks = seq(0, 0.15, 0.05),
                     sec.axis = sec_axis(~.*30,
                                         expression(paste(Psi[PD], " | ", Psi[soil], " (MPa)")),
                                         breaks = seq(-8, 0, 2))) +
  scale_x_date(date_breaks = "2 months",
               date_labels = "%b %d") +
  facet_wrap(~depths, ncol = 1) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y.left = element_text(color = "coral"),
        axis.title.y.right = element_text(color = "seagreen"))


et_test |>
  ggplot() +
  geom_vline(xintercept = -3.73) +
  geom_vline(xintercept = -2.92) +
  geom_point(aes(x = swp_6_16, 
                 y = GPP_DT, 
                 color = drydown))

# GPP vs. soil WP
flux_daily |>
  filter(depths == "6_16") |>
  mutate(season = case_when(date < as.Date("2023-07-19", tz = "America/Phoenix") ~ "spring",
                            .default = "monsoon")|>
           factor(levels = c("spring", "monsoon"))) |>
  ggplot() +
  geom_vline(data = seg_df_2, 
             aes(xintercept = EWP,
                 lty = "EWP")) +
  geom_vline(xintercept = -3.05) +
  geom_point(aes(x = swp, 
                 y = GPP_DT,
                 color = season)) +
  scale_x_continuous(expression(paste(Psi[soil], " (MPa)"))) +
  scale_y_continuous(expression(paste("GPP (mol ", CO[2], " ", m^-2, " ", d^-1, ")"))) +
  scale_linetype_manual(values = "dashed") +
  # facet_wrap(~season, ncol = 2) +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank()) +
  guides(color = "none", 
         lty = "none")


# GPP vs. predawn WP
seg_df <- data.frame(season = c("spring", "monsoon"),
                     EWP = c(-3.2, -3.56)) |>
  mutate(season = factor(season, levels = c("spring", "monsoon")))

flux_daily |>
  filter(depths == "6_16") |>
  mutate(season = case_when(date < as.Date("2023-07-19", tz = "America/Phoenix") ~ "spring",
                            .default = "monsoon")|>
           factor(levels = c("spring", "monsoon"))) |>
  ggplot() +
  geom_vline(data = seg_df,
             aes(xintercept = EWP,
                 lty = "EWP")) +
  geom_vline(xintercept = -3.05) +
  geom_point(aes(x = PD, 
                 y = GPP_DT,
                 color = season)) +
  scale_x_continuous(expression(paste(Psi[PD], " (MPa)"))) +
  scale_y_continuous(expression(paste("GPP (mol ", CO[2], " ", m^-2, " ", d^-1, ")"))) +
  scale_linetype_manual(values = "dashed") +
  theme_bw(base_size = 12) +
  theme(panel.grid = element_blank()) +
  guides(color = "none", 
         lty = "none")

