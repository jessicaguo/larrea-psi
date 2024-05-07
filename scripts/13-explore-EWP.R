# Attempt an ecosystem wilting point plot
library(tidyverse)
library(ggh4x)
library(cowplot)
library(segmented)

# Load daily met vars
vpd <- read_csv("data_clean/neon_atmdaily.csv")

# Load predawns
psy <- read_csv("data_clean/psy/psy_daily_site.csv")

# Load in MM's flux data + join with other daily products
flux_daily <- read_csv("data_clean/xSR_flux_daily.csv") |>
  left_join(vpd, by = join_by(date)) |>
  left_join(psy, by = join_by(date)) |>
  mutate(ppt_plot = ifelse(ppt_mm == 0, NA, ppt_mm),
         date = as.Date(date))

# Mark start and end dates of dry periods
periods <- data.frame(st = as.Date(c("2023-03-17", "2023-05-19", "2023-09-14")),
                      en = as.Date(c("2023-05-13", "2023-07-10", "2023-10-16")),
                      drydown = factor(c("spring", "premonsoon", "fall"),
                                       levels = c("spring", "premonsoon", "fall")))

str(periods)
# Plot ET and precip
flux_daily |>
  ggplot() +
  geom_rect(data = periods,
            aes(xmin = st, xmax = en,
                ymin = -Inf, ymax = Inf),
            alpha = 0.25) +
  geom_col(aes(x = date, y = ppt_plot/10),
           color = "darkblue") +
  geom_line(aes(x = date, y = et_mm_day),
            color = "coral") +
  geom_point(aes(x = date, y = PD/3),
             color = "forestgreen") +
  scale_y_continuous(expression(paste("ET (mm ", d^-1, ") | ppt (cm ", d^-1, ")")),
                     breaks = seq(0, 3, 1),
                     sec.axis = sec_axis(~.*3,
                                         expression(paste(Psi[PD], " (MPa)")),
                                         breaks = seq(-8, 0, 2))) +
  scale_x_date(date_breaks = "month",
               date_labels = "%b %d") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank())

# Join periods with data
et_drydown <- flux_daily |>
  left_join(periods, by = join_by(between(date, st, en))) |>
  dplyr::select(-st, -en) |>
  filter(!is.na(drydown)) |>
  group_by(drydown) |>
  mutate(et_cum = cumsum(et_mm_day))

# Plot
et_drydown |>
  ggplot() +
  geom_point(aes(x = et_cum,  y = -1/PD)) +
  facet_wrap(~drydown) +
  theme_bw(base_size = 14) +
  scale_x_continuous(expression(paste("Cumulative ET (mm)"))) +
  scale_y_continuous(expression(paste("1/|", Psi[PD], "| (-MPa)")),
                     guide = "axis_minor") +
  scale_color_brewer(palette = "Accent") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.75, 0.85),
        ggh4x.axis.ticks.length.minor = rel(1))


# Calculate spring + premonsoon, exclude days of rainfall
et_drydown2 <- flux_daily |>
  left_join(periods, by = join_by(between(date, st, en))) |>
  dplyr::select(-st, -en) |>
  filter(drydown %in% c("spring", "premonsoon")) |>
  mutate(et_cum = cumsum(et_mm_day))

# Plot
figa <- et_drydown2 |>
  ggplot() +
  geom_hline(yintercept = 1/3.05,
             lty = "dashed") +
  geom_point(aes(x = et_cum,  y = -1/PD, color = drydown)) +
  scale_x_continuous(expression(paste("Cumulative ET (mm)"))) +
  scale_y_continuous(expression(paste("1/|", Psi[PD], "| (-MPa)")),
                     guide = "axis_minor") +
  scale_color_brewer(palette = "Accent") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.75, 0.85),
        ggh4x.axis.ticks.length.minor = rel(1))


##### Recalculating for spring and monsoon drydowns #####

# Have to deal with precip and also increases in WP
# Develop algorithm to pick out sustained declines and remove bumps?
# Will also remove ET bumps from cumulative ET

# Divide into 2 time periods
p2 <- data.frame(st = as.Date(c("2023-03-23", "2023-08-26")),
                 en = as.Date(c("2023-05-15", "2023-10-16")),
                 drydown = factor(c("spring", "monsoon"),
                                  levels = c("spring", "monsoon")))

# Plot ET and precip
flux_daily |>
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
             color = "forestgreen") +
  scale_y_continuous(expression(paste("ET (mm ", d^-1, ") | ppt (cm ", d^-1, ")")),
                     breaks = seq(0, 3, 1),
                     sec.axis = sec_axis(~.*3,
                                         expression(paste(Psi[PD], " (MPa)")),
                                         breaks = seq(-8, 0, 2))) +
  scale_x_date(date_breaks = "2 months",
               date_labels = "%b %d") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y.left = element_text(color = "coral"),
        axis.title.y.right = element_text(color = "forestgreen"))

# Join and add cummin for PD as criteria for summing ET
et_test <- flux_daily |>
  left_join(p2, by = join_by(between(date, st, en))) |>
  dplyr::select(-st, -en) |>
  filter(!is.na(drydown)) |>
  group_by(drydown) |>
  mutate(PD_min = cummin(PD)) |>
  ungroup() |>
  mutate(et_to_add = if_else(PD > PD_min, 0, et_mm_day)) |>
  group_by(drydown) |>
  mutate(et_cum = cumsum(et_to_add),
         days_since = difftime(date, min(date), units = "days") |>
           as.numeric())

#  Plot timeseries with ET and PD

et_test |>
  ggplot() +
  geom_point(aes(x = date, y = et_cum)) +
  geom_col(aes(x = date, y = ppt_mm)) +
  geom_point(aes(x = date, y = PD*5 + 25)) +
  scale_y_continuous("cum ET",
                     sec.axis = sec_axis(~(.-25)/5)) +
  facet_wrap(~drydown, scale = "free_x")


# Try breakpoint model

# spring
et_spring <- et_test |>
  filter(drydown == "spring") |>
  mutate(y = -1/PD_min)

m1 <- lm(y ~ et_cum, data = et_spring)
seg1 <- segmented(m1,
                  seg.Z = ~et_cum,
                  psi = list(et_cum = c(8)))
summary(seg1)

seg1$psi

slope(seg1)
intercept(seg1)

pred <- data.frame(et_cum = et_spring$et_cum, y = fitted(seg1))
plot(pred$et_cum, pred$y)

# monsoon
et_monsoon <- et_test |>
  filter(drydown == "monsoon") |>
  mutate(y = -1/PD_min)

m2 <- lm(y ~ et_cum, data = et_monsoon)
seg2 <- segmented(m2,
                  seg.Z = ~et_cum,
                  psi = list(et_cum = c(12)))
summary(seg2)

seg2$psi
slope(seg2)

pred2 <- data.frame(et_cum = et_monsoon$et_cum, y = fitted(seg2))
plot(pred2$et_cum, pred2$y)

seg_df <- data.frame(drydown = rep(c("spring", "monsoon"), each = 2),
                     slopes = c(slope(seg1)$et_cum[,1], slope(seg2)$et_cum[,1]),
                     ints = c(intercept(seg1)$et_cum[,1], intercept(seg2)$et_cum[,1]),
                     psi = rep(c(seg1$psi[2], seg2$psi[2]), each = 2)) |>
  mutate(hline = slopes*psi + ints,
         EWP = -1/hline,
         lab = paste0("EWP:~", round(EWP, 2)),
         lab2 = paste0("TLP:~-3.05"),
         drydown = factor(drydown, levels = c("spring", "monsoon")))

# Plot as PV curve with PD_min and et_cum
# Add manually-derived TLP
# And segmented regression
et_test |>
  ggplot() +
  geom_abline(data = seg_df,
              aes(slope = slopes, intercept = ints),
              linetype = "dotted") +
  geom_hline(data = seg_df,
             aes(yintercept = hline),
             lty = "dashed",
             col = "forestgreen") +
  geom_hline(yintercept = 1/3.05,
             lty = "dashed") +
  geom_point(aes(x = et_cum,  y = -1/PD_min),
             alpha = 0.2) +
  geom_text(data = seg_df,
            aes(x = 10, y = 0.6, label = lab), parse = TRUE,
            color = "forestgreen",
            hjust = 0) +
  geom_text(data = seg_df,
            aes(x = 10, y = 0.55, label = lab2), parse = TRUE,
            hjust = 0) +
  scale_x_continuous(expression(paste("Cumulative ET (mm)"))) +
  scale_y_continuous(expression(paste("1/|", Psi[PD], "| (-MPa)")),
                     guide = "axis_minor") +
  # scale_color_brewer(palette = "Accent") +
  facet_wrap(~drydown, scale = "free_x") +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.75, 0.85),
        ggh4x.axis.ticks.length.minor = rel(1))

#### Testing EWP as as determinant of GPP - WP relationship ####
labels <- pretty(flux_daily$date, 5)

# Daytime
flux_daily |>
  ggplot() +
  geom_point(aes(x = date, y = GPP_DT))

flux_daily |>
  mutate(month = lubridate::month(date)) |>
  ggplot() +
  geom_vline(xintercept = -3.2, lty = "dashed") +
  geom_vline(xintercept = -3.56, lty = "dashed") +
  geom_vline(xintercept = -3.05, lty = "dashed") +
  # geom_point(aes(x = PD, y = GPP, col = "R")) +
  geom_point(aes(x = PD, y = GPP_DT, col = date)) +
  scale_x_continuous(expression(paste(Psi[PD], " (MPa)"))) +
  scale_y_continuous(expression(paste("GPP - DT (mol ", CO[2], " ", m^-2, " ", d^-1, ")"))) +
  scale_color_gradient(low = "forestgreen", high = "coral",
                       breaks = as.integer(labels),
                       labels = format(labels, "%b %d")) +
  facet_wrap(~month) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank())

# Nighttime

flux_daily |>
  ggplot() +
  geom_point(aes(x = date, y = GPP))

flux_daily |>
  mutate(month = lubridate::month(date)) |>
  ggplot() +
  geom_vline(xintercept = -3.2, lty = "dashed") +
  geom_vline(xintercept = -3.56, lty = "dashed") +
  geom_vline(xintercept = -3.05, lty = "dashed") +
  # geom_point(aes(x = PD, y = GPP, col = "R")) +
  geom_point(aes(x = PD, y = GPP, col = date)) +
  scale_x_continuous(expression(paste(Psi[PD], " (MPa)"))) +
  scale_y_continuous(expression(paste("GPP (mol ", CO[2], " ", m^-2, " ", d^-1, ")"))) +
  scale_color_gradient(low = "forestgreen", high = "coral",
                       breaks = as.integer(labels),
                       labels = format(labels, "%b %d")) +
  facet_wrap(~month) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank())

flux_daily |>
  mutate(mon = lubridate::month(date)) |>
  ggplot() +
  geom_vline(xintercept = -3.2, lty = "dashed") +
  geom_vline(xintercept = -3.56, lty = "dashed") +
  geom_vline(xintercept = -3.05, lty = "dashed") +
  # geom_point(aes(x = PD, y = GPP, col = "R")) +
  geom_point(aes(x = PD, y = GPP_DT, col = date)) +
  scale_x_continuous(expression(paste(Psi[PD], " (MPa)"))) +
  scale_y_continuous(expression(paste("GPP - DT (mol ", CO[2], " ", m^-2, " ", d^-1, ")"))) +
  scale_color_gradient(low = "forestgreen", high = "coral",
                       breaks = as.integer(labels),
                       labels = format(labels, "%b %d")) +
  facet_wrap(~mon) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank())
  


##### Add PV curves ####

pv <- read_csv("data_clean/pmass_comb_20240308.csv")

figb <- pv |>
  filter(keep == TRUE,
         P.MPa < 6) |>
  mutate(ID = factor(ID)) |>
  ggplot() +
  # add mean TLP from PV analysis
  geom_rect(aes(ymin = 1/3.31, ymax = 1/2.82,
                xmin = -Inf, xmax = Inf),
            fill = "gray",
            alpha = 0.25) +
  geom_hline(yintercept = 1/3.05) +
  geom_point(aes(x = mass_lost,
                 y = 1/P.MPa,
                 color = ID)) +
  geom_line(aes(x = mass_lost,
                y = 1/P.MPa,
                color = ID)) +
  scale_x_continuous(expression(paste(H[2], O, " lost (g)"))) +
  scale_y_continuous(expression(paste("1/|", Psi[leaf], "| (-MPa)")),
                     guide = "axis_minor") +
  scale_color_brewer(type = "div") +
  theme_bw(base_size = 14) +
  guides(color = "none") +
  theme(panel.grid = element_blank(),
        ggh4x.axis.ticks.length.minor = rel(1))

plot_grid(figb, figa, labels = "auto",
          ncol = 2,
          align = "h")
