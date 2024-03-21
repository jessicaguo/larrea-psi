# Attempt an ecosystem wilting point plot
library(tidyverse)
library(ggh4x)
library(cowplot)

# Load daily met vars
vpd <- read_csv("data_clean/neon_atmdaily.csv")

# Load predawns
psy <- read_csv("data_clean/psy/psy_daily_site.csv")

# Load in ET data + join with other daily products
et_daily <- read_csv("data_clean/xSR_et_daily.csv") |>
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
et_daily |>
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
et_drydown <- et_daily |>
  left_join(periods, by = join_by(between(date, st, en))) |>
  select(-st, -en) |>
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
et_drydown2 <- et_daily |>
  left_join(periods, by = join_by(between(date, st, en))) |>
  select(-st, -en) |>
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
