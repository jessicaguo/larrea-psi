#### Use vwc2swp.R on real VWC data
# Plot with site-level predawns
# appears that depth 16cm is the closest match
# although PD is generally much more negative than SWP_16

library(tidyverse)
source("source/vwc2swp.R")

# Read in moisture release data
mrc <- read_csv("models/mod1 - Gardner/moisture-release.csv")
# Read in vwc field data
vwc <- read_csv("data_clean/neon_swcdaily.csv")
# Read in psy_daily_site
psy_site <- read_csv("data_clean/psy/psy_daily_site.csv")

# Obtain texture by depth
mrc |>
  group_by(horizonTopDepth, horizonBottomDepth) |>
  summarize(texture = unique(texture))


#### Plot first for soil pits 1&2 average ####
# Quick plot 1&2 averaged
vwc |> 
  select(date, starts_with("m_p12")) |>
  ggplot() +
  geom_point(aes(x = date, y = m_p12_6, color = "12 - 06")) +
  geom_point(aes(x = date, y = m_p12_16, color = "12 - 16")) +
  geom_point(aes(x = date, y = m_p12_26, color = "12 - 26")) +
  geom_point(aes(x = date, y = m_p12_56, color = "12 - 56"))

# Add predicted swp by texture
# sandy loam for 6 and 16 and 26 and 56

swp_vwc_12 <- vwc |>
  select(date, starts_with("m_p12")) |>
  pivot_longer(-date, 
               names_to = c("plot", "depth"),
               names_pattern = "m_p(.*)_(.*)",
               values_to = "vwc") |>
  filter(!is.na(vwc)) |> # remove NA's for function to work
  mutate(depth = as.numeric(depth),
         texture = case_when(depth == 6 ~ "sandy loam",
                             depth == 16 ~ "sandy loam",
                             depth == 26 ~ "sandy loam",
                             depth == 56 ~ "sandy loam")) |>
  group_by(date, plot, depth, texture) |>
  mutate(vwc = unique(vwc),
         swp = vwc2swp(vwc, param = texture, stat = "median"))

swp_vwc_12 |>
  filter(depth != 56) |>
  ggplot(aes(x = date)) +
  geom_point(aes(y = swp, 
                 col = "SWP")) +
  geom_point(aes(y = vwc*10-2,
                 col = "VWC")) +
  scale_y_continuous(expression(paste(Psi[soil], " (MPa)")),
                     sec.axis = sec_axis(~(.+2)/10,
                                         expression(paste(Theta, " (", cm^3, " ", cm^-3, ")")))) +
  facet_wrap(~depth, ncol = 1) +
  scale_color_manual(values = c("seagreen", "tan")) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.title.y.left = element_text(color = "seagreen"),
        axis.title.y.right = element_text(color = "tan")) +
  guides(color = "none")

# These soil WP values are too high, will go with the average
# from plots 3&4

#### Then plot for soil pits 3&4 average ####
# Quick plot 3&4 averaged
vwc |> 
  ggplot() +
  geom_point(aes(x = date, y = m_p34_6, color = "34 - 06")) +
  geom_point(aes(x = date, y = m_p34_16, color = "34 - 16")) +
  geom_point(aes(x = date, y = m_p34_26, color = "34 - 26")) +
  geom_hline(yintercept = 0.04)



# Add predicted swp by texture
# Sandy loam for 6, 16, and 26

swp_vwc <- vwc |>
  select(date, starts_with("m_p34")) |>
  # m_p34_6, m_p34_16, m_p34_26) |>
  pivot_longer(-date, 
               names_to = c("plot", "depth"),
               names_pattern = "m_p(.*)_(.*)",
               values_to = "vwc") |>
  filter(!is.na(vwc)) |> # remove NA's for function to work
  mutate(depth = as.numeric(depth),
         texture = case_when(depth == 6 ~ "sandy loam",
                             depth == 16 ~ "sandy loam",
                             depth == 26 ~ "sandy loam")) |>
  group_by(date, plot, depth, texture) |>
  mutate(vwc = unique(vwc),
         swp = vwc2swp(vwc, param = texture, stat = "median"))

write_csv(swp_vwc, file = "data_clean/neon_vwc_swp_long.csv")

# Plot double axis by depth

swp_vwc |>
  ggplot(aes(x = date)) +
  geom_point(aes(y = swp, 
                 col = "SWP")) +
  geom_point(aes(y = vwc*20-3,
                 col = "VWC")) +
  scale_y_continuous(expression(paste(Psi[soil], " (MPa)")),
                     sec.axis = sec_axis(~(.+3)/20,
                                         expression(paste(Theta, " (", cm^3, " ", cm^-3, ")")))) +
  facet_grid(rows = vars(depth), 
             scales = "free_y",
             space = "free_y") +
  scale_color_manual(values = c("seagreen", "tan")) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank()
        axis.title.y.left = element_text(color = "seagreen"),
        axis.title.y.right = element_text(color = "tan")) +
  guides(color = "none")

# Join psy with swp, plot to see which matches best

psy_swp <- psy_site |>
  left_join(swp_vwc, by = "date")

psy_swp |>
  filter(!is.na(depth)) |>
  ggplot(aes(x = date)) +
  geom_point(aes(y = swp, 
                 col = "SWP")) +
  geom_errorbar(aes(ymin = PD - PD_sd,
                    ymax = PD + PD_sd,
                    col = "PD"),
                alpha = 0.25) +
  geom_point(aes(y = PD, col = "PD")) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  scale_y_continuous(expression(paste(Psi[soil], " (MPa)")),
                     sec.axis = sec_axis(~.,
                                         expression(paste(Psi[PD], " (MPa)")))) +
  facet_grid(rows = vars(depth), 
             scales = "free_y",
             space = "free_y") +
  scale_color_manual(values = c("seagreen", "tan")) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y.left = element_text(color = "tan"),
        axis.title.y.right = element_text(color = "seagreen")) +
  guides(color = "none")
