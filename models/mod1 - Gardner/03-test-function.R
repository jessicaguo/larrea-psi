#### Test vwc2swp.R on real data
# On inputs from WP4 
# and on instrumented vWC-SWP pairs

library(tidyverse)
source("source/vwc2swp.R")

# Load original input data
# Read in moisture release data
mrc <- read_csv("models/mod1 - Gardner/moisture-release.csv")
# Read in vwc field data
vwc <- read_csv("data_clean/neon_swcdaily.csv")
range(vwc$m_p34_6, na.rm = T)

vwc2swp(range(vwc$m_p34_6, na.rm = T), param = "loamy sand", stat = "median")
vwc2swp(range(vwc$m_p34_16, na.rm = T), param = "sandy loam", stat = "median")
vwc2swp(range(vwc$m_p34_26, na.rm = T), param = "sandy loam", stat = "median")

# Create column of predicted swp
preds <- mrc |> 
  filter(vwc > 0.015) |> 
  mutate(swp_texture = case_when(texture == "loamy sand" ~ vwc2swp(vwc, param = "loamy sand", stat = "median"),
                               texture == "sandy loam" ~ vwc2swp(vwc, param = "sandy loam", stat = "median")))


preds %>%
  ggplot(aes(x = vwc)) +
  geom_point(aes(y = wp,
                 col = "actual")) +
  geom_point(aes(y = swp_texture,
                 col = "by texture")) +
  theme_bw(base_size = 14)



# averaged by soil pits 1&2 or 3&4
# for depths 6, 16, 26, or 56 (deepest was ignored)
# Plots 1&2 averaged
vwc |> 
  ggplot() +
  geom_point(aes(x = date, y = m_p12_6, color = "12 - 6")) +
  geom_point(aes(x = date, y = m_p12_16, color = "12 - 16")) +
  geom_point(aes(x = date, y = m_p12_26, color = "12 - 26")) +
  geom_point(aes(x = date, y = m_p12_56, color = "12 - 56"))

# Plots 3&4 averaged
vwc |> 
  ggplot() +
  geom_point(aes(x = date, y = m_p34_6, color = "34 - 6")) +
  geom_point(aes(x = date, y = m_p34_16, color = "34 - 16")) +
  geom_point(aes(x = date, y = m_p34_26, color = "34 - 26")) +
  geom_hline(yintercept = 0.04)

# Obtain texture by depth
mrc |>
  group_by(horizonTopDepth, horizonBottomDepth) |>
  summarize(texture = unique(texture))

# Add predicted swp by texture
# Loamy sand for 6 cm, sandy loam for 16 and 26

swp_vwc <- vwc |>
  select(date, m_p34_6, m_p34_16, m_p34_26) |>
  pivot_longer(-date, 
               names_to = c("plot", "depth"),
               names_pattern = "m_p(.*)_(.*)",
               values_to = "vwc") |>
  filter(!is.na(vwc)) |> # remove NA's for function to work
  mutate(depth = as.numeric(depth),
         texture = case_when(depth == 6 ~ "loamy sand",
                             depth == 16 ~ "sandy loam",
                             depth == 26 ~ "sandy loam")) |>
  group_by(date, plot, depth, texture) |>
  mutate(vwc = unique(vwc),
         swp = vwc2swp(vwc, param = texture, stat = "median"))


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
  facet_wrap(~depth, ncol = 1) +
  scale_color_manual(values = c("seagreen", "tan")) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.title.y.left = element_text(color = "seagreen"),
        axis.title.y.right = element_text(color = "tan")) +
  guides(color = "none")

