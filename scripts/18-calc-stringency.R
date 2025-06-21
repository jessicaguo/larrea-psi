# Calculate Stringency (Mencuccini et al. 2024)
# Compare daily to hourly
# Associate with ABA data from Cade
# Compare predawn to soil someday?

library(tidyverse)

# Read in both timescales of shrub level data
psy <- read_csv("data_clean/psy/psy_daily_shrub.csv") |>
  mutate(shrub = factor(shrub))

psy30 <- read_csv("data_clean/psy/psy_30_shrub.csv")

vpd <- read_csv("data_clean/neon_atmdaily.csv") |>
  select(date, Dmax, Dmean)

# Plot
psy |>
  filter(shrub == 3) |>
  ggplot(aes(x = date, col = shrub)) +
  geom_point(aes(y = PD_m, shape = "PD")) +
  geom_point(aes(y = MD_m, shape = "MD")) +
  scale_shape_manual(values = c(22, 16)) +
  facet_wrap(~shrub)

# Need to remove the first and last bits of shrub 3
psy_rm1 <- psy |>
  filter(shrub == 3,
         date < as.Date("2023-04-15"))
psy_rm2 <- psy |>
  filter(shrub == 3,
         date > as.Date("2023-09-01"))

psy_clean <- psy |>
  anti_join(psy_rm1) |>
  anti_join(psy_rm2)

psy_clean |>
  ggplot(aes(x = date, col = shrub)) +
  geom_point(aes(y = PD_m, shape = "PD")) +
  geom_point(aes(y = MD_m, shape = "MD")) +
  scale_shape_manual(values = c(22, 16)) +
  facet_wrap(~shrub)

# Filter for only PD and MD days and add VPD
psy_d <- psy_clean |>
  filter(!is.na(PD_m) & !is.na(MD_m)) |>
  left_join(vpd, by = join_by("date")) |>
# Calculate gs/Kleaf ratio for each observation
  mutate(gk_max = (PD_m - MD_m)/Dmax,
         gk_mean = (PD_m - MD_m)/Dmean)
psy_d |>
  ggplot() +
  geom_histogram(aes(x = gk_max, 
                     fill = shrub)) +
  facet_wrap(~shrub) +
  theme_bw()

# Check for 95 and 99 percentiles of the gs/Kleaf ratio
quantile(psy_d$gk_max, prob = c( 0.95, 0.99), na.rm = TRUE)

psy_d |>
  group_by(shrub) |>
  summarize(gkhat_99 = quantile(gk_max, prob = 0.99, na.rm = TRUE),
            gkhat_95 = quantile(gk_max, prob = 0.95, na.rm = TRUE))

psy_d |>
  ggplot() +
  geom_point(aes(x = date,
                 y = gk_max, 
                 col = shrub)) +
  geom_hline(yintercept = 0.866) + # 95 percentile for all shrubs
  theme_bw()

# Use population-level 95% to determine gk_hat at 0.866
psy_d_clean <- psy_d |>
  filter(gk_max <= 0.866) |>
  mutate(Psi_hf = PD_m - 0.866*Dmax,
         S = MD_m - Psi_hf)

# Plot PD, MD, and Psi_hf
psy_d_clean |>
  ggplot(aes(x = date, col = shrub)) +
  geom_point(aes(y = PD_m),
             size = 0.5) +
  geom_line(aes(y = Psi_hf)) +
  geom_line(aes(y = MD_m),
            lty = "dashed") +
  facet_wrap(~shrub)

# Plot stringency
psy_d_clean |>
  ggplot(aes(x = date, col = shrub)) +
  geom_line(aes(y = S)) +
  geom_point(aes(y = Dmax)) +
  facet_wrap(~shrub)
