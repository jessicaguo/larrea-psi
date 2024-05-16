# Compare SWP and PD
# to each other, and to ET_cumm
# How does EWP differ?

library(tidyverse)

# read in precip
ppt <- read_csv("data_clean/neon_atmdaily.csv") |>
  select(date, ppt_mm)

# read in swp data
swp <- read_csv(file = "data_clean/neon_vwc_swp_long.csv") 
  
# Take average of 6 cm and 16 cm
swp_2 <- swp |>
  filter(depth != 26, 
         !is.na(swp)) |>
  group_by(date) |>
  summarize(swp_6_16 = mean(swp),
            swp_2_n = n())

# Take average of 6 cm, 16 cm, and 26 cm
swp_3 <- swp |>
  filter(!is.na(swp)) |>
  group_by(date) |>
  summarize(swp_6_16_26 = mean(swp),
            swp_3_n = n())

table(swp_2$swp_2_n) # 15 samples with 1 value
table(swp_3$swp_3_n) # 53 samples with 2 values

swp_2 |>
  ggplot(aes(x = date, y = swp_6_16,
             color = as.factor(swp_2_n))) +
  geom_point()

swp_3 |>
  ggplot(aes(x = date, y = swp_6_16_26,
             color = as.factor(swp_3_n))) +
  geom_point()

# Will remove points without full complement of values

swp_all <- swp_2 |>
  filter(swp_2_n == 2) |>
  left_join(filter(swp_3, swp_3_n == 3), 
            by = join_by("date")) 

# read in psy_daily_site
psy <- read_csv("data_clean/psy/psy_daily_site.csv") |>
  left_join(swp_all, by = join_by("date")) |>
  left_join(ppt, by = join_by("date")) |>
  select(-swp_2_n, -swp_3_n) |>
  pivot_longer(cols = starts_with("swp"),
               names_to = "depths",
               names_prefix = "swp_",
               values_to = "swp")

write_csv(psy, file = "data_clean/psy/psy_daily_site_swp.csv")


psy |>
  ggplot(aes(x = date)) +
  geom_col(aes(y = -ppt_mm)) +
  geom_point(aes(y = swp, 
                 color = "SWP")) +
  geom_errorbar(aes(ymin = PD - PD_sd,
                    ymax = PD + PD_sd,
                    col = "PD"),
                width = 0,
                alpha = 0.25) +
  geom_point(aes(y = PD, 
                 color = "PD")) +
  facet_grid(rows = vars(depths)) +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%b") +
  scale_y_continuous(expression(paste(Psi[soil], " (MPa) | precip (-mm)")),
                     limits = c(-10, 0), breaks = seq(-10, 0, 2),
                     sec.axis = sec_axis(~.,
                                         expression(paste(Psi[PD], " (MPa)")),
                                         breaks = seq(-10, 0, 2))) +
  scale_color_manual(values = c("seagreen", "tan")) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y.left = element_text(color = "tan"),
        axis.title.y.right = element_text(color = "seagreen")) +
  guides(color = "none")
  

