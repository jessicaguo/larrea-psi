library(tidyverse)
library(suncalc)

# read in site-level 30-min data
psy <- read_csv("data_clean/psy/psy_30_site.csv",
                locale = locale(tz = "America/Phoenix")) |> 
  mutate(date_only = as.Date(dt), # needs tz = "America/Phoenix argument to be correct, but plots better this way
         date = date_only |> 
           as.POSIXct() |> 
           force_tz(tzone = "America/Phoenix"),
         month = lubridate::month(date),
         time = difftime(dt, date, units = "hours"),
         day = lubridate::day(date),
         month_label = lubridate::month(date, label = TRUE))
str(psy)
unique(psy$date)

# Determine predawn times
PD_time <- getSunlightTimes(date = unique(psy$date_only),
                            lat = 31.91068,
                            lon = -110.83549,
                            keep = c("sunrise"),
                            tz = "America/Phoenix") |> 
  mutate(date = date |> 
           as.POSIXct() |> 
           force_tz(tzone = "America/Phoenix"),
         time = difftime(sunrise, date, units = "hours"),
         month = lubridate::month(date),
         day = lubridate::day(date),
         month_label = lubridate::month(date, label = TRUE))

psy |> 
  filter(month %in% 4:6) |> 
  ggplot(aes(x = time, y = WP_m)) +
  geom_point(aes(color = day)) +
  geom_vline(data = PD_time |> filter(month %in% 4:6),
            aes(xintercept = time, color = day)) +
  facet_grid(rows = vars(month_label),
             scales = "free_y",
             space = "free_y") +
  scale_color_gradient("Day of month",
                       low = "seagreen",
                       high = "aquamarine") +
  scale_x_continuous("Hours",
                     limits = c(-7, 9)) +
  scale_y_continuous(expression(paste(Psi[stem], " (MPa)"))) +
  theme_bw(base_size = 14) +
  theme(panel.grid = element_blank(),
        strip.background = element_blank())

ggsave(filename = "figs/predawn_recovery_apr_june.png",
       height = 7,
       width = 6,
       units = "in")

temp <- psy |> 
  filter(month %in% 4:6)

write_csv(temp, "data_clean/psy/SRER_LATR_pdd_Apr_June.csv")
