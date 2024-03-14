# Calculate ET and GPP from flux variables

library(tidyverse)
library(bigleaf)
library(udunits2)
library(plantecophys)
library(cowplot)

# Experiment with Ameriflux data, which only goes through 2022
ec <- read_csv("data_ameriflux/AMF_US-xSR_BASE_HH_8-5.csv",
               skip = 2,
               col_types = "cc",
               na = c("-9999", "NA")) 

# Convert using bigleaf package and sum to daily
et <- ec |>
  select(starts_with("TIMESTAMP"),
         starts_with("LE"),
         starts_with("TA")) |>
  # four heights of Tair, which to use?
  mutate(ET = LE.to.ET(LE, TA_1_1_1)*30*60, # in kg m-2 per 30 mins
         dt = as.POSIXct(TIMESTAMP_START, 
                         format = "%Y%m%d%H%M", 
                         tz = "America/Phoenix"),
         date = as.Date(dt, tz = "America/Phoenix")) |>
  filter(!is.na(ET)) |>
  group_by(date) |>
  summarize(et_g_cm_day = sum(ET) |>
              ud.convert("kg m^-2", "g cm^-2"), # in g m^-2 per day
            et_mm_day = ud.convert(et_g_cm_day, "cm", "mm"),
            et_n = n()) |>
  mutate(year = lubridate::year(date))

ggplot(et) +
  geom_point(aes(x = date, y = et_mm_day)) +
  facet_wrap(~year, nrow = 1, scales = "free_x") +
  scale_x_date(date_labels = "%b")
# Aggregate from 2004 - 2023 and use height 1 = 2 m above the ground
srm <- srm23 |> 
  bind_rows(srm_ameriflux) |> 
  mutate(dt = as.POSIXct(TIMESTAMP_START, 
                         format = "%Y%m%d%H%M", 
                         tz = "America/Phoenix"),
         date = as.Date(dt, tz = "America/Phoenix")) |> 
  select(dt, date, P, ends_with("1_1")) |> 
  mutate(TA = na_if(TA_1_1_1, -9999),
         RH = na_if(RH_1_1_1, -9999),
         P = na_if(P, -9999)) |> 
  select(-ends_with("1_1"))

