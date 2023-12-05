# After cleaning the data visually with the psychrometer tool
# create several finished products
# 1 - half-hourly by each stem, long
# 2 - PD and MD by each stem, long
# 3 - half-hourly by each shrub, long
# 4 - PD and MD by each shrub, long
# 5 - half-hourly by site, long
# 6 - PD and MD by site, long

library(tidyverse)
source("source/round.POSIXct.R")
library(suncalc)

# Read in data, calculate % of removed data
clean_list <- list.files("data_clean/appended_cleaned")
ts <- data.frame(dt = seq(as.POSIXct("2023-02-15 00:00:00", tz = "America/Phoenix"),
                          as.POSIXct("2023-11-03 23:30:00", tz = "America/Phoenix"),
                          by = 30*60))

perc_rm <- data.frame(branch = character(length(clean_list)),
                      n_tot = numeric(length(clean_list)),
                      n_rm = numeric(length(clean_list)))


for(i in 1:length(clean_list)) {
  stem_name <- str_extract(clean_list[i], "[1-6][a-e]")
  
  temp <- read_csv(paste0("data_clean/appended_cleaned/", clean_list[i]),
                   locale = locale(tz = "America/Phoenix")) |> 
    filter(!is.na(date) & !is.na(time)) |> 
    mutate(dt = round.POSIXct(dt, units = "30 mins"))
  
  # calculate number of WP's removed
  perc_rm[i,] <- c(stem_name, nrow(temp), sum(temp$to_remove == TRUE))
  
  # add column to ts 
  temp_filt <- temp |> 
    filter(to_remove != TRUE,
           !is.na(corrected_water_potential_m_pa)) |> 
    select(dt, corrected_water_potential_m_pa) |> 
    group_by(dt) |> # take mean if duplicated measurements per half-hour
    summarize(WP = mean(corrected_water_potential_m_pa)) |> 
    mutate({{stem_name}} := WP) |> 
    select(-WP)
    
  ts <- ts |> 
    left_join(temp_filt, by = join_by(dt))
}

##### Summarize and clean branch-level products #####
# summarize percent removed by branch
perc_rm <- perc_rm |> 
  mutate(n_rm = as.numeric(n_rm),
         n_tot = as.numeric(n_tot),
         percent_rm = round(n_rm/n_tot, 3))
write_csv(perc_rm, file = "data_clean/psy_stats/30mn_by_branch.csv")

perc_rm |> 
  ggplot() +
  geom_col(aes(x = branch, y = n_tot)) +
  geom_col(aes(x = branch, y = n_rm),
           fill = "red")


# make half-hourly long
psy_30_branch <- ts |> 
  pivot_longer("1a":"6c",
               values_to = "WP_mpa",
               names_to = "branch") |> 
  filter(!is.na(WP_mpa))

write_csv(psy_30_branch, file = "data_clean/psy/psy_30_branch.csv")

# summarize to PD and MD by branch
PD_time <- getSunlightTimes(date = seq(as.Date(min(psy_30_branch$dt)),
                                     as.Date(max(psy_30_branch$dt)),
                                     by = 1),
                          lat = 31.91068,
                          lon = -110.83549,
                          keep = c("sunrise"),
                          tz = "America/Phoenix") |> 
  mutate(sunrise_prior = sunrise - 2*60*60)

MD_time <- getSunlightTimes(date = seq(as.Date(min(psy_30_branch$dt)),
                                       as.Date(max(psy_30_branch$dt)),
                                       by = 1),
                            lat = 31.91068,
                            lon = -110.83549,
                            keep = c("solarNoon"),
                            tz = "America/Phoenix") |> 
  mutate(noon_st = solarNoon - 1*60*60,
         noon_en = solarNoon + 1*60*60)

# Use a non-equi join
PD_wide <- ts |> 
  inner_join(PD_time, join_by(dt >= sunrise_prior, dt <= sunrise))

MD_wide <- ts |> 
  inner_join(MD_time, join_by(dt >= noon_st, dt <= noon_en))

# Calculate predawns & middays
PD_psy <- PD_wide |> 
  pivot_longer("1a":"6c",
               values_to = "WP_mpa",
               names_to = "branch") |> 
  filter(!is.na(WP_mpa)) |> 
  group_by(date, branch) |> 
  summarize(PD = mean(WP_mpa),
            PD_sd = sd(WP_mpa),
            PD_n = n()) |> 
  ungroup()

MD_psy <- MD_wide |> 
  pivot_longer("1a":"6c",
               values_to = "WP_mpa",
               names_to = "branch") |> 
  filter(!is.na(WP_mpa)) |> 
  group_by(date, branch) |> 
  summarize(MD = mean(WP_mpa),
            MD_sd = sd(WP_mpa),
            MD_n = n()) |> 
  ungroup()

MD_psy |> 
  ggplot() +
  geom_col(aes(x = date, y = branch))

daily_psy <- PD_psy |> 
  full_join(MD_psy, by = join_by(date, branch))

write_csv(daily_psy, file = "data_clean/psy/psy_daily_branch.csv")

##### Summarize and clean shrub-level products #####

# Read in half-hourly branch data
psy_30_branch <- read_csv("data_clean/psy/psy_30_branch.csv",
                          locale = locale(tz = "America/Phoenix")) |> 
  mutate(shrub = str_extract(branch, "[1-6]"))

# Summarize to shrub level
psy_30_shrub <- psy_30_branch |> 
  group_by(dt, shrub) |> 
  summarize(WP = mean(WP_mpa),
            WP_sd = sd(WP_mpa),
            WP_n = n())

write_csv(psy_30_shrub, file = "data_clean/psy/psy_30_shrub.csv")

# Read in daily branch data
psy_daily_branch <- read_csv("data_clean/psy/psy_daily_branch.csv",
                          locale = locale(tz = "America/Phoenix")) |> 
  mutate(shrub = str_extract(branch, "[1-6]"))

# Summarize to shrub level
psy_daily_PD <- psy_daily_branch |> 
  filter(!is.na(PD)) |> 
  group_by(date, shrub) |> 
  summarize(PD_m = mean(PD),
            PD_sd = sd(PD),
            PD_n = n())

psy_daily_MD <- psy_daily_branch |> 
  filter(!is.na(MD)) |> 
  group_by(date, shrub) |> 
  summarize(MD_m = mean(MD),
            MD_sd = sd(MD),
            MD_n = n())

psy_daily_shrub <- psy_daily_PD |> 
  full_join(psy_daily_MD, by = join_by("date", "shrub"))
            
write_csv(psy_daily_shrub, file = "data_clean/psy/psy_daily_shrub.csv")

##### Summarize and clean site-level products #####

psy_30_shrub <- read_csv("data_clean/psy/psy_30_shrub.csv")

# summarize to whole site
psy_30_site <- psy_30_shrub |> 
  group_by(dt) |> 
  summarize(WP_m = mean(WP),
            WP_sd = sd(WP),
            WP_n = n())

ggplot(psy_30_site) + 
  geom_point(aes(x = dt, y = WP_m,
                 color = WP_n))

write_csv(psy_30_site, file = "data_clean/psy/psy_30_site.csv")

# Same for PD & MD
psy_daily_shrub <- read_csv("data_clean/psy/psy_daily_shrub.csv")

psy_site_PD <- psy_daily_shrub |> 
  filter(!is.na(PD_m)) |> 
  group_by(date) |> 
  summarize(PD = mean(PD_m),
            PD_sd = sd(PD_m),
            PD_n = n())

psy_site_MD <- psy_daily_shrub |> 
  filter(!is.na(MD_m)) |> 
  group_by(date) |> 
  summarize(MD = mean(MD_m),
            MD_sd = sd(MD_m),
            MD_n = n())

psy_daily_site <- psy_site_PD |> 
  full_join(psy_site_MD, by = join_by("date"))

write_csv(psy_daily_site, file = "data_clean/psy/psy_daily_site.csv")
