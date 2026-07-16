# Explore site-level soil moisture

library(neonUtilities)
options(stringsAsFactors = FALSE)
library(tidyverse)

token <- Sys.getenv("NEON_PAT")


if(!dir.exists("data_neon")) {
  dir.create("data_neon")
}

# Download and stack 2023 soil moisture for SRER
# Only run once, or update when new data is available
zipsByProduct(dpID = "DP1.00094.001",
              site = c("SRER"),
              startdate = "2023-01",
              enddate = "2023-12",
              package = "basic", 
              check.size = FALSE,
              token = token)
stackByTable(filepath = "filesToStack00094/",
             savepath = "data_neon",
             saveUnzippedFiles = FALSE)
unlink("filesToStack00094")


# Load 30 minute data

swc30 <- read_csv("data_neon/stackedFiles/SWS_30_minute.csv")
unique(swc30$siteID)
table(swc30$horizontalPosition, swc30$verticalPosition)

# VSWCFinalQF overall quality flag, 1=fail, 2=pass
# VSWCFinalQFSciRvw
# 2=fail and data redacted, 
# 1=fail but data retained and may be useful with additional corrections or filtering
# 0=previous quality concern resolved and final quality flag correctly reflects data quality

sum_swc30 <- swc30 %>%
  group_by(horizontalPosition,
           verticalPosition) %>%
  summarize(fail = length(which(VSWCFinalQF == 1)),
            pass = length(which(VSWCFinalQF == 0)),
            total = n()) %>%
  mutate(perc_fail = fail/total*100)

sum_swc30 %>% 
  filter(verticalPosition %in% 501:504) %>%
  ggplot(aes(x = perc_fail,
             fill = as.factor(verticalPosition))) +
  stat_density(alpha = 0.25)


# Filter for VSWCFinalQF == 0 and plot the same depth together
swc30 %>%
  filter(VSWCFinalQF == 0,
         verticalPosition == 501) %>%
  ggplot(aes(x = startDateTime,
             y = VSWCMean,
             color = horizontalPosition)) +
  geom_point()

swc30 %>%
  filter(VSWCFinalQF == 0,
         verticalPosition == 502) %>%
  ggplot(aes(x = startDateTime,
             y = VSWCMean,
             color = horizontalPosition)) +
  geom_point()

swc30 %>%
  filter(VSWCFinalQF == 0,
         verticalPosition == 503) %>%
  ggplot(aes(x = startDateTime,
             y = VSWCMean,
             color = horizontalPosition)) +
  geom_point()
# 005.503 is not working

swc30 %>%
  filter(VSWCFinalQF == 0,
         verticalPosition == 504) %>%
  ggplot(aes(x = startDateTime,
             y = VSWCMean,
             color = horizontalPosition)) +
  geom_point()

# Take mean of plots 1/2 and 3/4, for top 4 layers
# Keep only if 2 or more points included in mean
# plots 1/2 have missing 06 data, but more infiltration measured at 56
# plots 3/4 have more complete 06-26 data, but no measurements at 56

swc_12 <- swc30 %>%
  filter(VSWCFinalQF == 0,
         verticalPosition %in% c(501, 502, 503, 504),
         horizontalPosition %in% c("001", "002")) %>%
  filter(!is.na(VSWCMean)) %>%
  mutate(depth = case_when(verticalPosition == 501 ~ 6,
                           verticalPosition == 502 ~ 16,
                           verticalPosition == 503 ~ 26,
                           verticalPosition == 504 ~ 56)) %>%
  group_by(depth,
           startDateTime) %>%
  summarize(swc_mean = mean(VSWCMean),
            swc_n = n()) %>%
  filter(swc_n > 1) %>%
  pivot_wider(names_from = depth, 
              values_from = swc_mean,
              names_prefix = "p12_")

swc_34 <- swc30 %>%
  filter(VSWCFinalQF == 0,
         verticalPosition %in% c(501, 502, 503),
         horizontalPosition %in% c("003", "004")) %>%
  filter(!is.na(VSWCMean)) %>%
  mutate(depth = case_when(verticalPosition == 501 ~ 6,
                           verticalPosition == 502 ~ 16,
                           verticalPosition == 503 ~ 26)) %>%
  group_by(depth,
           startDateTime) %>%
  summarize(swc_mean = mean(VSWCMean),
            swc_n = n()) %>%
  filter(swc_n > 1) %>%
  pivot_wider(names_from = depth, 
              values_from = swc_mean,
              names_prefix = "p34_")
  
# Check plots
swc_12 %>%  
  ggplot() +
  geom_point(aes(x = startDateTime,
                 y = p12_6,
                 color = "06 cm")) +
  geom_point(aes(x = startDateTime,
                 y = p12_16, 
                 color = "16 cm")) +
  geom_point(aes(x = startDateTime,
                 y = p12_26, 
                 color = "26 cm")) +
  geom_point(aes(x = startDateTime,
                 y = p12_56, 
                 color = "56 cm"))

  
  
swc_34 %>%  
  ggplot() +
  geom_point(aes(x = startDateTime,
                 y = p34_6,
                 color = "06 cm")) +
  geom_point(aes(x = startDateTime,
                 y = p34_16, 
                 color = "16 cm")) +
  geom_point(aes(x = startDateTime,
                 y = p34_26, 
                 color = "26 cm")) 


# combine and output
swc_all <- full_join(select(swc_12, -swc_n), select(swc_34, -swc_n), by = join_by(startDateTime))

# Check for gaps in the average of 3 and 4
ggplot(swc_all) +
  geom_point(aes(x = startDateTime, y = p34_6,
                 color = "6 cm")) +
  geom_point(aes(x = startDateTime, y = p34_16,
                 color = "16 cm"))+
  geom_point(aes(x = startDateTime, y = p34_26,
                 color = "26 cm"))

# Total number of NAs
sum(is.na(swc_all$p34_6))# 44843
sum(is.na(swc_all$p34_16))# 7677
sum(is.na(swc_all$p34_26))# 9979

# Try gapfilling with SRM data
# Soil depths are:
# 1_1 = 5 cm
# 1_2 = 10 cm
# 1_3 = 20 cm
# 1_4 = 30 cm
# 1_5 = 50 cm
srm_soil <- read_csv("data_ameriflux/US-SRM_HH_202212312330_202312312330.csv",
                     na = "-9999") |> 
  # Also reported in UTC
  mutate(dt = as.POSIXct(paste(TIMESTAMP_START),
                         format = "%Y%m%d%H%M")) |> 
  relocate(dt) |> 
  select(dt, starts_with("SWC_")) |> 
  mutate(srm_05 = SWC_1_1_A/100,
         srm_10 = SWC_1_2_A/100,
         srm_20 = SWC_1_3_A/100,
         srm_30 = SWC_1_4_A/100,
         srm_50 = SWC_1_5_A/100)

# Missing data in SRM too, but mostly in March
sum(is.na(srm_soil$SWC_1_1_A))


both <- left_join(swc_all,
                  select(srm_soil, dt, starts_with("srm")),
                  join_by(startDateTime == dt)) |> 
  rename(dt = startDateTime) |> 
  arrange(dt)


#### Gapfill for p34_6 using SRM's 10 cm data ####
# Find NAs for p34_6
na_rects_6 <- both |> 
  mutate(is_na = is.na(p34_6)) |> 
  # Create unique IDs for contiguous NA groups vs. valid data groups
  mutate(group = cumsum(is_na != lag(is_na, default = first(is_na)))) |> 
  filter(is_na == TRUE) |> 
  group_by(group) %>%
  summarize(
    xmin = min(dt),
    xmax = max(dt),
    ymin = -Inf,
    ymax = Inf,
    dur = difftime(xmax, xmin, units = "days")
  )
# Most gaps are short, only 11 are longer than 12 hours (0.5 day)
sum(length(which(na_rects_6$dur > 0.5)))

gaps_to_fill_6 <- na_rects_6 |> 
  filter(dur > 0.5) |> 
  mutate(R2 = NA_real_,
         p = NA_real_,
         slope = NA_real_,
         int = NA_real_)

# For each gap, plot relationship (check linearity) with 2 days on each side
for(i in 1:nrow(gaps_to_fill_6)) {
  st <- gaps_to_fill_6$xmin[i]
  en <- gaps_to_fill_6$xmax[i]
  
  clipped <-   both |> 
    filter(dt > st - 96*60*60,
           dt < en + 96*60*60)
  
  clipped |>
    ggplot() +
    geom_rect(data = gaps_to_fill_6[i,], aes(xmin = xmin, xmax = xmax,
                                   ymin = ymin, ymax = ymax),
              fill = "red", alpha = 0.2, inherit.aes = FALSE) +
    geom_point(aes(x = dt, y = srm_05,
                   color = "05 cm")) +
    geom_point(aes(x = dt, y = srm_10,
                   color = "10 cm")) +
    geom_point(aes(x = dt, y = p34_6,
                   color = "6 cm"))

  clipped |>
    ggplot() +
    geom_point(aes(x = srm_10,
                   y = p34_6))
  
  # Simple linear regression
  m1 <- lm(p34_6 ~ srm_10, data = clipped)
  
  # Record parameters
  gaps_to_fill_6$R2[i] <- summary(m1)$adj.r.squared
  f <- summary(m1)$fstatistic
  gaps_to_fill_6$p[i] <-  pf(f[1], f[2], f[3], lower.tail = FALSE)
  gaps_to_fill_6$slope[i] <- coef(summary(m1))[2,1]
  gaps_to_fill_6$int[i] <- coef(summary(m1))[1,1]
  
}

# Fill in the gap
temp_6 <- both |> 
  left_join(gaps_to_fill_6,
            join_by(between(dt, xmin, xmax))) |> 
  mutate(p34_6_gap = if_else(!is.na(dur), 
                             int + slope * srm_10,
                             p34_6))

sum(is.na(temp_6$p34_6)) - sum(is.na(temp_6$p34_6_gap))
# 437 missing rows filled

temp_6 |> 
  mutate(is_na = is.na(p34_6_gap)) |> 
  # Create unique IDs for contiguous NA groups vs. valid data groups
  mutate(group = cumsum(is_na != lag(is_na, default = first(is_na)))) |> 
  filter(is_na == TRUE) |> 
  group_by(group) %>%
  summarize(
    xmin = min(dt),
    xmax = max(dt),
    dur = difftime(xmax, xmin, units = "days")
  ) |> 
  arrange(desc(dur)) |>
  head(3)
# Longest gap is now 12 hours or less
  

#### Gapfill for p34_16 using SRM's 20 cm data ####
# Find NAs for p34_16
na_rects_16 <- both |> 
  mutate(is_na = is.na(p34_16)) |> 
  # Create unique IDs for contiguous NA groups vs. valid data groups
  mutate(group = cumsum(is_na != lag(is_na, default = first(is_na)))) |> 
  filter(is_na == TRUE) |> 
  group_by(group) %>%
  summarize(
    xmin = min(dt),
    xmax = max(dt),
    ymin = -Inf,
    ymax = Inf,
    dur = difftime(xmax, xmin, units = "days")
  )
# Most gaps are short, only 5 are longer than 15 hours (0.625 day)
sum(length(which(na_rects_16$dur > 0.625)))

gaps_to_fill_16 <- na_rects_16 |> 
  filter(dur > 0.625) |> 
  mutate(R2 = NA_real_,
         p = NA_real_,
         slope = NA_real_,
         int = NA_real_)

# For each gap, plot relationship (check linearity) with 2 days on each side
for(i in 1:nrow(gaps_to_fill_16)) {
  st <- gaps_to_fill_16$xmin[i]
  en <- gaps_to_fill_16$xmax[i]
  
  clipped <-   both |> 
    filter(dt > st - 96*60*60,
           dt < en + 96*60*60)
  
  clipped |>
    ggplot() +
    geom_rect(data = gaps_to_fill_16[i,], aes(xmin = xmin, xmax = xmax,
                                             ymin = ymin, ymax = ymax),
              fill = "red", alpha = 0.2, inherit.aes = FALSE) +
    geom_point(aes(x = dt, y = srm_20,
                   color = "20 cm")) +
    geom_point(aes(x = dt, y = srm_10,
                   color = "10 cm")) +
    geom_point(aes(x = dt, y = p34_16,
                   color = "16 cm"))
  
  clipped |>
    ggplot() +
    geom_point(aes(x = srm_20,
                   y = p34_16))
  
  # Simple linear regression
  m1 <- lm(p34_16 ~ srm_20, data = clipped)
  
  # Record parameters
  gaps_to_fill_16$R2[i] <- summary(m1)$adj.r.squared
  f <- summary(m1)$fstatistic
  gaps_to_fill_16$p[i] <-  pf(f[1], f[2], f[3], lower.tail = FALSE)
  gaps_to_fill_16$slope[i] <- coef(summary(m1))[2,1]
  gaps_to_fill_16$int[i] <- coef(summary(m1))[1,1]
}

# Fill in the gap
temp_16 <- both |> 
  left_join(gaps_to_fill_16,
            join_by(between(dt, xmin, xmax))) |> 
  mutate(p34_16_gap = if_else(!is.na(dur), 
                             int + slope * srm_20,
                             p34_16))

sum(is.na(temp_16$p34_16)) - sum(is.na(temp_16$p34_16_gap))
# 176 missing rows filled

temp_16 |> 
  mutate(is_na = is.na(p34_16_gap)) |> 
  # Create unique IDs for contiguous NA groups vs. valid data groups
  mutate(group = cumsum(is_na != lag(is_na, default = first(is_na)))) |> 
  filter(is_na == TRUE) |> 
  group_by(group) %>%
  summarize(
    xmin = min(dt),
    xmax = max(dt),
    dur = difftime(xmax, xmin, units = "days")
  ) |> 
  arrange(desc(dur)) |>
  head(3)
# Longest gap is now 15 hours or less


# p34_26 data cannot be gapfilled well - too mismatched
# Will employ linear interpolation at the daily scale if needed

both$p34_6_gap <- temp_6$p34_6_gap
both$p34_16_gap <- temp_16$p34_16_gap

# No
# Plot both as timeseries 
ggplot(both) +
  geom_rect(data = na_rects, aes(xmin = xmin, xmax = xmax, 
                                 ymin = ymin, ymax = ymax), 
            fill = "red", alpha = 0.2, inherit.aes = FALSE) +
  geom_point(aes(x = dt, y = srm_05,
                 color = "05 cm")) +
  geom_point(aes(x = dt, y = srm_10,
                 color = "10 cm")) +
  geom_point(aes(x = dt, y = p34_6,
                 color = "16cm"))

# check linearity of relationships
ggplot(both) +
  geom_point(aes(x = srm_10,
                 y = p34_6,
                 color = startDateTime))



write_csv(both, "data_clean/neon_swc30.csv")
# Written out as the original tz of UTC

# Average to daily values

swc30 <- read_csv("data_clean/neon_swc30.csv") |> 
  # Read in as UTC, then convert to local
    mutate(dt = lubridate::with_tz(dt, tzone = "America/Phoenix")) |> 
  # Add date in America/Phoenix
  mutate(date = as.Date(dt, tz = "America/Phoenix"))

# attr(swc30$startDateTime, "tzone")


# Summarize to daily means, sd, and n

swc_daily <- swc30 |> 
  select(dt, date, starts_with("p")) |> 
  pivot_longer(cols = c(-1, -2), 
               names_to = "depth",
               values_to = "swc") %>%
  group_by(date, depth) %>%
  summarize(m = mean(swc, na.rm = TRUE),
            sd = sd(swc, na.rm = TRUE),
            n = sum(!is.na(swc))) %>%
  pivot_wider(names_from = depth,
              names_sep = "_",
              values_from = c(m, sd, n))

sum(is.na(swc_daily$m_p34_6_gap))
sum(is.na(swc_daily$m_p34_16_gap))
min(swc_daily$n_p34_6_gap)
min(swc_daily$n_p34_16_gap)

# Some daily means with very little data, but we need unbroken records

write_csv(swc_daily, "data_clean/neon_swcdaily.csv")
