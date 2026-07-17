# Explore site-level VPD, precip

library(neonUtilities)
options(stringsAsFactors = FALSE)
library(tidyverse)
library(plantecophys)

token <- Sys.getenv("NEON_PAT")


if(!dir.exists("data_neon")) {
  dir.create("data_neon")
}

# Download and stack 2023 relative humidity for SRER
# Only run once, or update when new data is available
zipsByProduct(dpID = "DP1.00098.001", 
              site = c("SRER"),
              startdate = "2023-01",
              enddate = "2023-12",
              package = "basic", check.size = TRUE,
              token = token)
stackByTable(filepath = "filesToStack00098/",
             savepath = "data_neon",
             saveUnzippedFiles = FALSE)
unlink("filesToStack00098")

# Load 30 minute data

# rh1 <- read_csv("data_neon/stackedFiles/RH_1min.csv")
rh30 <- read_csv("data_neon/stackedFiles/RH_30min.csv")
unique(rh30$siteID)
table(rh30$horizontalPosition, rh30$verticalPosition)
# tower RH is at HOR 000 and VER 040

# RHFinalQF
# tempRHFinalQF
# 1 = FAIL
# 0 = PASS

# Use 30 minutely data, 
# quality control for RH and temp
# calculate vpd


vpd30_tower <- rh30 |>
  # Keep only vertical position 040 
  filter(verticalPosition == "040") |> 
  # For QF flags != 0, chagne value to NA
  mutate(RHMean = if_else(RHFinalQF != 0, NA, RHMean),
         tempRHMean = if_else(tempRHFinalQF != 0, NA, tempRHMean)) |> 
  mutate(VPD = RHtoVPD(RH = RHMean, 
                       TdegC = tempRHMean)) |>
  # add local time
  mutate(dt = lubridate::with_tz(startDateTime, "America/Phoenix")) |> 
  select(siteID, verticalPosition,
         startDateTime, dt, RHMean, tempRHMean, VPD)

vpd30_tower |>
  ggplot() +
  geom_point(aes(x = dt, y = RHMean,
                 color = "RH")) +
  geom_point(aes(x = dt, y = tempRHMean,
                 color = "temp"))


# How many missing values?
sum(is.na(vpd30_tower$VPD))
#500, gapfill from SRM

#### Gapfilling RH and Temp from SRM ####
srm_vpd <- read_csv("data_ameriflux/US-SRM_HH_202212312330_202312312330.csv",
                     na = "-9999") |> 
  # Theoretically reported in UTC, but actually local time
  mutate(TIMESTAMP_START = as.POSIXct(paste(TIMESTAMP_START),
                         format = "%Y%m%d%H%M",
                         tz = "UTC"),
         dt = force_tz(TIMESTAMP_START, "America/Phoenix")) |> 
  # relocate(dt) |> 
  select(TIMESTAMP_START, dt, TA_1_2_1, RH_1_2_1) |> 
  mutate(VPD_1_2_1 = RHtoVPD(RH = RH_1_2_1, 
                       TdegC = TA_1_2_1))

# Join 2 datasets together
both <- left_join(vpd30_tower,
                  srm_vpd,
                  join_by(dt == dt)) |> 
  arrange(dt)

# Find NAs for temp
na_rects_t <- both |> 
  mutate(is_na = is.na(tempRHMean)) |> 
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
# Most gaps are short, only 3 are longer than 0.5 days
sum(length(which(na_rects_t$dur > 0.5)))

gaps_to_fill_t <- na_rects_t |> 
  filter(dur > 0.5) |> 
  mutate(R2 = NA_real_,
         p = NA_real_,
         slope = NA_real_,
         int = NA_real_)

# For each gap, plot relationship (check linearity) with 4 days on each side
for(i in 1:nrow(gaps_to_fill_t)) {
  st <- gaps_to_fill_d$xmin[i]
  en <- gaps_to_fill_d$xmax[i]
  
  clipped <-   both |> 
    filter(dt > st - 96*60*60,
           dt < en + 96*60*60)
  
  clipped |>
    ggplot() +
    geom_rect(data = gaps_to_fill_t[i,], aes(xmin = xmin, xmax = xmax,
                                             ymin = ymin, ymax = ymax),
              fill = "red", alpha = 0.2, inherit.aes = FALSE) +
    geom_point(aes(x = dt, y = TA_1_2_1,
                   color = "SRM")) +
    geom_point(aes(x = dt, y = tempRHMean,
                   color = "SRER")) 
  
  clipped |>
    ggplot() +
    geom_point(aes(x = TA_1_2_1,
                   y = tempRHMean))
  
  # Simple linear regression
  if(sum(!is.na(clipped$TA_1_2_1)) != 0) {
    m1 <- lm(tempRHMean ~ TA_1_2_1, data = clipped)
    
    # Record parameters
    gaps_to_fill_t$R2[i] <- summary(m1)$adj.r.squared
    f <- summary(m1)$fstatistic
    gaps_to_fill_t$p[i] <-  pf(f[1], f[2], f[3], lower.tail = FALSE)
    gaps_to_fill_t$slope[i] <- coef(summary(m1))[2,1]
    gaps_to_fill_t$int[i] <- coef(summary(m1))[1,1]
  }
}

# Fill in the gap
temp_t <- both |> 
  left_join(gaps_to_fill_t,
            join_by(between(dt, xmin, xmax))) |> 
  mutate(Tair_gap = if_else(!is.na(p), 
                             int + slope * TA_1_2_1,
                             tempRHMean))

sum(is.na(temp_t$tempRHMean)) - sum(is.na(temp_t$Tair_gap))
# Filled in 394 missing air Temp values


# Find NAs for RH
na_rects_rh <- both |> 
  mutate(is_na = is.na(RHMean)) |> 
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
# Most gaps are short, only 3 are longer than 0.5 days
sum(length(which(na_rects_rh$dur > 0.5)))

gaps_to_fill_rh <- na_rects_rh |> 
  filter(dur > 0.5) |> 
  mutate(R2 = NA_real_,
         p = NA_real_,
         slope = NA_real_,
         int = NA_real_)

# For each gap, plot relationship (check linearity) with 4 days on each side
for(i in 1:nrow(gaps_to_fill_rh)) {
  st <- gaps_to_fill_d$xmin[i]
  en <- gaps_to_fill_d$xmax[i]
  
  clipped <-   both |> 
    filter(dt > st - 96*60*60,
           dt < en + 96*60*60)
  
  clipped |>
    ggplot() +
    geom_rect(data = gaps_to_fill_rh[i,], aes(xmin = xmin, xmax = xmax,
                                             ymin = ymin, ymax = ymax),
              fill = "red", alpha = 0.2, inherit.aes = FALSE) +
    geom_point(aes(x = dt, y = RH_1_2_1,
                   color = "SRM")) +
    geom_point(aes(x = dt, y = RHMean,
                   color = "SRER")) 
  
  clipped |>
    ggplot() +
    geom_point(aes(x = RH_1_2_1,
                   y = RHMean))
  
  # Simple linear regression
  if(sum(!is.na(clipped$RH_1_2_1)) != 0) {
    m1 <- lm(RHMean ~ RH_1_2_1, data = clipped)
    
    # Record parameters
    gaps_to_fill_rh$R2[i] <- summary(m1)$adj.r.squared
    f <- summary(m1)$fstatistic
    gaps_to_fill_rh$p[i] <-  pf(f[1], f[2], f[3], lower.tail = FALSE)
    gaps_to_fill_rh$slope[i] <- coef(summary(m1))[2,1]
    gaps_to_fill_rh$int[i] <- coef(summary(m1))[1,1]
  }
}

# Fill in the gap
temp_t_rh <- temp_t |> 
  select(colnames(both), Tair_gap) |> 
  left_join(gaps_to_fill_rh,
            join_by(between(dt, xmin, xmax))) |> 
  mutate(RH_gap = if_else(!is.na(p), 
                            int + slope * RH_1_2_1,
                            RHMean)) |> 
  mutate(VPD_gap = RHtoVPD(RH_gap, Tair_gap))

sum(is.na(temp_t_rh$RHMean)) - sum(is.na(temp_t_rh$RH_gap))
# Filled in 393 missing RH values
sum(is.na(temp_t_rh$VPD)) - sum(is.na(temp_t_rh$VPD_gap))
# Filled in 393 missing VPD values

# Check vpds
ggplot(temp_t_rh) +
  geom_point(aes(x = dt, y = VPD_gap,
                 color = "gap")) +
  geom_point(aes(x = dt, y = VPD,
                 color = "orig"))

ggplot(temp_t_rh) +
  geom_point(aes(x = VPD_1_2_1, y = VPD_gap))


# Write out, dt now in local time
write_csv(temp_t_rh |> select(-startDateTime,
                           -TIMESTAMP_START,
                           -group:-int), 
          "data_clean/neon_atm30.csv")

##### Precipitation #####
# Download and stack 2023 precipitation for SRER
# Only run once, or update when new data is available
zipsByProduct(dpID = "DP1.00044.001", 
              site = c("SRER"),
              startdate = "2023-01",
              enddate = "2023-12",
              package = "basic",
              check.size = FALSE,
              token = token)
# code below is throwing an error, not sure why
# stackByTable(filepath = "filesToStack00044/",
#              savepath = "data_neon",
#              saveUnzippedFiles = FALSE)
unlink("filesToStack00044")

# Try alternative method (this makes available 60min and daily data)

pri <- loadByProduct(dpID = "DP1.00044.001", 
                     site = "SRER", 
                     startdate = "2023-01", 
                     enddate = "2023-12",
                     token = token)

# Load daily data from weighing gauge

# ppt30 <- read_csv("data_neon/stackedFiles/PRIPRE_30min.csv")
ppt60 <- pri$WEIPRE_60min
pri$sensor_positions_00044$HOR.VER
pri$sensor_positions_00044$HOR.VER
# weighing gauge is located at HOR 900 and VER 000

# priPrecipFinalQF
# 1 = FAIL
# 0 = PASS

# Use 60 minutely data, 
# quality control for priPrecipBulk
ppt_daily <- ppt60  |> 
  # For QF flags != 0, change value to NA
  mutate(precipBulk = if_else(finalQF != 0, NA, precipBulk)) |> 
  # add local time
  mutate(dt = lubridate::with_tz(startDateTime, "America/Phoenix"),
         date = as.Date(dt, tz = "America/Phoenix")) |> 
  select(siteID, verticalPosition,
         startDateTime, dt, date, precipBulk) |> 
  group_by(date) |> 
  summarize(ppt = sum(precipBulk))

ppt_daily |>
  ggplot(aes(x = date, y = ppt)) +
  geom_point()

#### Gapfilling daily PPT from SRM ####
srm_ppt <- read_csv("data_ameriflux/US-SRM_HH_202212312330_202312312330.csv",
                    na = "-9999") |> 
  # Theoretically reported in UTC, but actually local time
  mutate(TIMESTAMP_START = as.POSIXct(paste(TIMESTAMP_START),
                                      format = "%Y%m%d%H%M",
                                      tz = "UTC"),
         dt = force_tz(TIMESTAMP_START, "America/Phoenix"),
         date = as.Date(dt, tz = "America/Phoenix")) |> 
  # relocate(dt) |> 
  select(TIMESTAMP_START, dt, date, P) |> 
  group_by(date) |> 
  summarize(ppt_1_2_1 = sum(P))

# Join 2 datasets together
both <- left_join(ppt_daily,
                  srm_ppt,
                  join_by(date)) |> 
  arrange(date)

# Find NAs for vpd
na_rects_p <- both |> 
  mutate(is_na = is.na(ppt)) |> 
  # Create unique IDs for contiguous NA groups vs. valid data groups
  mutate(group = cumsum(is_na != lag(is_na, default = first(is_na)))) |> 
  filter(is_na == TRUE) |> 
  group_by(group) %>%
  summarize(
    xmin = min(date),
    xmax = max(date),
    ymin = -Inf,
    ymax = Inf,
    dur = difftime(xmax, xmin, units = "days")
  )

gaps_to_fill_p <- na_rects_p |> 
  mutate(R2 = NA_real_,
         p = NA_real_,
         slope = NA_real_,
         int = NA_real_)

# For each gap, plot relationship (check linearity) with 14 days on each side
for(i in 1:nrow(gaps_to_fill_p)) {
  st <- gaps_to_fill_p$xmin[i]
  en <- gaps_to_fill_p$xmax[i]
  
  clipped <-   both |> 
    filter(date > st - 14,
           date < en + 14)
  
  clipped |>
    ggplot() +
    geom_rect(data = gaps_to_fill_p[i,], aes(xmin = xmin, xmax = xmax,
                                             ymin = ymin, ymax = ymax),
              fill = "red", alpha = 0.2, inherit.aes = FALSE) +
    geom_point(aes(x = date, y = ppt_1_2_1,
                   color = "SRM")) +
    geom_point(aes(x = date, y = ppt,
                   color = "SRER")) 
  
  clipped |>
    ggplot() +
    geom_point(aes(x = ppt_1_2_1,
                   y = ppt))
  
  # Simple linear regression
  if(sum(clipped$ppt_1_2_1) != 0) {
    m1 <- lm(ppt ~ ppt_1_2_1, data = clipped)
    
    # Record parameters
    gaps_to_fill_p$R2[i] <- summary(m1)$adj.r.squared
    f <- summary(m1)$fstatistic
    gaps_to_fill_p$p[i] <-  pf(f[1], f[2], f[3], lower.tail = FALSE)
    gaps_to_fill_p$slope[i] <- coef(summary(m1))[2,1]
    gaps_to_fill_p$int[i] <- coef(summary(m1))[1,1]
  }
}

# Fill in the gap
temp_p <- both |> 
  left_join(gaps_to_fill_p,
            join_by(between(date, xmin, xmax))) |> 
  mutate(ppt_gap = case_when(is.na(dur) ~ ppt,
                             !is.na(dur) & ppt_1_2_1 == 0 ~ 0,
                             !is.na(dur) & !is.na(p) ~ int + slope * ppt_1_2_1))

sum(is.na(temp_p$ppt)) - sum(is.na(temp_p$ppt_gap))
# Filled in 15 missing values
sum(is.na(temp_p$ppt_gap))
# no missing values at the daily level

# Write out, datet now in local time
write_csv(temp_p |> select(-group:-int), 
          "data_clean/neon_pptdaily.csv")

#### Summarize atm vars to daily, join with ppt #### 

atm30 <- read_csv("data_clean/neon_atm30.csv",
                   locale = locale(tz = "America/Phoenix"))
pptdaily <- read_csv("data_clean/neon_pptdaily.csv",
                  locale = locale(tz = "America/Phoenix"))
atm <- atm30 |> 
  mutate(date = as.Date(dt, tz = "America/Phoenix")) |> 
  # mutate_if(is.numeric, function(x) ifelse(is.infinite(x), NA, x)) |> 
  group_by(date) |> 
  summarize(Tmax = max(Tair_gap, na.rm = TRUE),
            Tmean = mean(Tair_gap, na.rm = TRUE),
            Dmax = max(VPD_gap, na.rm = TRUE),
            Dmean = mean(VPD_gap, na.rm = TRUE),
            T_n = sum(!is.na(Tair_gap)),
            D_n = sum(!is.na(VPD_gap))) |> 
  left_join(pptdaily |> select(date, ppt_gap)) |> 
  rename(ppt_mm = ppt_gap)

write_csv(atm, file = "data_clean/neon_atmdaily_gap.csv")
