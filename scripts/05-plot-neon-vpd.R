# Explore site-level VPD, precip

library(neonUtilities)
options(stringsAsFactors = FALSE)
library(tidyverse)
library(plantecophys)


if(!dir.exists("data_neon")) {
  dir.create("data_neon")
}

# Download and stack 2023 relative humidity for SRER
# Only run once, or update when new data is available
zipsByProduct(dpID = "DP1.00098.001", 
              site = c("SRER"),
              startdate = "2023-01",
              enddate = "2023-12",
              package = "basic", check.size = TRUE)
stackByTable(filepath = "filesToStack00098/",
             savepath = "data_neon",
             saveUnzippedFiles = FALSE)
unlink("filesToStack00098")

# Load 30 minute data

rh1 <- read_csv("data_neon/stackedFiles/RH_1min.csv")
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


vpd30_tower <- rh30 %>%
  filter(tempRHFinalQF == 0 &
           RHFinalQF == 0 & 
           verticalPosition == "040") %>%
  mutate(VPD = RHtoVPD(RH = RHMean, 
                       TdegC = tempRHMean)) %>%
  select(siteID, verticalPosition,
         startDateTime, RHMean, tempRHMean, VPD)

vpd30_tower %>%
  ggplot(aes(x = startDateTime, y = VPD)) +
  geom_point()


# Read in and join with swc data
env_2023 <- data.frame(dt = seq(as.POSIXct("2023-01-01 00:00:00", tz = "America/Phoenix"),
                                as.POSIXct("2023-10-31 23:30:00", tz = "America/Phoenix"),
                                by = "30 mins"))
swc_all <- read_csv("data_clean/neon_swc30.csv")


env_all <- env_2023 %>%
  left_join(select(vpd30_tower, -siteID, -verticalPosition),
            by = join_by(dt == startDateTime)) %>%
  left_join(swc_all, by = join_by(dt == startDateTime))

# Write out
write_csv(env_all, "data_clean/neon_vpd30.csv")

##### Precipitation #####
# Download and stack 2023 precipitation for SRER
# Only run once, or update when new data is available
zipsByProduct(dpID = "DP1.00006.001", 
              site = c("SRER"),
              startdate = "2023-01",
              package = "basic", check.size = TRUE)
stackByTable(filepath = "filesToStack00006/",
             savepath = "data_neon",
             saveUnzippedFiles = FALSE)
unlink("filesToStack00006")

# Load 30 minute data

ppt30 <- read_csv("data_neon/stackedFiles/PRIPRE_30min.csv")
unique(ppt30$siteID)
table(ppt30$horizontalPosition, ppt30$verticalPosition)
# tower ppt is at HOR 900 and VER 000

# priPrecipFinalQF
# 1 = FAIL
# 0 = PASS

# Use 30 minutely data, 
# quality control for priPrecipBulk


ppt30_out <- ppt30 %>%
  filter(priPrecipFinalQF == 0) %>%
  select(siteID, verticalPosition,
         startDateTime, priPrecipBulk)

ppt30_out %>%
  ggplot(aes(x = startDateTime, y = priPrecipBulk)) +
  geom_point()


# Read in and join with swc data
env_2023 <- data.frame(dt = seq(as.POSIXct("2023-01-01 00:00:00", tz = "America/Phoenix"),
                                as.POSIXct("2023-10-31 23:30:00", tz = "America/Phoenix"),
                                by = "30 mins"))
swc_vpd <- read_csv("data_clean/neon_vpd30.csv")


env_all <- env_2023 %>%
  left_join(select(ppt30_out, -siteID, -verticalPosition),
            by = join_by(dt == startDateTime)) %>%
  left_join(swc_vpd, by = join_by(dt == dt))

# Write out
write_csv(env_all, "data_clean/neon_vpd_ppt30.csv")

#### Pull out atm vars and summarize to daily #### 

env_30 <- read_csv("data_clean/neon_vpd_ppt30.csv",
                   locale = locale(tz = "America/Phoenix"))
atm <- env_30 |> 
  select(dt, priPrecipBulk, tempRHMean, VPD) |> 
  mutate(date = as.Date(dt, tz = "America/Phoenix")) |> 
  mutate_if(is.numeric, function(x) ifelse(is.infinite(x), NA, x)) |> 
  group_by(date) |> 
  summarize(ppt_mm = sum(priPrecipBulk, na.rm = TRUE),
            Tmax = max(tempRHMean, na.rm = TRUE),
            Tmean = mean(tempRHMean, na.rm = TRUE),
            Dmax = max(VPD, na.rm = TRUE),
            Dmean = mean(VPD, na.rm = TRUE),
            ppt_n = sum(!is.na(priPrecipBulk)),
            T_n = sum(!is.na(tempRHMean)),
            D_n = sum(!is.na(VPD))) |> 
  mutate_if(is.numeric, function(x) ifelse(is.infinite(x), NA, x))

write_csv(atm, file = "data_clean/neon_atmdaily.csv")
