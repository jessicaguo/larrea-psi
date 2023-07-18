# Explore site-level soil moisture

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
swc_all <- read_csv("data_clean/neon_swc30.csv")

env_all <- vpd30_tower %>%
  select(-siteID, -verticalPosition) %>%
  full_join(swc_all, by = join_by(startDateTime))

# Write out
write.csv(env_all, "data_clean/neon_vpd30.csv")
