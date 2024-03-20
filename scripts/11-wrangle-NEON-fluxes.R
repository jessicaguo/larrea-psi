# plot neon fluxes
# download PAR
# prepare for partitioning
# calculate ET for 2023

#BiocManager::install('rhdf5')
library(neonUtilities)
options(stringsAsFactors = FALSE)
library(tidyverse)
library(bigleaf)
library(udunits2)

# Download and stack 2023 eddy flux bundle for SRER
# Only run once, or update when new data is available
zipsByProduct(dpID = "DP4.00200.001", 
              site = c("SRER"),
              startdate = "2023-01",
              enddate = "2023-12",
              package = "basic", check.size = TRUE)

flux <- stackEddy(filepath = "filesToStack00200/",
          level = "dp04")

unlink("filesToStack00200/")


names(flux)
dictionary <- flux$objDesc
units <- flux$variables
colnames(flux$SRER)
str(flux$SRER)

# Read in RH
rh30 <- read_csv("data_neon/stackedFiles/RH_30min.csv") |>
  filter(horizontalPosition == "000") # from tower, height = 040

colnames(rh30)

# Download and stack PAR
# Only run once, or update when new data is available
zipsByProduct(dpID = "DP1.00024.001",
              site = c("SRER"),
              startdate = "2023-01",
              enddate = "2023-12",
              package = "basic", check.size = TRUE)
stackByTable(filepath = "filesToStack00024/",
             savepath = "data_neon",
             saveUnzippedFiles = FALSE)
unlink("filesToStack00024")

par30 <- read_csv("data_neon/stackedFiles/PARPAR_30min.csv") |>
  filter(verticalPosition == "040")


# Join fluxes with SRER RH and PAR
out <- flux$SRER |>
  left_join(rh30, by = join_by("timeBgn" == "startDateTime")) |>
  left_join(par30, by = join_by("timeBgn" == "startDateTime")) |>
  mutate(timeBgn = with_tz(timeBgn, tzone = "America/Phoenix"),
         timeEnd = with_tz(timeEnd, tzone = "America/Phoenix"))

write_csv(out, file = "data_neon/stackedFiles/FLUX_rh_temp.csv")

save(flux, file = "data_neon/SRER_eddy_list.Rdata")


# timestamps are in UTC
out |>
  filter(timeBgn >= as.POSIXct("2023-04-01", tz = "America/Phoenix"),
         timeBgn <= as.POSIXct("2023-04-02", tz = "America/Phoenix")) |>
  ggplot() +
  geom_point(aes(x = timeBgn,
                 y = data.fluxCo2.nsae.flux))



