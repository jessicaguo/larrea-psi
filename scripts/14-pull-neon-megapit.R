# Need to obtail bulk density of each soil layer in the SRER megapit

# Explore site-level soil moisture

# library(neonUtilities)
# options(stringsAsFactors = FALSE)
library(tidyverse)


if(!dir.exists("data_neon")) {
  dir.create("data_neon")
}

# Download megapit soil properties for SRER
# Only run once
# zipsByProduct(dpID = "DP1.00096.001", 
#               site = c("SRER"),
#               package = "basic", check.size = TRUE)
# stackByTable(filepath = "filesToStack00096/",
#              savepath = "data_neon",
#              saveUnzippedFiles = FALSE)
# unlink("filesToStack00096")

# Soil classification
# mgp_permegapit - thermic typic torrifluvents entisols
# coarse-loamy mixed calcereous

# Soil horizons and depths
# mgp_perhorizon
horizon <- read_csv("data_neon/stackedFiles/mgp_perhorizon.csv")

# Texture + chemical properties in
# mgp_perbiogeosample
texture <- read_csv("data_neon/stackedFiles/mgp_perbiogeosample.csv") |>
  select(horizonID, horizonName, coarseFrag2To5:clayTotal)


# Bulk density
# mgp_perbulksample
# provides medium, top, and bottom bulk density
# as 'bulkDensExclCoarseFrag'
# match to horizons for actual depths of horizons

bd <- read_csv("data_neon/stackedFiles/mgp_perbulksample.csv") |>
  left_join(horizon,
            by = join_by(domainID, siteID, pitNamedLocation, pitID,
                         horizonID, horizonName)) |>
  left_join(texture, by = join_by(horizonID, horizonName)) |>
  arrange(horizonTopDepth) |>
  mutate(depthID = 1:5) |>
  select(-remarks.x:-collectDate.y, -remarks.y:-release.y)
colnames(bd)
bd$bulkDensExclCoarseFrag

write_csv(bd, file = "data_neon/megapit_bulkdens.csv")

