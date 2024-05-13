# Explore moisture release curve for NEON 
# Need to model and create function

library(tidyverse)
library(readxl)

# read in bulk density
bd <- read_csv("data_neon/megapit_bulkdens.csv") |>
  # bulkDensExclCoarseFrag is in g cm^-3
  select(horizonName:depthID) |>
  mutate(depthID = as.character(depthID),
         texture = c("loamy sand", "sandy loam", "sandy loam", "loamy sand", "sandy loam")) 

mrc <- read_excel("data_neon/Soil Moisture Release Curves - RainMan Soils.xlsx",
                  sheet = "Plots") |> 
  filter(grepl("NEON", sample_id)) |> # Only soils from NEON-SRER
  rename(cup_dry_soil_g = "cup_dry soil_g",
         wp = "mpa") |> 
  mutate(gwc = (cup_soil_g - cup_dry_soil_g) / (cup_dry_soil_g - cup_g)) |>
  tidyr::separate_wider_position(sample_id, widths = c(ID = 4, depthID = 1)) |>
  left_join(select(bd, depthID, horizonTopDepth, horizonBottomDepth,
                   bulkDensTopDepth, bulkDensBottomDepth,
                   bulkDensExclCoarseFrag, coarseFrag2To5:clayTotal, texture
                   )) |>
  # calculated volumetric WC from gravimetric WC
  mutate(vwc = gwc * bulkDensExclCoarseFrag)
  
mrc |>
  ggplot() +
  geom_point(aes(x = gwc, y = abs(wp),
                 color = depthID)) +
  geom_line(aes(x = gwc, y = abs(wp),
                color = depthID))

mrc |>
  ggplot() +
  geom_point(aes(x = vwc, y = abs(wp),
                 color = depthID)) +
  geom_line(aes(x = vwc, y = abs(wp),
                group = depthID,
                lty = texture)) +
  theme_bw(base_size = 14)
# Plotting by volumetric seems to separate out the 2 soil types even further
# Both depth 1 and 4 are loamy sand (or very close to it), with very high sand content
# While depths 2, 3, 5 are sandy loam

# write out csv with relevant details

write_csv(mrc, file = "models/mod1 - Gardner/moisture-release.csv")

