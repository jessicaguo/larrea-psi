# Compare REddyProc version vs.
# Marguerite's output done with the online tool

library(tidyverse)
library(bigleaf)
library(udunits2)

# Read in gap-filled data
# Headers
eddy_gap_all_head <- read_tsv("data_clean/xSR_eddyproc.txt",
                              n_max = 0)
eddy_gap_mm_head <- read_tsv("data_clean/xSR_eddyproc_MM.txt",
                             n_max = 0)
# Data (skipping units row)
eddy_gap_all <- read_tsv("data_clean/xSR_eddyproc.txt",
                         skip = 1)
eddy_gap_mm <- read_tsv("data_clean/xSR_eddyproc_MM.txt",
                         skip = 1)
# Combine column names
colnames(eddy_gap_all) <- colnames(eddy_gap_all_head)
colnames(eddy_gap_mm) <- colnames(eddy_gap_mm_head)

# Convert -9999 to NA
eddy_gap_all <- eddy_gap_all |>
  mutate(across(Ustar:H, ~ifelse(.x == -9999, NA, .x)))

eddy_gap_mm <- eddy_gap_mm |>
  mutate(across(NEE:Rg_fall_qc, ~ifelse(.x == -9999, NA, .x)))

# Check for NA's, structure, column names
sum(is.na(eddy_gap_all$LE)); sum(is.na(eddy_gap_mm$LE))
sum(is.na(eddy_gap_all$LE_U50_f)); sum(is.na(eddy_gap_mm$LE_f))

plot(eddy_gap_all$LE_U50_f)
points(eddy_gap_mm$LE_f, col = "red")

plot(x = eddy_gap_all$LE_U50_f, y = eddy_gap_mm$LE_f)
points(, col = "red")

plot(eddy_gap_all$GPP_U50_f)
points(eddy_gap_mm$GPP_f, col = "red")
points(eddy_gap_mm$GPP_DT, col = "blue")


# Convert and summarize MM's data to daily values for downstream analysis


colnames(eddy_gap_mm)
# LE_f
# NEE_f
# GPP_f
# Reco
# GPP_DT
# Reco_DT
# VPD_f
# Tair_f


# Select relevant columns only
# Add date and dt back in POSIX
eddy_mm <- eddy_gap_mm |>
  dplyr::select(`Date Time`:Hour, VPD_f, Tair_f, LE_f, NEE_f, GPP_f, Reco, GPP_DT, Reco_DT) |>
  mutate(date = parse_date_time(x = paste(Year, DoY), orders = "yj"),
         hr = floor(Hour),
         min = Hour%%1*60,
         dt = parse_date_time(x = paste(Year, DoY, hr, ":", min), orders = "yjHM") - 30*60) |>
  # convert to ET
  mutate(ET_f = LE.to.ET(LE_f, Tair_f)) # kg m^-2 s^-1



# Summarize to daily

flux_daily <- eddy_mm |>
  mutate(et_half = ET_f *30 * 60,# multiply to get kg m-2 per 30 mins
         gpp_half = GPP_f*30*60, # to get umol m-2 per 30 mins
         gpp_dt_half = GPP_DT*30*60, # to get umol m-2 per 30 mins
         nee_half = NEE_f*30*60, # to get umol m-2 per 30 mins
         reco_half = Reco*30*60, # to get umol m-2 per 30 mins
         reco_dt_half = Reco_DT*30*60 # to get umol m-2 per 30 mins
         ) |> 
  group_by(date) |>
  summarize(et_g_cm_day = sum(et_half) |>
              ud.convert("kg m^-2", "g cm^-2"), # in g m^-2 per day
            et_mm_day = ud.convert(et_g_cm_day, "cm", "mm"),
            # All CO2 fluxes are in mol CO2 m^-2 per day
            NEE = sum(nee_half) |>
              ud.convert("umol", "mol"),
            GPP = sum(gpp_half) |>
              ud.convert("umol", "mol"),
            GPP_DT = sum(gpp_dt_half) |>
              ud.convert("umol", "mol"),
            Reco = sum(reco_half) |>
              ud.convert("umol", "mol"),
            Reco_DT = sum(reco_dt_half) |>
              ud.convert("umol", "mol"),
            D_max = max(VPD_f),
            D_mean = mean(VPD_f),
            D_min = min(VPD_f),
            T_max = max(Tair_f),
            T_mean = mean(Tair_f),
            T_min = min(Tair_f),
            n = n()) 

flux_daily |>
  ggplot(aes(x = date)) +
  geom_line(aes(y = et_mm_day, col = "ET")) +
  geom_point(aes(y = GPP*15, col = "GPP")) +
  geom_point(aes(y = GPP_DT*15, col = "GPP_DT")) +
  scale_y_continuous("ET (mm/day)",
                     sec.axis = sec_axis(~./15,
                                         "GPP")) +
  theme_bw(base_size = 14)

write_csv(flux_daily, file = "data_clean/xSR_flux_daily.csv")
