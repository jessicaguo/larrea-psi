# Convert, gapfill, and summarize to daily

# FC = fluxCo2/turb
# SC = fluxCo2/stor
# NEE = fluxCo2/nsae
# H = fluxTemp/turb
# SH = fluxTemp/stor
# LE = fluxH2o/turb
# SLE = fluxH2o/stor
# USTAR = fluxMome/turb

library(tidyverse)
library(REddyProc)
library(bigleaf)
library(udunits2)


out <- read_csv(file = "data_neon/stackedFiles/FLUX_rh_temp.csv",
                locale = locale(tz = "America/Phoenix"))

colnames(out)

# Prep half-hourly data for REddyProc
flux <- out |>
  select(timeBgn, timeEnd,
         contains("fluxH2o.turb"), # W m^-2
         contains("fluxCo2.nsae"), # umol m-2 s-1
         contains("fluxMome.turb"), 
         contains("fluxTemp.turb"), #  m^-2
         RHMean, tempRHMean, PARMean, inSWMean,
         RHFinalQF, tempRHFinalQF, PARFinalQF, inSWFinalQF) |>
  mutate(Year = lubridate::year(timeBgn),
         DoY = lubridate::yday(timeBgn),
         hour = lubridate::hour(timeBgn),
         min = lubridate::minute(timeBgn),
         Hour = case_when(min == 0 ~ hour + 0.5,
                          min == 30 ~ hour + 1),
         Ustar = if_else(qfqm.fluxMome.turb.qfFinl == 0,
                         data.fluxMome.turb.veloFric, NA),
         LE = if_else(qfqm.fluxH2o.turb.qfFinl == 0, 
                      data.fluxH2o.turb.flux, NA),
         NEE_temp = if_else(qfqm.fluxCo2.nsae.qfFinl == 0,
                       data.fluxCo2.nsae.flux, NA),
         NEE = if_else(NEE_temp < 80, NEE_temp, NA),
         H = if_else(qfqm.fluxTemp.turb.qfFinl == 0, 
                     data.fluxTemp.turb.flux, NA),
         RH = if_else(RHFinalQF == 0, 
                      RHMean, NA),
         Tair = if_else(tempRHFinalQF == 0,
                        tempRHMean, NA),
         PAR = if_else(PARFinalQF == 0, 
                       PARMean,  NA),
         Rg_temp = if_else(inSWFinalQF == 0, 
                      inSWMean, NA),
         Rg = if_else(Rg_temp <0, 0, Rg_temp),
         VPD = fCalcVPDfromRHandTair(RH, Tair)) |>
  select(-hour, -min, -Rg_temp, -NEE_temp) |>
  filter(DoY != 365)

# Convert from tibble to dataframe
EddyData <- data.frame(flux |>
                         select(Year:VPD))
str(EddyData)

write_csv(EddyData, "data_clean/xSR_needs_partitioning.csv")

#adding posix
flux_posix <- fConvertTimeToPosix(EddyData, 'YDH',Year = 'Year', Day = 'DoY', Hour = 'Hour',
                                  tz = "America/Phoenix")

# Initalize R5 reference class sEddyProc for post-processing of eddy data with the variables needed for post-processing later
# specify hourly (24) or half-hourly (48) data by assigning DTS
EProc <- sEddyProc$new('US-xSR', flux_posix, c('NEE','Rg','Tair','VPD', 'Ustar', 'LE', 'H'), DTS = 48)

# Ustar threshold estimation
EProc$sEstimateUstarScenarios(nSample = 100L, probs = c(0.05, 0.5, 0.95), UstarColName = "Ustar", NEEColName = "NEE",TempColName = "Tair", RgColName = "Rg")
EProc$sGetEstimatedUstarThresholdDistribution()
EProc$sGetUstarScenarios()

# Gapfilling for ustar thresholds NEE
EProc$sMDSGapFillUStarScens('NEE', RgColName = "Rg")
EProc$sMDSGapFillUStarScens('LE', RgColName = "Rg")


#setting location and gapfilling missing met data
#TimeZoneHour is the UTC offset (but already converted, so set to 0?)
EProc$sSetLocationInfo(LatDeg = 31.91068, LongDeg = -110.83549, TimeZoneHour = 0)  
EProc$sMDSGapFill('Tair', FillAll = FALSE,  minNWarnRunLength = NA)     
EProc$sMDSGapFill('VPD', FillAll = FALSE,  minNWarnRunLength = NA)    
EProc$sMDSGapFill('Rg', FillAll = FALSE,  minNWarnRunLength = NA)    

#partitioning with Reichstein method
#Note: you can do the Lasslop method too, but I've had trouble getting it to work across sites consistently. plus, I prefer this method
EProc$sMRFluxPartitionUStarScens()

#formatting data and writing to csv
FilledEddyData <- EProc$sExportResults()
CombinedData <- cbind(EddyData, FilledEddyData)
fWriteDataframeToFile(CombinedData, 'xSR_eddyproc.txt', Dir = "data_clean/")

# Read in gap-filled data
# Headers
eddy_gap_all_head <- read_tsv("data_clean/xSR_eddyproc.txt",
                         n_max = 0)
# Data (skipping units row)
eddy_gap_all <- read_tsv("data_clean/xSR_eddyproc.txt",
                         skip = 1)
# Combine column names
colnames(eddy_gap_all) <- colnames(eddy_gap_all_head)
# Convert -9999 to NA
eddy_gap_all <- eddy_gap_all |>
  mutate(across(Ustar:H, ~ifelse(.x == -9999, NA, .x)))
# Check for NA's, structure, column names
sum(is.na(eddy_gap_all$LE))
str(eddy_gap_all)
colnames(eddy_gap_all)
# LE_U50_f
# NEE_U50_f
# GPP_U50_f
# Reco_U50_f

# Select relevant columns only
# Add date and dt back in POSIX
eddy_gap <- eddy_gap_all |>
  select(Year:VPD, LE_U50_f, NEE_U50_f, GPP_U50_f, Reco_U50, Ustar_U50_Thres) |>
  mutate(date = parse_date_time(x = paste(Year, DoY), orders = "yj"),
         hr = floor(Hour),
         min = Hour%%1*60,
         dt = parse_date_time(x = paste(Year, DoY, hr, ":", min), orders = "yjHM") - 30*60)


  
# Plot original + gapfilled LE
# LE gapfilled with Ustar filtering and a lookup table with Rg
eddy_gap |>
  ggplot(aes(x = date)) +
  geom_point(aes(y = LE_U50_f, color = "gapfilled"),
             alpha = 0.5) +
  geom_point(aes(y = LE, color = "orig"),
             alpha = 0.1) +
  scale_y_continuous(expression(paste("LE (W ", m^-2, ")"))) +
  theme_bw(base_size = 14)

# Convert to ET and sum to daily

et_flux <- eddy_gap |>
  mutate(ET_f = LE.to.ET(LE_U50_f, Tair), # kg m^-2 s^-1
         ET = LE.to.ET(LE, Tair))
# Plot 
et_flux |>
  ggplot(aes(x = date)) +
  geom_point(aes(y = ET_f, color = "gapfilled"),
             alpha = 0.5) +
  geom_point(aes(y = ET, color = "orig"),
             alpha = 0.1) +
  scale_y_continuous(expression(paste("ET (kg ", m^-2, " ", s^-1, ")"))) +
  theme_bw(base_size = 14)

# Sum to daily

et_daily <- et_flux |>
  mutate(et_half = ET_f *30 * 60) |> # multiply to get kg m-2 per 30 mins
  group_by(date) |>
  summarize(et_g_cm_day = sum(et_half) |>
              ud.convert("kg m^-2", "g cm^-2"), # in g m^-2 per day
            et_mm_day = ud.convert(et_g_cm_day, "cm", "mm"),
            et_n = n()) 


ggplot(et_daily) +
  geom_point(aes(x = date, y = et_mm_day))

write_csv(et_daily, file = "data_clean/xSR_et_daily.csv")
