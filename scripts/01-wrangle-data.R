library(googlesheets4)
library(googledrive)
library(tidyverse)
library(janitor)
library(here)

# Create function for negate %in%
`%nin%` = Negate(`%in%`)
# Create directories if does not already exist
if(!dir.exists("data_raw")) {
  dir.create("data_raw")
}

if(!dir.exists("data_appended")) {
  dir.create("data_appended")
}

##### Download manual data from Google Drive #####

manual <- read_sheet("https://docs.google.com/spreadsheets/d/1gVmWOpvJVHU81S7W8M3cM0stLKn6hsTKvb2_XSMpKMo/edit#gid=247691673",
                     sheet = "Predawns") %>%
  mutate(dt = Date + lubridate::hm(Time)) %>%
  clean_names() 

ggplot(manual) +
  geom_point(aes(x = dt, y = predawn_m_pa, color = factor(shrub_id)))

write_csv(manual, "data_clean/pressure_chamber.csv")

##### Download psychrometer csv's from Google Drive #####

# Name local local csv's
fn <- list.files("data_raw")

# Set url, get file names
folder_url <- "https://drive.google.com/drive/folders/1IBRsGNnpZErwqUVErc1ROD6FbxHtvvaG"
folder <- drive_get(as_id(folder_url))
all_files <- drive_ls(folder, type = "csv")

# Only download new files on Google Drive
csv_files <- all_files %>%
  filter(name %nin% fn)

# Download new files to data_raw folder
map2(.x = csv_files$id, .y = csv_files$name,
     ~ drive_download(as_id(.x), path = paste0("data_raw/", .y), overwrite = TRUE))

###### Append files by shrub/branch #####
fn <- list.files("data_raw")
branches <- map(fn, ~str_extract(.x, "\\d{1}[a-z]{1}")) %>%
  unlist() %>%
  unique()

for(i in 1:length(branches)) {
  files <- paste0("data_raw/", grep(branches[i], fn, value = TRUE))
  df <- map(files, ~read_csv(.x, 
                             skip = 15, 
                             locale=locale(encoding="latin1", tz = "America/Phoenix"),
                             col_types = cols(Date = col_date("%d/%m/%Y"),
                                              Time = col_time(format = "%H:%M:%S"),
                                              `Chamber Temperature (°C)` = col_double(),
                                              `dT (µV)` = col_double(),
                                              `Wet Bulb Depression (µV)` = col_double(),
                                              `Corrected Water Potential (MPa)` = col_double(),
                                              Intercept = col_double(),
                                              Slope = col_double(),
                                              EDBO = col_double(),
                                              `Correction for dT (MPa)` = col_double(),
                                              `Correction Factor` = col_double(),
                                              `Internal Battery Voltage (V)` = col_double(),
                                              `Internal Battery Temperature (°C)` = col_double(),
                                              `External Power Supply Present` = col_character(),
                                              `External Power Supply Voltage (V)` = col_double(),
                                              `External Power Supply Current (mA)` = col_double(),
                                              `Diagnostic Comment` = col_character()))) %>%
    list_rbind() %>%
    mutate(dt = ymd_hms(paste(Date, Time), tz = "America/Phoenix")) %>%
    clean_names()
  
  write_csv(df, file = paste0("data_appended/psy_", branches[i], ".csv"))
  
}


# Read in data to check
fn_appended <- list.files("data_appended")

dat_sum <- data.frame(filename = character(),
                      start = Date(),
                      end = Date(),
                      length_non_NA = numeric())

for(i in 1:length(fn_appended)) {
  dat <- read_csv(here("data_appended", fn_appended[i]))
  
  dat_sum[i,1] <- fn_appended[i]
  dat_sum[i,2] <- min(dat$date, na.rm = TRUE)
  dat_sum[i,3] <- max(dat$date, na.rm = TRUE)
  dat_sum[i,4] <- nrow(dat)
  
}

dat_sum <- dat_sum %>%
  mutate(branch = str_extract(filename, "\\d[a-z]"),
         shrub = str_extract(filename, "\\d"))

# Plot frequency of measurements
ggplot(dat_sum) +
  geom_errorbarh(aes(xmin = start, xmax = end,
                     y = branch,
                     color = shrub),
                 height = 0)
