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
                     sheet = "Predawns",
                     range = "A1:F120") %>%
  mutate(dt = Date + lubridate::hm(Time)) %>%
  clean_names() 

ggplot(manual, 
       aes(x = dt, y = predawn_m_pa, color = factor(shrub_id))) +
  geom_point() +
  geom_line() +
  facet_wrap(~shrub_id)

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
  
  # Grep the correct file names
  files <- paste0("data_raw/", grep(branches[i], fn, value = TRUE))
  
  # Slow method: allows for checking whether header is present or not (see previous versions for map method)
  comb <- data.frame()
  for(j in 1:length(files)) {
    
    # Testing for which kind of import
    skip_read <- read_csv(files[j], 
                    skip = 15, n_max = 3,
                    locale=locale(encoding="latin1", tz = "America/Phoenix"))
    
    ncol_read <- read_csv(files[j], n_max = 3,
                          locale=locale(encoding="latin1", tz = "America/Phoenix"),
                          col_names = FALSE) %>%
      select(where(function(x) any(!is.na(x))))

    if(colnames(skip_read)[1] == "Date" & ncol(skip_read) == 16) {
      # If Correction Factor not present
      df_temp <- read_csv(files[j], 
                          skip = 15, 
                          locale=locale(encoding="latin1", tz = "America/Phoenix"),
                          col_types = cols(Date = col_character(), # col_date("%d/%m/%Y")
                                           Time = col_time(format = "%H:%M:%S"),
                                           `Chamber Temperature (°C)` = col_double(),
                                           `dT (µV)` = col_double(),
                                           `Wet Bulb Depression (µV)` = col_double(),
                                           `Corrected Water Potential (MPa)` = col_double(),
                                           Intercept = col_double(),
                                           Slope = col_double(),
                                           EDBO = col_double(),
                                           `Correction for dT (MPa)` = col_double(),
                                           `Internal Battery Voltage (V)` = col_double(),
                                           `Internal Battery Temperature (°C)` = col_double(),
                                           `External Power Supply Present` = col_character(),
                                           `External Power Supply Voltage (V)` = col_double(),
                                           `External Power Supply Current (mA)` = col_double(),
                                           `Diagnostic Comment` = col_character())) %>%
        mutate(Date = lubridate::dmy(Date, tz = "America/Phoenix"),
               dt = ymd_hms(paste(Date, Time), tz = "America/Phoenix")) %>%
        select(-`Diagnostic Comment`)
    } else if (colnames(skip_read)[1] == "Date" & ncol(skip_read) == 17) {
      # If Correction Factor is present
      df_temp <- read_csv(files[j], 
                          skip = 15, 
                          locale=locale(encoding="latin1", tz = "America/Phoenix"),
                          col_types = cols(Date = col_character(), # col_date("%d/%m/%Y")
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
                                           `Diagnostic Comment` = col_character())) %>%
        mutate(Date = lubridate::dmy(Date, tz = "America/Phoenix"),
               dt = ymd_hms(paste(Date, Time), tz = "America/Phoenix")) %>%
        select(-`Diagnostic Comment`, -`Correction Factor`)
    } else if (colnames(skip_read)[1] != "Date" & ncol(ncol_read) == 16) {
      df_temp <- read_csv(files[j], 
                          locale=locale(encoding="latin1", tz = "America/Phoenix"),
                          col_names = FALSE,
                          col_types = "ctdddddddddddcdd") %>%
        rename(Date = 1,
               Time = 2,
               `Chamber Temperature (°C)` = 3,
               `dT (µV)` = 4,
               `Wet Bulb Depression (µV)` = 5,
               `Corrected Water Potential (MPa)` = 6,
               Intercept = 7,
               Slope = 8,
               EDBO = 9,
               `Correction for dT (MPa)` = 10,
               `Correction Factor` = 11,
               `Internal Battery Voltage (V)` = 12,
               `Internal Battery Temperature (°C)` = 13,
               `External Power Supply Present` = 14,
               `External Power Supply Voltage (V)` = 15,
               `External Power Supply Current (mA)` = 16) %>%
        mutate(Date = lubridate::dmy(Date, tz = "America/Phoenix"),
               dt = ymd_hms(paste(Date, Time), tz = "America/Phoenix")) %>%
        select(-`Correction Factor`)
    } else if (colnames(skip_read)[1] != "Date" & ncol(ncol_read) == 15) {
      df_temp <- read_csv(files[j], 
                          locale=locale(encoding="latin1", tz = "America/Phoenix"),
                          col_names = FALSE,
                          col_types = "ctddddddddddcdd") %>%
        rename(Date = 1,
               Time = 2,
               `Chamber Temperature (°C)` = 3,
               `dT (µV)` = 4,
               `Wet Bulb Depression (µV)` = 5,
               `Corrected Water Potential (MPa)` = 6,
               Intercept = 7,
               Slope = 8,
               EDBO = 9,
               `Correction for dT (MPa)` = 10,
               `Internal Battery Voltage (V)` = 11,
               `Internal Battery Temperature (°C)` = 12,
               `External Power Supply Present` = 13,
               `External Power Supply Voltage (V)` = 14,
               `External Power Supply Current (mA)` = 15) %>%
        mutate(Date = lubridate::dmy(Date, tz = "America/Phoenix"),
               dt = ymd_hms(paste(Date, Time), tz = "America/Phoenix"))
    }
    
    # Append
    comb <- bind_rows(comb, df_temp) 
      
  }
  
  df <- comb %>%
    janitor::clean_names(replace = janitor:::mu_to_u)
  write_csv(df, file = paste0("data_appended/psy_", branches[i], ".csv"))
  
}


# Read in data to check
fn_appended <- list.files("data_appended")

dat_sum <- data.frame(filename = character(),
                      start = Date(),
                      end = Date(),
                      length_non_NA = numeric())

for(i in 1:length(fn_appended)) {
  dat <- read_csv(here::here("data_appended", fn_appended[i]))
  
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


# Wrangle manual PV curve data
pv <- read_sheet("https://docs.google.com/spreadsheets/d/1oXb-js0mXpXzwDzQSmtrYZELwQvRPbcRv18v8q9qAcs/edit#gid=0",
                 sheet = "20230908") %>%
  mutate(shrub_id = factor(shrub_id))

ggplot(pv) +
  geom_point(aes(x = water_content, y = WP,
                 color = shrub_id))

write_csv(pv, file = paste0("data_clean/pv.csv"))
