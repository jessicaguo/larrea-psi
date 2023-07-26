# Explore site-level soil moisture

library(neonUtilities)
options(stringsAsFactors = FALSE)
library(tidyverse)


if(!dir.exists("data_neon")) {
  dir.create("data_neon")
}

# Download and stack 2023 soil moisture for SRER
# Only run once, or update when new data is available
zipsByProduct(dpID = "DP1.00094.001", 
                       site = c("SRER"),
                       startdate = "2023-01",
                       package = "basic", check.size = TRUE)
stackByTable(filepath = "filesToStack00094/",
             savepath = "data_neon",
             saveUnzippedFiles = FALSE)
unlink("filesToStack00094")

# Load 30 minute data

swc30 <- read_csv("data_neon/stackedFiles/SWS_30_minute.csv")
unique(swc30$siteID)
table(swc30$horizontalPosition, swc30$verticalPosition)

# VSWCFinalQF overall quality flag, 1=fail, 2=pass
# VSWCFinalQFSciRvw
# 2=fail and data redacted, 
# 1=fail but data retained and may be useful with additional corrections or filtering
# 0=previous quality concern resolved and final quality flag correctly reflects data quality

sum_swc30 <- swc30 %>%
  group_by(horizontalPosition,
           verticalPosition) %>%
  summarize(fail = length(which(VSWCFinalQF == 1)),
            pass = length(which(VSWCFinalQF == 0)),
            total = n()) %>%
  mutate(perc_fail = fail/total*100)

sum_swc30 %>% 
  filter(verticalPosition %in% 501:504) %>%
  ggplot(aes(x = perc_fail,
             fill = as.factor(verticalPosition))) +
  stat_density(alpha = 0.25)


# Filter for VSWCFinalQF == 0 and plot the same depth together
swc30 %>%
  filter(VSWCFinalQF == 0,
         verticalPosition == 501) %>%
  ggplot(aes(x = startDateTime,
             y = VSWCMean,
             color = horizontalPosition)) +
  geom_point()

swc30 %>%
  filter(VSWCFinalQF == 0,
         verticalPosition == 502) %>%
  ggplot(aes(x = startDateTime,
             y = VSWCMean,
             color = horizontalPosition)) +
  geom_point()

swc30 %>%
  filter(VSWCFinalQF == 0,
         verticalPosition == 503) %>%
  ggplot(aes(x = startDateTime,
             y = VSWCMean,
             color = horizontalPosition)) +
  geom_point()
# 005.503 is not working

swc30 %>%
  filter(VSWCFinalQF == 0,
         verticalPosition == 504) %>%
  ggplot(aes(x = startDateTime,
             y = VSWCMean,
             color = horizontalPosition)) +
  geom_point()

# Take mean of plots 1/2 and 3/4, for top 4 layers
# Keep only if 2 or more points included in mean
# plots 1/2 have missing 06 data, but more infiltration measured at 56
# plots 3/4 have more complete 06-26 data, but no measurements at 56

swc_12 <- swc30 %>%
  filter(VSWCFinalQF == 0,
         verticalPosition %in% c(501, 502, 503, 504),
         horizontalPosition %in% c("001", "002")) %>%
  filter(!is.na(VSWCMean)) %>%
  mutate(depth = case_when(verticalPosition == 501 ~ 6,
                           verticalPosition == 502 ~ 16,
                           verticalPosition == 503 ~ 26,
                           verticalPosition == 504 ~ 56)) %>%
  group_by(depth,
           startDateTime) %>%
  summarize(swc_mean = mean(VSWCMean),
            swc_n = n()) %>%
  filter(swc_n > 1) %>%
  pivot_wider(names_from = depth, 
              values_from = swc_mean,
              names_prefix = "p12_")

swc_34 <- swc30 %>%
  filter(VSWCFinalQF == 0,
         verticalPosition %in% c(501, 502, 503),
         horizontalPosition %in% c("003", "004")) %>%
  filter(!is.na(VSWCMean)) %>%
  mutate(depth = case_when(verticalPosition == 501 ~ 6,
                           verticalPosition == 502 ~ 16,
                           verticalPosition == 503 ~ 26)) %>%
  group_by(depth,
           startDateTime) %>%
  summarize(swc_mean = mean(VSWCMean),
            swc_n = n()) %>%
  filter(swc_n > 1) %>%
  pivot_wider(names_from = depth, 
              values_from = swc_mean,
              names_prefix = "p34_")
  
# Check plots
swc_12 %>%  
  ggplot() +
  geom_point(aes(x = startDateTime,
                 y = p12_6,
                 color = "06 cm")) +
  geom_point(aes(x = startDateTime,
                 y = p12_16, 
                 color = "16 cm")) +
  geom_point(aes(x = startDateTime,
                 y = p12_26, 
                 color = "26 cm")) +
  geom_point(aes(x = startDateTime,
                 y = p12_56, 
                 color = "56 cm"))

  
  
swc_34 %>%  
  ggplot() +
  geom_point(aes(x = startDateTime,
                 y = p34_6,
                 color = "06 cm")) +
  geom_point(aes(x = startDateTime,
                 y = p34_16, 
                 color = "16 cm")) +
  geom_point(aes(x = startDateTime,
                 y = p34_26, 
                 color = "26 cm")) 


# combine and output
swc_all <- full_join(select(swc_12, -swc_n), select(swc_34, -swc_n), by = join_by(startDateTime))

write_csv(swc_all, "data_clean/neon_swc30.csv")

# Average to daily values

swc30 <- read_csv("data_clean/neon_swc30.csv",
                  locale = locale(tz = "America/Phoenix")) %>%
  mutate(date = as.Date(startDateTime, tz = "America/Phoenix"))

# attr(swc30$startDateTime, "tzone")
# attr(swc30$date, "tzone")
# 
# swc30 %>%
#   filter(date >= as.Date("2023-05-01", tz = "America/Phoenix"),
#          date <= as.Date("2023-05-03", tz = "America/Phoenix")) %>%
#   ggplot(aes(x = startDateTime, y = p12_16)) +
#   geom_point()

# Summarize to dily means, sd, and n
# But only keep data from days with 10 or more observations

swc_daily <- swc30 %>%
  pivot_longer(cols = c(-1, -9), 
               names_to = "depth",
               values_to = "swc") %>%
  group_by(date, depth) %>%
  summarize(m = mean(swc, na.rm = TRUE),
            sd = sd(swc, na.rm = TRUE),
            n = sum(!is.na(swc))) %>%
  filter(n >= 10) %>%
  pivot_wider(names_from = depth,
              names_sep = "_",
              values_from = c(m, sd, n))

write_csv(swc_daily, "data_clean/neon_swcdaily.csv")
