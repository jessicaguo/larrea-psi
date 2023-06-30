# Explore site-level soil moisture

library(neonUtilities)
options(stringsAsFactors = FALSE)
library(tidyverse)


if(!dir.exists("data_neon")) {
  dir.create("data_neon")
}

# Download and stack 2023 soil moisture for SRER
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

# Average all of them for 501 - 503, except for 005.503
