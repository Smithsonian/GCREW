# Load packages
library(data.table)
library(lubridate)
library(tidyverse)

# Create directory file path
path <- file.path("C:", "Users", "ChengS1", "Dropbox (Smithsonian)", "DOE Warming Experiment", "Plot Water Level Data")

# C3 2016 ------------------------------------------------------------------
# Read in data
c3_2016 <- fread(file.path(path, "C3 (2016-2017).csv")) 

# Column names
names(c3_2016) <- c("timestamp", "elapsed_time_s", "pressure", "actual_conductivity", "specific_conductivity",
                    "depth", "depth_m", "temperature", "salinity", "water_level_m")

# ROUND TIMESTAMPS
# Set timestamp and make col for site
c3_2016$timestamp <- as.POSIXct(c3_2016$timestamp, format = "%m/%d/%Y %H:%M")

c3_2016$time2 <- round_date(c3_2016$timestamp,unit="15 minutes")

# Create new time variable
# minute_time <- round(minute(c3_2016$timestamp)/15)*15
# c3_2016$time2 <- update(c3_2016$timestamp, min = minute_time)
c3_2016$site <- "c3"

# select cols we want
c3_2016 <- c3_2016 %>%
  select(site, time2, everything())

# Create output directory
out_path <- file.path("C:", "Users", "ChengS1", "Dropbox (Smithsonian)", "GCREW_RESEARCHER_DATA",
                      "gcrew_waterlevel", "yearly")

filename <- "c3log_waterlevel_2017.csv"
  
final_path <- file.path(out_path, filename)

write.table(c3_2016, final_path, append = FALSE, quote = FALSE, sep = ",",
            na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"))

# time2
# actual_conductivity
# depth
# pressure
# resistivity
# salinity
# specific_conductivity
# tds
# temperature
# water_density

# C3 2018 ------------------------------------------------------------------
c3_2018 <- fread(file.path(path, "C3_2018_8-10.csv"))

# Column names
names(c3_2018) <- c("timestamp", "elapsed_time_s", "pressure", "temperature", "depth", "actual_conductivity", 
                    "specific_conductivity", "salinity", "tds", "resistivity", "water_density")

# Set timestamp and make col for site
c3_2018$timestamp <- as.POSIXct(c3_2018$timestamp, format = "%m/%d/%Y %H:%M")

# Create new time variable
minute_time <- round(minute(c3_2018$timestamp)/15)*15
c3_2018$time2 <- update(c3_2018$timestamp, min = minute_time)
c3_2018$site <- "c3"

# select cols we want
c3_2018 <- c3_2018 %>%
  select(site, time2, everything())

# Create output directory
out_path <- file.path("C:", "Users", "ChengS1", "Dropbox (Smithsonian)", "GCREW_RESEARCHER_DATA",
                      "gcrew_waterlevel", "yearly")

filename <- "c3log_waterlevel_preloggernet_2018.csv"

final_path <- file.path(out_path, filename)

write.table(c3_2018, final_path, append = FALSE, quote = FALSE, sep = ",",
            na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"))

# C4 2016 ------------------------------------------------------------------
c4_2016 <- fread(file.path(path, "C4.csv"))

# Column names
names(c4_2016) <- c("timestamp", "elapsed_time_s", "pressure", "actual_conductivity", "specific_conductivity",
                    "depth_ft", "depth_m", "temperature", "salinity", "water_level_m")

# Set timestamp and make col for site
# Set timestamp and make col for site
c4_2016$timestamp <- as.POSIXct(c4_2016$timestamp, format = "%m/%d/%Y %H:%M")
c4_2016$time2 <- round_date(c4_2016$timestamp,unit="15 minutes")

# Create new time variable
# minute_time <- round(minute(c4_2016$timestamp)/15)*15
# c4_2016$time2 <- update(c4_2016$timestamp, min = minute_time)
c4_2016$site <- "c4"
c4_2016$depth <- c4_2016$depth_ft * 30.48

# select cols we want
c4_2016 <- c4_2016 %>%
  select(site, time2, everything())

c4_2016 <- c4_2016[!is.na(c4_2016$depth_ft),]

# Create output directory
out_path <- file.path("C:", "Users", "ChengS1", "Dropbox (Smithsonian)", "GCREW_RESEARCHER_DATA",
                      "gcrew_waterlevel", "yearly")

filename <- "c4log_waterlevel_2017.csv"

final_path <- file.path(out_path, filename)

write.table(c4_2016, final_path, append = FALSE, quote = FALSE, sep = ",",
            na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"))

# C4 2018 ------------------------------------------------------------------
c4_2018 <- fread(file.path(path, "C4_2018_8-10.csv"))

# Column names
names(c4_2018) <- c("timestamp", "elapsed_time_s", "pressure", "temperature", "depth", "actual_conductivity", 
                    "specific_conductivity", "salinity", "tds", "resistivity", "water_density")

# Set timestamp and make col for site
c4_2018$timestamp <- as.POSIXct(c4_2018$timestamp, format = "%m/%d/%Y %H:%M")

# Create new time variable
minute_time <- round(minute(c4_2018$timestamp)/15)*15
c4_2018$time2 <- update(c4_2018$timestamp, min = minute_time)
c4_2018$site <- "c4"

# select cols we want
c4_2018 <- c4_2018 %>%
  select(site, time2, everything())

# Create output directory
out_path <- file.path("C:", "Users", "ChengS1", "Dropbox (Smithsonian)", "GCREW_RESEARCHER_DATA",
                      "gcrew_waterlevel", "yearly")

filename <- "c4log_waterlevel_preloggernet_2018.csv"

final_path <- file.path(out_path, filename)

write.table(c4_2018, final_path, append = FALSE, quote = FALSE, sep = ",",
            na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"))




