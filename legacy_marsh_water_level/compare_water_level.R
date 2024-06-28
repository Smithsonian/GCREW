library(data.table)
library(tidyverse)

i <- list.files(path = "data", pattern = "dat", all.files = T, full.names = T, recursive = T,
                ignore.case = T)

i <- i[-27]

i13_15 <- i[grepl("2013", i) | grepl("2014", i) | grepl("2015", i)]
i16_22 <- i[i %in% i13_15 == F]

# -----------------------------------------------------------------------
# Read and concatenate data for 2013-2015
# Read in first data table
dat13_15 <- fread(i13_15[1], header = F)

# For the rest of the data
for(n in 2:length(i13_15)){
  # Read in data
  dat <- fread(i13_15[n], header = F)
  
  # Rbind
  dat13_15 <- rbind(dat13_15, dat)
}

# Remove columns 7 and 8
dat13_15 <- dat13_15[, -c(7,8)]

# Set column names
names(dat13_15) <- c("105", "year", "yday", "hour_minute", "cr21temp_avg", "airtemp_1_avg", "depth_ft_avg", "battv_avg")

# Get unique observations
dat13_15 <- unique(dat13_15)

# -----------------------------------------------------------------------
# Read and concatenate data for 2016-2022
dat16_22 <- fread(i16_22[1], header = F)

# For the rest of the data
for(k in 2:length(i16_22)){
  # Read in data
  dat <- fread(i16_22[k], header = F)
  
  # Rbind
  dat16_22 <- rbind(dat16_22, dat)
}

# Get unique observations
dat16_22 <- unique(dat16_22)

# Set column names
names(dat16_22) <- c("105", "year", "yday", "hour_minute", "cr21temp_avg", "airtemp_1_avg", "depth_ft_avg", "battv_avg")

# ---------------------------------------------------------------------
# Combine data
wl_dat <- rbind(dat13_15, dat16_22)
wl_dat <- unique(wl_dat)
wl_dat <- wl_dat[, -1]

# Create date column for data
wl_dat <- wl_dat %>%
  mutate(date = as.Date(yday, origin = paste0((year-1), "-12-31")))

# Create time column for data
wl_dat <- wl_dat %>%
  mutate(modified_hm = as.character(ifelse(nchar(hour_minute) == 1, paste0("000", hour_minute),
                              ifelse(nchar(hour_minute) == 2, paste0("00", hour_minute),
                              ifelse(nchar(hour_minute) == 3, paste0("0", hour_minute),
                                     hour_minute)))),
         time = as.POSIXct(modified_hm, format = "%H%M", tz = "Etc/GMT"),
         timestamp = paste0(date, substr(time, 11, 19)))

wl_dat$timestamp <- as.POSIXct(wl_dat$timestamp, format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT")

# Take time back 1 hour to align with water level data from LoggerNet
wl_dat$timestamp <- wl_dat$timestamp - 3600

# Convert ft to cm
wl_dat$depth_cm <- wl_dat$depth_ft_avg * 30.48
wl_dat <- wl_dat[wl_dat$depth_cm > 0,]
wl_dat <- wl_dat[wl_dat$timestamp > "2013-01-01",]

# Data before 2016-05-18 looks weird actually
# So...throw out 2013-2015 data
wl_dat <- wl_dat[wl_dat$timestamp > "2016-05-18",]

# Correct depth. Offset is based on C3LOG depth attribute from LoggerNet
wl_dat$corrected_depth_cm <- wl_dat$depth_cm - 58.5

# Select cols we want
wl_dat <- wl_dat %>%
  select(timestamp, date, year, cr21temp_avg, airtemp_1_avg, depth_ft_avg, depth_cm, corrected_depth_cm,
         battv_avg, yday, hour_minute)

write.table(wl_dat, "marsh_water_level_2016_2022.csv", append = FALSE, quote = FALSE, sep = ",",
            na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"))

# ---------------------------------------------------------------------
# Combine C3 waterlevel data for easy comparison in JMP

# path <- "C:/Users/ChengS1/Dropbox (Smithsonian)/GCREW_RESEARCHER_DATA/gcrew_waterlevel/yearly"
# 
# i <- list.files(path, pattern = "c4log", all.files = T, full.names = T, recursive = F,
#                 ignore.case = T)
# 
# c4log <- fread(i[1])
# 
# for(j in 2:length(i)){
#   dat <- fread(i[j])
#   
#   c4log <- rbind(c4log, dat, fill = T)
# }
# 
# c4log <- unique(c4log)
# 
# # Plot data
# wl_dat <- fread("marsh_water_level_2013_2022.csv")
# 
# ggplot() +
#   geom_point(data = c4dat, aes(x = time2, y = depth), colour = "red") + 
#   geom_point(data = wl_dat, aes(x = timestamp, y = corrected_depth_cm), colour = "blue")
#   
# # Join data? and save?
# wl_dat2 <- merge(c4log, wl_dat, by.x = "time2", by.y = "timestamp", all.x = T, all.y = T)
# 
# write.table(wl_dat2, "combined_water_level_TEST_C4LOG.csv", append = FALSE, quote = FALSE, sep = ",",
#             na = "NA", dec = ".", row.names = FALSE,
#             col.names = TRUE, qmethod = c("escape", "double"))

