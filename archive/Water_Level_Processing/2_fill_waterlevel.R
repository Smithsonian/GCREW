# Written by Selina Cheng
# Last modified April 03, 2023
# --------------------------------------------------------------
# Load some packages
library(data.table)
library(tidyverse)

# Set waterlevel directory
waterlevel_dir <- file.path("C:", "Users", Sys.getenv("USERNAME"), "Dropbox (Smithsonian)", "GCREW_RESEARCHER_DATA", 
                     "gcrew_waterlevel", "combined")

# Get files
i <-list.files(waterlevel_dir, pattern = "combined_WIDE", all.files = FALSE,
               full.names = TRUE, recursive = F,
               ignore.case = FALSE, include.dirs = F)

#==================================================================================
# 2021 is the fullest source of data for C3, C4, GCREW MET, GENX, and C3 float gauge, so that's the year I'm going to use.
# I am going to do directional pairwise linear regressions between the corrected_depth variable at all sites.
# The intercept of the linear model will be used as the offset.
# Read in 2021 data
year <- "2023"
dat_test <- fread(i[grepl(year, i)])

# C3, C4, Met, GENX, float
# Linear regression: lm(y~x)

# Create an empty matrix where rows = "X" and columns = "Y"
# How does X predict Y? What offset do we need to get there?
offsets <- matrix(data = NA, nrow = 4, ncol = 4)
colnames(offsets) <- c("C3", "GCREW_MET", "GENX", "C4")
rownames(offsets) <- colnames(offsets)

# --------------------------------------------------------------
# Use C3 to predict GCREW MET
plot(corrected_depth.gcrew_met ~ corrected_depth.c3, data = dat_test)
mod <- lm(corrected_depth.gcrew_met ~ corrected_depth.c3, data = dat_test)
mod_summary <- summary(mod)
offsets["C3", "GCREW_MET"] <- mod_summary$coefficients[1,1]

# Use C3 to predict GENX
plot(corrected_depth.genx ~ corrected_depth.c3, data = dat_test)
mod <- lm(corrected_depth.genx ~ corrected_depth.c3, data = dat_test)
mod_summary <- summary(mod)
offsets["C3", "GENX"] <- mod_summary$coefficients[1,1]

# Use C3 to predict C4
plot(corrected_depth.c4 ~ corrected_depth.c3, data = dat_test)
mod <- lm(corrected_depth.c4 ~ corrected_depth.c3, data = dat_test)
mod_summary <- summary(mod)
offsets["C3", "C4"] <- mod_summary$coefficients[1,1]

# --------------------------------------------------------------
# Use GCREW MET to predict C3
plot(corrected_depth.c3 ~ corrected_depth.gcrew_met, data = dat_test)
mod <- lm(corrected_depth.c3 ~ corrected_depth.gcrew_met, data = dat_test)
mod_summary <- summary(mod)
offsets["GCREW_MET", "C3"] <- mod_summary$coefficients[1,1]

# Use GCREW MET to predict GENX
plot(corrected_depth.genx ~ corrected_depth.gcrew_met, data = dat_test)
mod <- lm(corrected_depth.genx ~ corrected_depth.gcrew_met, data = dat_test)
mod_summary <- summary(mod)
offsets["GCREW_MET", "GENX"] <- mod_summary$coefficients[1,1]

# Use GCREW MET to predict C4
plot(corrected_depth.c4 ~ corrected_depth.gcrew_met, data = dat_test)
mod <- lm(corrected_depth.c4 ~ corrected_depth.gcrew_met, data = dat_test)
mod_summary <- summary(mod)
offsets["GCREW_MET", "C4"] <- mod_summary$coefficients[1,1]

# --------------------------------------------------------------
# Use GENX to predict C3
plot(corrected_depth.c3 ~ corrected_depth.genx, data = dat_test)
mod <- lm(corrected_depth.c3 ~ corrected_depth.genx, data = dat_test)
mod_summary <- summary(mod)
offsets["GENX", "C3"] <- mod_summary$coefficients[1,1]

# Use GENX to predict GCREW MET
plot(corrected_depth.gcrew_met ~ corrected_depth.genx, data = dat_test)
mod <- lm(corrected_depth.gcrew_met ~ corrected_depth.genx, data = dat_test)
mod_summary <- summary(mod)
offsets["GENX", "GCREW_MET"] <- mod_summary$coefficients[1,1]

# Use GENX to predict C4
plot(corrected_depth.c4 ~ corrected_depth.genx, data = dat_test)
mod <- lm(corrected_depth.c4 ~ corrected_depth.genx, data = dat_test)
mod_summary <- summary(mod)
offsets["GENX", "C4"] <- mod_summary$coefficients[1,1]

# --------------------------------------------------------------
# Use C4 to predict C3
plot(corrected_depth.c3 ~ corrected_depth.c4, data = dat_test)
mod <- lm(corrected_depth.c3 ~ corrected_depth.c4, data = dat_test)
mod_summary <- summary(mod)
offsets["C4", "C3"] <- mod_summary$coefficients[1,1]

# Use C4 to predict GCREW MET 
plot(corrected_depth.gcrew_met ~ corrected_depth.c4, data = dat_test)
mod <- lm(corrected_depth.gcrew_met ~ corrected_depth.c4, data = dat_test)
mod_summary <- summary(mod)
offsets["C4", "GCREW_MET"] <- mod_summary$coefficients[1,1]

# Use C4 to predict GENX
plot(corrected_depth.genx ~ corrected_depth.c4, data = dat_test)
mod <- lm(corrected_depth.genx ~ corrected_depth.c4, data = dat_test)
mod_summary <- summary(mod)
offsets["C4", "GENX"] <- mod_summary$coefficients[1,1]

# Just fill out the diagonal with 0s
offsets[1,1] <- 0
offsets[2,2] <- 0
offsets[3,3] <- 0
offsets[4,4] <- 0

# Save matrix
# write.csv(offsets, file = "design documents/offset_matrix_fill_waterlevel.csv")
#==================================================================================
# For years before 2021:
# C3: Fill with C4, then float gauge
# C4: Fill with C3, then float gauge

for(n in 1:(grep("2021", i)-1)){
  # Read in data
  dt <- fread(i[n])
  
  dt2 <- dt
  # First fill C3 with C4
  # If C3 depth is NA, check if C4 depth is NOT NA. If it's not NA, fill depth with C4 + 11 CM. 
  # If it is NA, check that C3 float gauge is NOT NA. If it's not NA, fill depth with C3 float gauge.
  dt2$filled_depth.c3 <- ifelse(is.na(dt2$corrected_depth.c3), 
                                   ifelse(!is.na(dt2$corrected_depth.c4), (dt2$corrected_depth.c4 + offsets["C4", "C3"]),
                                          ifelse(!is.na(dt2$corrected_depth.c3_floatgauge), dt2$corrected_depth.c3_floatgauge,
                                                 dt2$corrected_depth.c3)), 
                                   dt2$corrected_depth.c3)
  
  # Check
  check <- data.frame(time2 = dt2$time2, c3_first = dt$corrected_depth.c3, c3_fixed = dt2$filled_depth.c3, 
                      float = dt$corrected_depth.c3_floatgauge, c4 = dt$corrected_depth.c4)
  
  # Now fill C4 with C3
  dt2$filled_depth.c4 <- ifelse(is.na(dt2$corrected_depth.c4),
                                   ifelse(!is.na(dt2$corrected_depth.c3), (dt2$corrected_depth.c3 + offsets["C3", "C4"]),
                                          ifelse(!is.na(dt2$corrected_depth.c3_floatgauge), (dt2$corrected_depth.c3_floatgauge + offsets["C3", "C4"]),
                                                 dt2$corrected_depth.c4)), 
                                   dt2$corrected_depth.c4)
  
  # Check
  check <- data.frame(time2 = dt2$time2, c4_first = dt$corrected_depth.c4, c4_fixed = dt2$filled_depth.c4, 
                      float = dt$corrected_depth.c3_floatgauge, c3 = dt$corrected_depth.c3)
  
  # Save as wide form first
  # Create output directory
  filename <- paste0("waterlevel_combined_filled_wide_", year(dt2$time2[nrow(dt2)]), ".csv")
  final_path <- file.path(waterlevel_dir, filename)
  
  # Save as long form also
  write.table(dt2, final_path, append = FALSE, quote = FALSE, sep = ",",
              na = "NA", dec = ".", row.names = FALSE,
              col.names = TRUE, qmethod = c("escape", "double"))
}  

# For 2021 and after:
# C3: Fill with GCREW MET, GENX, C4, float gauge
# GCREW_MET: Fill with C3, C4, GENX, float gauge
# GENX: Fill with C4, GCREW MET, C3, float gauge
# C4: Fill with GENX, GCREW_MET, C3, float gauge

offsets <- read.csv("design documents/offset_matrix_fill_waterlevel.csv")
rownames(offsets) <- offsets$X
offsets <- offsets[, 2:5]

for(n in 1:length(i)){
  # Read in data
  dt <- fread(i[n])
  
  dt2 <- dt
  # First fill C3 with C4
  # C3: Fill with GCREW MET, GENX, C4, float gauge
  dt2$filled_depth.c3 <- ifelse(is.na(dt2$corrected_depth.c3), 
                                ifelse(!is.na(dt2$corrected_depth.gcrew_met), (dt2$corrected_depth.gcrew_met + offsets["GCREW_MET", "C3"]),
                                       ifelse(!is.na(dt2$corrected_depth.genx), (dt2$corrected_depth.genx + offsets["GENX", "C3"]),
                                              ifelse(!is.na(dt2$corrected_depth.c4), (dt2$corrected_depth.c4 + offsets["C4", "C3"]),
                                                     ifelse(!is.na(dt2$corrected_depth.c3_floatgauge), dt2$corrected_depth.c3_floatgauge,
                                                            dt2$corrected_depth.c3)))),
                                dt2$corrected_depth.c3)
  
  # GCREW_MET: Fill with C3, C4, GENX, float gauge
  dt2$filled_depth.gcrew_met <- ifelse(is.na(dt2$corrected_depth.gcrew_met), 
                                ifelse(!is.na(dt2$corrected_depth.c3), (dt2$corrected_depth.c3 + offsets["C3", "GCREW_MET"]),
                                       ifelse(!is.na(dt2$corrected_depth.c4), (dt2$corrected_depth.c4 + offsets["C4", "GCREW_MET"]),
                                              ifelse(!is.na(dt2$corrected_depth.genx), (dt2$corrected_depth.genx + offsets["GENX", "GCREW_MET"]),
                                                     ifelse(!is.na(dt2$corrected_depth.c3_floatgauge), (dt2$corrected_depth.c3_floatgauge + offsets["C3", "GCREW_MET"]),
                                                            dt2$corrected_depth.gcrew_met)))),
                                dt2$corrected_depth.gcrew_met)
  
  # GENX: Fill with C4, GCREW MET, C3, float gauge
  # GENX: For 2023, filled with GCREW MET, then C4, then C3, then float gauge
  dt2$filled_depth.genx <- ifelse(is.na(dt2$corrected_depth.genx), 
                                  ifelse(!is.na(dt2$corrected_depth.gcrew_met), (dt2$corrected_depth.gcrew_met + offsets["GCREW_MET", "GENX"]),
                                       ifelse(!is.na(dt2$corrected_depth.c4), (dt2$corrected_depth.c4 + offsets["C4", "GENX"]),
                                                     ifelse(!is.na(dt2$corrected_depth.c3), (dt2$corrected_depth.c3 + offsets["C3", "GENX"]),
                                                            ifelse(!is.na(dt2$corrected_depth.c3_floatgauge), (dt2$corrected_depth.c3_floatgauge + offsets["C3", "GENX"]),
                                                                   dt2$corrected_depth.genx)))),
                                       dt2$corrected_depth.genx)
  
  # C4: Fill with GENX, GCREW_MET, C3, float gauge
  dt2$filled_depth.c4 <- ifelse(is.na(dt2$corrected_depth.c4), 
                                  ifelse(!is.na(dt2$corrected_depth.genx), (dt2$corrected_depth.genx + offsets["GENX", "C4"]),
                                         ifelse(!is.na(dt2$corrected_depth.gcrew_met), (dt2$corrected_depth.gcrew_met + offsets["GCREW_MET", "C4"]),
                                                ifelse(!is.na(dt2$corrected_depth.c3), (dt2$corrected_depth.c3 + offsets["C3", "C4"]),
                                                       ifelse(!is.na(dt2$corrected_depth.c3_floatgauge), (dt2$corrected_depth.c3_floatgauge + offsets["C3", "C4"]),
                                                              dt2$corrected_depth.c4)))),
                                  dt2$corrected_depth.c4)
  
  # Save as wide form first
  # Create output directory
  filename <- paste0("waterlevel_combined_filled_wide_", year(dt2$time2[nrow(dt2)]), ".csv")
  final_path <- file.path(waterlevel_dir, filename)
  
  # Format timestamp nicely for midnight tzs
  dt2$time2 <- format(as.POSIXct(dt2$time2, tz="UTC"), format="%Y-%m-%d %H:%M:%S")
  
  # Save as long form also
  write.table(dt2, final_path, append = FALSE, quote = FALSE, sep = ",",
              na = "NA", dec = ".", row.names = FALSE,
              col.names = TRUE, qmethod = c("escape", "double"))
}  
  
year <- "2023"
# Create long form dataset
# Get files
i <-list.files(waterlevel_dir, pattern = paste0("filled_wide_", year), all.files = FALSE,
               full.names = TRUE, recursive = F,
               ignore.case = FALSE, include.dirs = F)
   
for(n in 1:length(i)){
  dt <- fread(i[n])
  # Bring into long form
  dt_long <- melt(dt, id.vars = c("time2"), variable.name = "colnames", value.name = "value", na.rm = T)
  
  dt_long$colnames <- as.character(dt_long$colnames)
  names <- strsplit(dt_long$colnames, ".", fixed = T)
  newnames <- sapply(names, "[[", 1)
  sites <- sapply(names, "[[", 2)
  
  # Create new colnames and site
  dt_long$newname <- newnames
  dt_long$site <- sites
  
  dt_long <- dt_long[, c(1, 3, 4, 5)]

  # Now make it a little wider
  dt_wide <- dcast(dt_long, time2+site ~ newname, subset = NULL, drop = TRUE, value.var = "value")
  
  # Create output directory
  filename <- paste0("waterlevel_combined_filled_long_", year(dt_wide$time2[nrow(dt_wide)]), ".csv")
  final_path <- file.path(waterlevel_dir, filename)
  
  # Format timestamp nicely for midnight tzs
  dt_wide$time2 <- format(as.POSIXct(dt_wide$time2, tz="UTC"), format="%Y-%m-%d %H:%M:%S")
  
  # Save as long form also
  write.table(dt_wide, final_path, append = FALSE, quote = FALSE, sep = ",",
              na = "NA", dec = ".", row.names = FALSE,
              col.names = TRUE, qmethod = c("escape", "double"))
}
