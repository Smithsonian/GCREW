#### Step 4 -- Add the Corrected_Depth Column to Annual Combined Water Level Dataset #### 
#Description -- 
#Add the accurate corrected_depth column to the yearly combined waterlevel dataset

#Written by Selina Cheng

####------------------Required User Input-------------------------------------#####
#We only want to do this on yearly files that have ALL of their loggernet data.
#enter in the vector below the year you want to combine

year <- "2025"

# Load  packages
library(data.table)
library(tidyverse)

#------------------- End Required user input --------------------------------------#

#### Load Functions and Directories ####
#relevant directories
L0_CombinedYearly_dir <- paste0(Sys.getenv("dropbox_filepath") , "Taylor_Projects/WaterLevelWorkflow_TEST/2_L0_NormalizedData/processed/combined/")
offset_dir <- paste0(Sys.getenv("dropbox_filepath") , "GCREW_LOGGERNET_WORKFLOW/design documents/")

# Get files
i <-list.files(L0_CombinedYearly_dir, pattern = paste0("combined_WIDE_", year, "_final"), all.files = FALSE,
               full.names = TRUE, recursive = F,
               ignore.case = FALSE, include.dirs = F)

#==================================================================================
# 2021 is the fullest source of data for C3, C4, GCREW MET, GENX, and C3 float gauge, so that's the year I'm going to use.
# I am going to do directional pairwise linear regressions between the corrected_depth variable at all sites.
# The intercept of the linear model will be used as the offset.
# Read in 2021 data
yearmatrix <- "2024"
dat_test <- fread(i[grepl(yearmatrix, i)])

#C3, C4, Met, GENX, float
#Linear regression: lm(y~x)

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
write.csv(offsets, paste0(offset_dir, "offset_matrix_fill_waterlevel_", year, ".csv"))

#==================================================================================

# For 2021 and after:
# C3: Fill with GCREW MET, GENX, C4, float gauge
# GCREW_MET: Fill with C3, C4, GENX, float gauge
# GENX: Fill with C4, GCREW MET, C3, float gauge
# C4: Fill with GENX, GCREW_MET, C3, float gauge

offsets <- read.csv(paste0(offset_dir, "offset_matrix_fill_waterlevel_", yearmatrix, ".csv"))
rownames(offsets) <- offsets$X
offsets <- offsets[, 2:5]

for(n in 1:length(i)){
  # Read in data
  # dt <- fread(i[n])
  dt <- fread(i[grepl(year, i)])
  
  dt2 <- dt

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
  dt2$filled_depth.genx <- dt2$corrected_depth.genx
  dt2$filled_depth.genx[1:20173] <- ifelse(is.na(dt2$corrected_depth.genx[1:20173]), 
                                  ifelse(!is.na(dt2$corrected_depth.gcrew_met[1:20173]), (dt2$corrected_depth.gcrew_met[1:20173] + offsets["GCREW_MET", "GENX"]),
                                         ifelse(!is.na(dt2$corrected_depth.c4[1:20173]), (dt2$corrected_depth.c4[1:20173] + offsets["C4", "GENX"]),
                                                ifelse(!is.na(dt2$corrected_depth.c3[1:20173]), (dt2$corrected_depth.c3[1:20173] + offsets["C3", "GENX"]),
                                                       ifelse(!is.na(dt2$corrected_depth.c3_floatgauge[1:20173]), (dt2$corrected_depth.c3_floatgauge[1:20173] + offsets["C3", "GENX"]),
                                                              dt2$corrected_depth.genx[1:20173])))),
                                  dt2$corrected_depth.genx[1:20173])
  dt2$filled_depth.genx[20174:nrow(dt2)] <- NA
  
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
  filename <- paste0("waterlevel_combined_filled_wide_adjgenx_", year, ".csv")
  final_path <- file.path(L0_CombinedYearly_dir, filename)
  
  # Format timestamp nicely for midnight tzs
  dt2$time2 <- format(as.character(dt2$time2))
  
  # Save as long form also
  write.table(dt2, final_path, append = FALSE, quote = FALSE, sep = ",",
              na = "NA", dec = ".", row.names = FALSE,
              col.names = TRUE, qmethod = c("escape", "double"))
}  

# Create long form dataset
# Get files
i <-list.files(L0_CombinedYearly_dir, pattern = paste0("filled_wide_", year), all.files = FALSE,
               full.names = TRUE, recursive = F,
               ignore.case = FALSE, include.dirs = F)

for(n in 1:length(i)){
  # dt <- fread(i[n])
  dt <- fread(i[grepl(year, i)])
  
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
  filename <- paste0("waterlevel_combined_filled_long_", year, ".csv")
  final_path <- file.path(L0_CombinedYearly_dir, filename)
  
  # Format timestamp nicely for midnight tzs
  dt_wide$time2 <- format(as.character(dt_wide$time2))
  
  # Save as long form also
  write.table(dt_wide, final_path, append = FALSE, quote = FALSE, sep = ",",
              na = "NA", dec = ".", row.names = FALSE,
              col.names = TRUE, qmethod = c("escape", "double"))
}
