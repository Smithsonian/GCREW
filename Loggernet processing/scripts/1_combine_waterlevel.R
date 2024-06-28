# Written by Selina Cheng
# Last modified April 03, 2023
# Load packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(reshape2, lubridate, data.table, tools, plyr, tidyverse)

# Combine waterlevel tables and create wide format data table
waterlevel_dir <- file.path("C:", "Users", Sys.getenv("USERNAME"), "Dropbox (Smithsonian)", "GCREW_RESEARCHER_DATA", 
                            "gcrew_waterlevel", "yearly")

  # Get files
  i <-list.files(waterlevel_dir, pattern = NULL, all.files = FALSE,
                 full.names = TRUE, recursive = F,
                 ignore.case = FALSE, include.dirs = F)

  # Get years from filename
  years <- strsplit(i, split = "_")
  years <- sapply(years, "[[", length(years[[2]]))
  years <- unique(substr(years, 1, 4))

# ------------------------------------------------------------------------------------------
# Combine 2017 and 2018 by hand

# 2017
year <- 2017
group <- i[grepl(year, i)]

# Read in data
c3log <- fread(group[grepl("c3log", group)])
c4log <- fread(group[grepl("c4log", group)])
float <- fread(group[grepl("marsh", group)])

# rbind
dat <- rbind.fill(c3log, c4log)
dat <- rbind.fill(dat, float)

# Give offset column
dat$offset <- ifelse(dat$site == "c3", 45, 
                    ifelse(dat$site == "c4", 65,
                           ifelse(dat$site == "c3_floatgauge", 45, NA)))

# Add corrected depth column
dat$corrected_depth <- dat$depth - dat$offset

# Subset to only useful columns
keep <- c("site", "time2", "offset", "depth", "corrected_depth", "actual_conductivity", "specific_conductivity", 
          "pressure", "salinity", "temperature")

dat_2 <- subset(dat, select = keep)
setDT(dat_2)

# Order dt2 by time2
dat_2 <- dat_2[order(time2)]

# NOW THE DATA IS IN LONG FORM. THIS IS NICE....but Roy wants it in wide form
# f <- as.formula(paste("time2 ~ ", paste(names(dat_2)[3:13], collapse = " + ")))
# 
# dat_wide <- dcast(dat_2, f, subset = NULL, drop = TRUE, value.var = "site")
# Make it even longer
dat_long <- melt(dat_2, id.vars = c("time2", "site"), variable.name = "colnames", value.name = "value", na.rm = T)
dat_long$newname <- paste0(dat_long$colnames, ".", dat_long$site)

dat_long <- dat_long[,c(1, 4, 5)]

dat_long <- unique(dat_long)

# Now make it wide
dat_wide <- dcast(dat_long, time2 ~ newname, fun = mean, subset = NULL, drop = TRUE, value.var = "value")

# Save wide format
out_dir <- file.path("C:", "Users", Sys.getenv("USERNAME"), "Dropbox (Smithsonian)", "GCREW_RESEARCHER_DATA", 
                     "gcrew_waterlevel", "combined")
filename <- paste0("waterlevel_combined_WIDE_", year, ".csv")
out_path <- file.path(out_dir, filename)

write.table(dat_wide, out_path, append = FALSE, quote = FALSE, sep = ",",
            na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"))

# Save long format
filename <- paste0("waterlevel_combined_LONG_", year, ".csv")
out_path <- file.path(out_dir, filename)

write.table(dat_2, out_path, append = FALSE, quote = FALSE, sep = ",",
            na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"))

# ------------------------------------------------------------------------------------------
# 2018
year <- 2018
group <- i[grepl(year, i)]

# Read in data
c3log <- fread(group[grepl("c3log", group)])
c4log <- fread(group[grepl("c4log", group)])
float <- fread(group[grepl("marsh", group)])

# rbind
dat <- rbind.fill(c3log, c4log)
dat <- rbind.fill(dat, float)

# Give offset column
dat$offset <- ifelse(dat$site == "c3", 45, 
                     ifelse(dat$site == "c4", 65,
                            ifelse(dat$site == "c3_floatgauge", 45, NA)))

# Add corrected depth column
dat$corrected_depth <- dat$depth - dat$offset

# Subset to only useful columns
keep <- c("site", "time2", "offset", "depth", "corrected_depth", "actual_conductivity", "specific_conductivity", 
          "pressure", "resistivity", "salinity", "tds", "temperature", "water_density")

dat_2 <- subset(dat, select = keep)
setDT(dat_2)

# Order dt2 by time2
dat_2 <- dat_2[order(time2)]

# NOW THE DATA IS IN LONG FORM. THIS IS NICE....but Roy wants it in wide form
# f <- as.formula(paste("time2 ~ ", paste(names(dat_2)[3:13], collapse = " + ")))
# 
# dat_wide <- dcast(dat_2, f, subset = NULL, drop = TRUE, value.var = "site")
# Make it even longer
dat_long <- melt(dat_2, id.vars = c("time2", "site"), variable.name = "colnames", value.name = "value", na.rm = T)
dat_long$newname <- paste0(dat_long$colnames, ".", dat_long$site)

dat_long <- dat_long[,c(1, 4, 5)]

dat_long <- unique(dat_long)

# Now make it wide
dat_wide <- dcast(dat_long, time2 ~ newname, fun = mean, subset = NULL, drop = TRUE, value.var = "value")

# Save wide format
out_dir <- file.path("C:", "Users", Sys.getenv("USERNAME"), "Dropbox (Smithsonian)", "GCREW_RESEARCHER_DATA", 
                     "gcrew_waterlevel", "combined")
filename <- paste0("waterlevel_combined_WIDE_", year, ".csv")
out_path <- file.path(out_dir, filename)

write.table(dat_wide, out_path, append = FALSE, quote = FALSE, sep = ",",
            na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"))

# Save long format
filename <- paste0("waterlevel_combined_LONG_", year, ".csv")
out_path <- file.path(out_dir, filename)

write.table(dat_2, out_path, append = FALSE, quote = FALSE, sep = ",",
            na = "NA", dec = ".", row.names = FALSE,
            col.names = TRUE, qmethod = c("escape", "double"))
  
# ------------------------------------------------------------------------------------------
  # for(n in 3:6){
year <- "2023"
    # Get files with that year
    group <- i[grepl(year, i)]
    
    # Read in first datatable
    dt <- fread(group[1])
    
    # Rbind all the rest
    if(length(group) > 1){
      for(m in 2:length(group)){
        dt2 <- fread(group[m])
        dt <- rbind.fill(dt, dt2)
      }
    }
    
    # Subtract offsets
    # c3log = 45
    # c4log = 65
    # gcrew_met = 58
    # genx = 76
    dt$offset <- ifelse(dt$site == "c3", 45, 
                        ifelse(dt$site == "c4", 65,
                               ifelse(dt$site == "gcrew_met", 58,
                                      ifelse(dt$site == "genx", 76, 
                                             ifelse(dt$site == "c3_floatgauge", 45, NA)))))
    
    # Add corrected depth column
    dt$corrected_depth <- dt$depth - dt$offset
    
    # Subset to only useful columns
    keep <- c("site", "time2", "offset", "depth", "corrected_depth", "actual_conductivity", "specific_conductivity", 
              "pressure", "resistivity", "salinity", "tds", "temperature", "water_density")
    
    dt2 <- subset(dt, select = keep)
    setDT(dt2)
    
    # Order dt2 by time2
    dt2 <- dt2[order(time2)]
    
    # NOW THE DATA IS IN LONG FORM. THIS IS NICE....but Roy wants it in wide form
    # Make it even longer
    dt_long <- melt(dt2, id.vars = c("time2", "site"), variable.name = "colnames", value.name = "value", na.rm = T)
    dt_long$newname <- paste0(dt_long$colnames, ".", dt_long$site)
    
    dt_long <- dt_long[,c(1, 4, 5)]
    
    dt_long <- unique(dt_long)
    
    # Now make it wide
    dt_wide <- dcast(dt_long, time2 ~ newname, subset = NULL, drop = TRUE, value.var = "value")
    
    # Save wide format
    out_dir <- file.path("C:", "Users", Sys.getenv("USERNAME"), "Dropbox (Smithsonian)", "GCREW_RESEARCHER_DATA", 
                         "gcrew_waterlevel", "combined")
    
    filename <- paste0("waterlevel_combined_WIDE_", year, ".csv")
    out_path <- file.path(out_dir, filename)
    
    # Format timestamp nicely for midnight tzs
    dt_wide$time2 <- format(as.POSIXct(dt_wide$time2, tz="UTC"), format="%Y-%m-%d %H:%M:%S")
    
    write.table(dt_wide, out_path, append = FALSE, quote = FALSE, sep = ",",
                na = "NA", dec = ".", row.names = FALSE,
                col.names = TRUE, qmethod = c("escape", "double"))
    
    # Save long format
    filename <- paste0("waterlevel_combined_LONG_", year, ".csv")
    out_path <- file.path(out_dir, filename)
    
    # Format timestamp nicely for midnight tzs
    dt2$time2 <- format(as.POSIXct(dt2$time2, tz="UTC"), format="%Y-%m-%d %H:%M:%S")
    
    write.table(dt2, out_path, append = FALSE, quote = FALSE, sep = ",",
                na = "NA", dec = ".", row.names = FALSE,
                col.names = TRUE, qmethod = c("escape", "double"))
  # }
