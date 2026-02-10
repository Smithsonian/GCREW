#### Step 3 -- Combine Normalized Data from All Tables #### 
#Description -- 
#Clean the data in each table based on a low and high limit (data range)
#Put the data in long format using the design table (normalize it)
#ensure correct time stamp formatting 

####------------------Required User Input-------------------------------------#####
#We only want to do this on yearly files that have ALL of their loggernet data.
#enter in the vector below the year you want to combine

year <- "2025"

#Load necessary packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(reshape2, lubridate, data.table, tools, plyr, tidyverse, stringr)

#------------------- End Required user input --------------------------------------#

#### Load Functions and Directories ####
#Load all functions 
invisible(lapply(list.files("functions/", pattern = "\\.R$", full.names = TRUE), source))

#relevant directories
L0_NormalizedYearly_dir <- paste0(Sys.getenv("dropbox_filepath") , "Taylor_Projects/TEST/2_L0_NormalizedData/processed/yearly/")
L0_CombinedYearly_dir <- paste0(Sys.getenv("dropbox_filepath") , "Taylor_Projects/TEST/2_L0_NormalizedData/processed/combined/")
#researcherdata_dir <- (paste0(Sys.getenv("TE_dropbox_filepath"), "GCREW_RESEARCHER_DATA"))

  #Get files
  i <-list.files(L0_NormalizedYearly_dir, pattern = NULL, all.files = FALSE,
               full.names = TRUE, recursive = F,
               ignore.case = FALSE, include.dirs = F)

  # Get years from filename
  years <- sapply(strsplit(i, split = "_"), tail, 1)
  years <- unique(substr(years, 1, 4))

#### Combine Steps -- done one yearly file at a time ####
  #Get files with that year
  group <- i[grepl(year, i)]

  #Read in first datatable
  dt <- fread(group[1])

  #Rbind all the rest
  if(length(group) > 1){
    for(m in 2:length(group)){
      dt2 <- fread(group[m])
      dt <- rbind.fill(dt, dt2)
    }
  }

  #Subtract offsets
  # c3log = 45
  # c4log = 65
  # gcrew_met = 58
  # genx = 76
  # genxhydros = 56
  dt$offset <- ifelse(dt$site == "c3", 45, 
                      ifelse(dt$site == "c4", 65,
                             ifelse(dt$site == "gcrew_met", 58,
                                    ifelse(dt$site == "genx", 76, 
                                           ifelse(dt$site == "genxhydros", 56,
                                                  ifelse(dt$site == "c3_floatgauge", 45, NA))))))
  
  #Add corrected depth column
  dt$corrected_depth <- dt$depth - dt$offset
  
  #Subset to only useful columns
  keep <- c("site", "time2", "offset", "depth", "corrected_depth", "actual_conductivity", 
            "electrical_conductivity","specific_conductivity", "pressure", 
            "resistivity", "salinity", "tds", "temperature", "water_density")
  
  dt2 <- subset(dt, select = keep)
  setDT(dt2)
  
  #Order dt2 by time2
  dt2 <- dt2[order(time2)]
  
  #NOW THE DATA IS IN LONG FORM. THIS IS NICE....but Roy wants it in wide form
  #Make it even longer
  dt_long <- melt(dt2, id.vars = c("time2", "site"), variable.name = "colnames", value.name = "value", na.rm = T)
  dt_long$newname <- paste0(dt_long$colnames, ".", dt_long$site)
  
  dt_long <- dt_long[,c(1, 4, 5)]
  
  dt_long <- unique(dt_long)

  #Now make it wide
  dt_wide <- dcast(dt_long, time2 ~ newname, subset = NULL, drop = TRUE, value.var = "value")
  
  #Save wide format
  filename <- paste0("waterlevel_combined_WIDE_", year, ".csv")
  out_path <- file.path(L0_CombinedYearly_dir, filename)
  
  #Format timestamp nicely for midnight tzs
  dt_wide$time2 <- format(as.character(dt_wide$time2))

  write.table(dt_wide, out_path, append = FALSE, quote = FALSE, sep = ",",
              na = "NA", dec = ".", row.names = FALSE,
              col.names = TRUE, qmethod = c("escape", "double"))
  
  #Save long format
  filename <- paste0("waterlevel_combined_LONG_", year, ".csv")
  out_path <- file.path(L0_CombinedYearly_dir, filename)
  
  #Format timestamp nicely for midnight tzs
  dt2$time2 <- format(as.character(dt2$time2))

  write.table(dt2, out_path, append = FALSE, quote = FALSE, sep = ",",
              na = "NA", dec = ".", row.names = FALSE,
              col.names = TRUE, qmethod = c("escape", "double"))

  