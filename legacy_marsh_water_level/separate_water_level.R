library(data.table)
library(tidyverse)

marsh_dat <- fread("marsh_water_level_2016_2022.csv")

years <- unique(year(marsh_dat$timestamp))

waterlevel_dir <- file.path("C:", "Users", Sys.getenv("USERNAME"), "Dropbox (Smithsonian)", "GCREW_RESEARCHER_DATA", 
                            "gcrew_waterlevel", "yearly")

for(n in 1:length(years)){
  temp_dat <- marsh_dat[year(marsh_dat$timestamp) == years[n],]
  
  temp_dat <- temp_dat %>%
    select(-date, -year, -yday, -hour_minute)
  
  temp_dat <- temp_dat %>%
    select(timestamp, corrected_depth_cm)
  
  names(temp_dat) <- c("time2", "depth")
  
  temp_dat$site <- "c3_floatgauge"
  
  outpath <- file.path(waterlevel_dir, paste0("marsh_waterlevel_", years[n], ".csv"))
    
  write.table(temp_dat, outpath, append = FALSE, quote = FALSE, sep = ",",
              na = "NA", dec = ".", row.names = FALSE,
              col.names = TRUE, qmethod = c("escape", "double"))
}

