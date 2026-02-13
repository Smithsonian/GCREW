## Function --  Split the unprocessed data into monthly files. for each month, check if the file already exist. If it does, append the data. 
#If it does not, create a new file for that month. 
#if appending, check for duplicates durring the appending process so it does not need to be done down the line 
library(tidyverse)
library(data.table)
library(anytime)

write_monthly_yearly_waterlevel <- function(dt, L0_NormalizedData_dir) {
  
  #ensure time columns remain as characters before writing into csv
  dt$time2 <- as.character(dt$time2)
  dt$timestamp <- as.character(dt$timestamp)
  
  #for naming consistency and filtering
  dt$year_month <- substr(dt$timestamp,1,7)
  year_month_var <- unique(dt$year_month)
  dt$years  <- substr(dt$timestamp, 1, 4)
  year <- unique(dt$years)
  
  for (m in year_month_var){
    #Filter the current dataset down to just the month in question
    dt_m <- dt %>%
      filter(year_month == m) %>% 
      select(-year_month, -years)
    
    month <- unique(substr(dt_m$timestamp, 6, 7))
    
    #define a name for this monthly file
    file_path <- paste0(L0_NormalizedData_dir, "monthly/", table, "_",year,"_",month, ".csv")
    
    #This is here just in case this is a brand new table. Might move to the automated steps script. Makes a folder for this table in the rawCSV folder for neatness.
    if (!dir.exists(paste0(L0_NormalizedData_dir, "monthly","/"))){
      dir.create(paste0(L0_NormalizedData_dir, "monthly","/"), recursive = TRUE)
    }
    
    #determine if any data for that month has already been processed.
    if(!file.exists(file_path)){
      
      write.csv(dt_m, file_path, row.names = FALSE)
      
    }else{
      
      #if there is data already for this year, append the new data to that file and remove any duplicates
      existing_data <- read.csv(file_path)
      
      #Combine the existing data and the new data
      monthly_data <- rbindlist(list(dt_m, existing_data), use.names=F, fill=T) #remove fill = true
      
      monthly_data <- monthly_data %>%
        mutate(.TIMESTAMP_SORT = as.POSIXct(time2, format = "%Y-%m-%d %H:%M:%S")) %>%
        arrange(.TIMESTAMP_SORT) %>%
        select(-.TIMESTAMP_SORT)
      
      write.csv(monthly_data, file_path, row.names = FALSE)
    }
  }
  
  
  for (y in year){
    
    dt_y <- dt %>%
      filter(years == y) %>%
      select(-year_month, -years)
    
    #establish file paths to be used later
    file_path <- paste0(L0_NormalizedData_dir, "yearly" ,"/", table, "_", y,".csv")
    archived_path <- paste0(rawCSVDataArchive_dir, basename(file))
    
    #This is here just in case this is a brand new table. Might move to the automated steps script. Makes a folder for this table in the rawCSV folder for neatness.
    if (!dir.exists(paste0(L0_NormalizedData_dir, "yearly","/"))){
      dir.create(paste0(L0_NormalizedData_dir, "yearly","/"), recursive = TRUE)
    }
    
    #determine if any data for that month has already been processed.
    if(!file.exists(file_path)){
      
      write.csv(dt_y, file_path, row.names = FALSE)
      
    }else{
      
      #if there is data already for this year, append the new data to that file and remove any duplicates
      existing_data <- read.csv(file_path)
      
      #Combine the existing data and the new data
      yearly_data <- rbindlist(list(dt_y, existing_data), use.names=F, fill=T) #remove fill = true
      
      yearly_data <- yearly_data %>%
        mutate(.TIMESTAMP_SORT = as.POSIXct(time2, format = "%Y-%m-%d %H:%M:%S")) %>%
        arrange(.TIMESTAMP_SORT) %>%
        select(-.TIMESTAMP_SORT)
      
      write.csv(yearly_data, file_path, row.names = FALSE)
      
    }
    
    #put the file that you just processed in the archive so that it does not needlessly get processed again.  
    file.rename(file, paste0(rawCSVDataArchive_dir, basename(file)))

  }
}
