## Function --  Split the unprocessed data into monthly files. for each month, check if the file already exist. If it does, append the data. 
#If it does not, create a new file for that month. 
#if appending, check for duplicates durring the appending process so it does not need to be done down the line 


write_monthly_data_waterlevel_01 <- function(dt, rawCSVData_dir, filename) {
  
  dt$year_month <- substr(dt$TIMESTAMP,1,7)
  months <- unique(dt$year_month)
  
  for (m in months){
    #Filter the current dataset down to just the month in question
    dt_m <- dt %>%
      filter(year_month == m)%>%
      select(!year_month) #once this has been used for filtering, immediately get rid of it so it does not create an aggragation issue
    
    #define a name for this monthly file
    file_path <- paste0(rawCSVData_dir,filename,"_", m, ".csv") 
    
    #This is here just in case this is a brand new table. Might move to the automated steps script. Makes a folder for this table in the rawCSV folder for neatness. 
    if (!dir.exists(paste0(rawCSVData_dir))){
      dir.create(paste0(rawCSVData_dir), recursive = TRUE)
    }
    
    #determine if any data for that month has already been processed. 
    if(!file.exists(file_path)){
      
      #if the file does not already exist, we do not need to run duplicate removal. 
      write.csv(dt_m, file_path, row.names = FALSE)
      
    }else{
      
      #if there is data already for this month, append the new data to that file and remove any duplicates 
      existing_data <- read.csv(file_path)
      
      #Combine the existing data and the new data -- could be more robust 
      combined_data <- rbindlist(list(dt_m, existing_data), use.names=F, fill = T)
      
      #run this function which handles the possibility of duplicate timestamps 
      aggregated_combined_data <- aggregate_data_waterlevel(combined_data)
      
      aggregated_combined_data <- aggregated_combined_data %>%
        mutate(.TIMESTAMP_SORT = as.POSIXct(TIMESTAMP, format = "%Y-%m-%d %H:%M:%S")) %>%
        arrange(.TIMESTAMP_SORT) %>%
        select(-.TIMESTAMP_SORT)
      
      write.csv(aggregated_combined_data, file_path, row.names = FALSE)
      
    }
  }
}
