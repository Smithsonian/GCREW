## Convert raw loggernet file to a CSV. break the data into monthly chunks 

#Load all functions 
invisible(lapply(list.files("functions/", pattern = "\\.R$", full.names = TRUE), source))


#relevant directories
rawData_dir <- paste0(Sys.getenv("dropbox_filepath") , "Chapada_Stem_Data/0_RawData/unprocessed_data/")
rawDataArchive_dir <- paste0(Sys.getenv("dropbox_filepath") , "Chapada_Stem_Data/0_RawData/archive_data/")
rawCSVData_dir <- paste0(Sys.getenv("dropbox_filepath") , "Chapada_Stem_Data/1_RawCSVData/unprocessed/")

#### Step 1  -- Convert to CSV ####
#Description -- Load the raw Loggernet files and save them as .csv files Then archive the raw Loggernet file. 

#list files in the raw data folder to be converted. 
files <- list.files(rawData_dir, full.names = T)%>%
  str_subset(pattern = 'backup',negate = TRUE)%>% #We are not processing backup tables
  str_subset(pattern = 'FLUX_7810',negate = TRUE)%>% #We are not currently processing licor data
  str_subset(pattern = 'FLUX_COMB',negate = TRUE)%>% #we are not currently processing licor data
  str_subset(pattern = 'NewConstTable',negate = TRUE) # We are not processing the NewConstTable 

#load the files and save as a csv in a for loop 
for (file in files){
  
  #load in the data 
  dt <- read_datalogger_file_chapada(file)
  
  #filter out any weird time stamps that are marked before 2025 -- note that we want to put it int he proper timezone here as well
  dt <- dt %>%
    filter(year(TIMESTAMP) > 2024)
  
  #write it out as a csv file. break into monthly files and handle duplicate timestamps. 
  filename <- substr(basename(file),1,(nchar(basename(file))-4)) #will need to change the number once the renaming functions kick in. 
  
  write_monthly_data_chapada(dt, rawCSVData_dir, filename)
  #move the raw loggernet data file into the archive
  file.rename(file, paste0(rawDataArchive_dir, basename(file)))
  
}


