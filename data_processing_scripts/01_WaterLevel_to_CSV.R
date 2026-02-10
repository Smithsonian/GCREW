#### Step 2 -- Convert raw loggernet file to a CSV. break the data into monthly chunks #### 
#Description -- 
#The raw data from loggernet are .dat files
#Need to be .csv files to manipulate

#### Load Functions and Directories ####
invisible(lapply(list.files("functions/", pattern = "\\.R$", full.names = TRUE), source))

# previous relevant directories
#rawData_dir <- paste0(Sys.getenv("dropbox_filepath") , "GCREW_LOGGERNET_DATA/0_RawData/unprocessed_data/")
#rawDataArchive_dir <- paste0(Sys.getenv("dropbox_filepath") , "GCREW_LOGGERNET_DATA/0_RawData/archive_data/")
#rawCSVData_dir <- paste0(Sys.getenv("dropbox_filepath") , "GCREW_LOGGERNET_WORKFLOW/1_RawCSVData/unprocessed/")

#current relevant directories
#will have to modify once dropbox folder is created
rawData_dir <- paste0(Sys.getenv("TE_dropbox_filepath") , "Taylor_Projects/TEST/0_RawData/unprocessed_data/")
rawDataArchive_dir <- paste0(Sys.getenv("TE_dropbox_filepath") , "Taylor_Projects/TEST/0_RawData/archive_data/")
rawCSVData_dir <- paste0(Sys.getenv("TE_dropbox_filepath") , "Taylor_Projects/TEST/1_RawCSVData/unprocessed/")

#### Step 1  -- Convert to CSV ####
#Description -- Load the raw Loggernet files and save them as .csv files Then archive the raw Loggernet file. 

#list files in the raw data folder to be converted. 
files <- list.files(rawData_dir, full.names = T, ignore.case = T)%>%
  str_subset(pattern = "(?i)WaterLevel") #Only include water level datasets in current data

#load the files and save as a csv in a for loop 
for (file in files){
  
  #load in the data 
  dt <- read_datalogger_file_waterlevel(file)
  
  #filter out any weird time stamps that are marked before 2025 -- note that we want to put it int he proper timezone here as well
  dt <- dt %>%
    filter(year(TIMESTAMP) > 2018)
  
  #write it out as a csv file. break into monthly files and handle duplicate timestamps. 
  if (grepl("GCREW_MET_GCREW_WaterLevel200", file)) {
    filename <- substr(basename(file),1,(nchar(basename(file))-19))
    filename <- gsub("GCREW_MET_GCREW_WaterLevel200", "gcrew_met_gcrewwaterlevel200", filename)
  } else{
    filename <- substr(basename(file),1,(nchar(basename(file))-19)) #will need to change the number once the renaming functions kick in. 
  }
  
  #write and move newly converted .csv file
  write_monthly_data_waterlevel_01(dt, rawCSVData_dir, filename)
  
  #move the raw loggernet data file into the archive
  file.rename(file, paste0(rawDataArchive_dir, basename(file)))
  
}

