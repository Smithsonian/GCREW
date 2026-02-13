#### Step 2 -- Normalization and Automatic Range Clean #### 
#Description -- 
#Combine datasets from all tables by year
#Put data in long and wide form
#Possibly add filled column data

####------------------Required User Input-------------------------------------#####
#We only want to do this on monthly files that have ALL of their loggernet data.
#enter in the vector below any months that may not have a complete data set yet in the format yyyy-mm
#recommend excluding the current month and the previous month 

library(tidyverse)
library(stringr)
library(data.table)

exclude_months <- c("2026-02", "2026-01")%>%
  paste(collapse = "|")
#------------------- End Required user input --------------------------------------#

#### Load Functions, Directories, and Design Tables ####
#Load all functions 
invisible(lapply(list.files("functions/", pattern = "\\.R$", full.names = TRUE), source))

#relevant directories
rawCSVData_dir <- paste0(Sys.getenv("dropbox_filepath") , "Taylor_Projects/TEST/1_RawCSVData/unprocessed/")
rawCSVDataArchive_dir <- paste0(Sys.getenv("dropbox_filepath") , "Taylor_Projects/TEST/1_RawCSVData/processed/")
L0_NormalizedData_dir <- paste0(Sys.getenv("dropbox_filepath") , "Taylor_Projects/TEST/2_L0_NormalizedData/processed/")
#researcherdata_dir <- (paste0(Sys.getenv("TE_dropbox_filepath"), "GCREW_RESEARCHER_DATA"))

#design table 
plotnames <- read.csv(paste0(Sys.getenv("TE_dropbox_filepath"), "GCREW_LOGGERNET_WORKFLOW/design documents/plotnames_waterlevel.csv")) %>%
  select(-offset_022026)
design <- read.csv(paste0(Sys.getenv("TE_dropbox_filepath"), "GCREW_LOGGERNET_WORKFLOW/design documents/waterlevel_design.csv"))
merged_design <- load_design_table()
#get the names of the tables from the design document
table_name <- unique(merged_design$Table)

#### Normalization Steps -- done one table and one monthly file at a time ####
for (table in table_name){
  cat(paste0("\n Processing ",table,": \n"))
  
  #filter the design table to just the specific loggernet table we are working with and get the cr1000_names
  design_table <- filter(merged_design, Table == table) 

  #define all the files that are to be processed based on the logger name and the excluded months. 
  files <- list.files(rawCSVData_dir, pattern = table, recursive = T, full.names = T, ignore.case = T) %>%
    str_subset(exclude_months, negate = TRUE)
  
  for (file in files){ 
    
    #get the CSV file associated with that table and change headers to cr1000 names. 
    csv_data <- read.csv(file) %>%
      select(-Format) %>%
      mutate(rowid = paste("gcrew", .[[3]], tolower(.[[5]]), sep="_"),
             time2 = .[[3]])
    
    #correct genxhydros logger name
    if (table == "waterlevelhydros"){
      csv_data$Statname <- "genxhydros"
    }
    
    #ensure there is statname data
    if (NA %in% c(csv_data$Statname, csv_data$STATNAME)){
      #manufacture logger data (needed for merge)
      newloggerinfo <- design_table$logger[1]

      if ("Statname" %in% names(csv_data)){
        csv_data$Statname <- newloggerinfo
      } else if ("STATNAME" %in% names(csv_data)){
        csv_data$STATNAME <- newloggerinfo
      }
    }
    
    #convert column to lower case for consistency
    csv_data[sapply(csv_data, is.character)] <- lapply(csv_data[sapply(csv_data, is.character)], tolower)
    
    #change the loggernet headers to the cr1000 names given in the design table. 
    csv_data <- convert_loggernet_headers_waterlevel(design_table, csv_data)
    
    #Normalize the data. this function spits out warnings and I can't figure out how to fix it. It does not affect the data. I tested every which way. 
    normalized_data <- normalize_loggernet_csv_data_waterlevel(csv_data, design_table) 
    
    #Apply range limitation cleaning for variables that have been marked with a range 
    normalized_data <- apply_range_limitation_waterlevel(design_table, normalized_data)
    
    ##You can use the below lines when setting up the design tables to check if the range limitation constants you are using are appropriate. 
    #plot <- plot_variable_waterlevel(normalized_data,normalized_data$barometric_pressure)
    #plot
    
    #try to process data all together instead of grabbing files sporadically - it takes longer to process
    write_monthly_yearly_waterlevel(normalized_data, L0_NormalizedData_dir)
    
  }  
}
  
