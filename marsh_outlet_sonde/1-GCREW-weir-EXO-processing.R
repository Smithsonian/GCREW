## Processing GCREW Sonde Data

# User should run the "0-GCREW-workflow-execute.R" script up to the "sorter" step, to process Loggernet EXO tables to CSV step.
# This script folds the data appropriately and assigns correct headers for output.

# Reads in data from the sorted_working folder, then saves it to the normal_working folder
# After this, you can run the monthlymanage and bundle functions in "0-GCREW-workflow-execute.R"

# ----------- Set up libraries and directories -------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(lubridate, data.table, tools, plyr, tidyverse)

# Set various directories
source_dir <- file.path("C:", "Users", Sys.getenv("USERNAME"), "Dropbox (Smithsonian)", "GCREW_LOGGERNET_WORKFLOW", "2_sorted")

# Folder you want in sorted_dir
folder <- "gcrewmarshoutlet_exotable"

# Set output dir, which is the "normal" directory
output_dir <- file.path("C:", "Users", Sys.getenv("USERNAME"), "Dropbox (Smithsonian)", "GCREW_LOGGERNET_WORKFLOW",
                        "3_normal")

# Read in files
files <- list.files(file.path(source_dir, "sorted_working", folder), pattern = NULL, all.files = FALSE,
                    full.names = TRUE, recursive = FALSE,
                    ignore.case = T)

# Read in header lookup
header_lookup <- fread("Loggernet processing/design documents/gcrew_weir_header_lookup.csv")

# Read in final data structure
data_structure <- fread("Loggernet processing/design documents/gcrew_weir_data_structure.csv")

# Set site timezone
site_timezone <- "America/Jamaica"

# Set site instrument id
instrument <- "GCREW_weir_water-quality"

# -------- Function: Evaluate headers ----------------
# Evaluates whether the column names match expected headers
# Returns FALSE if any columns are unexpected
# Columns can be missing

evaluate_headers <- function(df, header_lookup){
  if(any(!(colnames(df) %in% header_lookup$original_column))){
    return(FALSE)
  } else {
    return(TRUE)
  }
}

# -------------- Process all files ------------------
for(n in 1:length(files)){
  # Read in data
  dat <- fread(files[n])

  # If all headers in data are expected...  
  if(evaluate_headers(dat, header_lookup)){
    # ------------------ Only keep columns that are in header lookup table -----------------
    dat2 <- dat %>%
      rename_at(header_lookup$original_column, ~ header_lookup$updated_column)
    
    # ------------------ Add missing columns ---------------------------------------
    # Look for missing columns
    missing_columns <- header_lookup %>% 
      filter(!(updated_column %in% colnames(dat2)))
    
    # Add any missing columns 
    if(nrow(missing_columns) > 0){
      # For each row in the schema, rename the associated header in the dataframe
      for(k in 1:nrow(missing_columns)){
        dat2 <- dat2 %>%
          mutate(!!missing_columns$updated_column[k] := NA)
      }
    }
    
    # ------------------ Drop data where the data says No Sonde! or is erroneous -----------------------
    dat2 <- dat2[!grepl("No Sonde!", dat2$snn),]
    dat2 <- dat2[dat2$snn != "",]
    dat2 <- dat2[!is.na(dat2$snn),]
    
    #  -------------------- Process timestamp correctly ----------------------------------------
    # Sometimes the logger timestamp is wrong
    # Create a time variable for the sonde internal time
    # Set wrong dates and times (7999 is an error code) to NA
    dat2 <- dat2 %>%
      mutate(date = ifelse(date == 7999, NA, date),
             time = ifelse(time == 7999, NA, time))
    
    # Set date to date variable and time to character
    dat2 <- dat2 %>%
      mutate(date = mdy(date),
             time = as.character(time))
    
    # The time variable needs a little bit of modification...
    dat2 <- dat2 %>%
      mutate(time = ifelse(nchar(time) == 2, paste0("0000", time),
                        ifelse(nchar(time) == 3, paste0("000", time),
                               ifelse(nchar(time) == 4, paste0("00", time),
                                      ifelse(nchar(time) == 5, paste0("0", time), time)))))
    
    # Add colons and then create new time variable for the sonde timestamp
    dat2 <- dat2 %>%
      mutate(time = paste0(substr(time, 1, 2), ":", substr(time, 3, 4), ":", substr(time, 5, 6)),
             sonde_timestamp = ymd_hms(paste0(date, time), tz = "America/New_York"))
    
    # Convert sonde timestamp to EST, not American daylight savings time
    dat2 <- dat2 %>%
      mutate(sonde_timestamp_est = with_tz(sonde_timestamp, tzone = site_timezone))
    
    # If logger time is egregious, substitute it for sonde time
    # Ask ML how to deal with this, if he can think of anything? 
    dat2 <- dat2 %>%
      mutate(timestamp_local = as.POSIXct(ifelse(year(timestamp_local) > year(Sys.time()),
                                                 sonde_timestamp_est, timestamp_local), origin = "1970-01-01", tz = "UTC"))
    
    # If there are still erroneous timestamps with huge years, drop those values?
    dat2 <- dat2 %>%
      filter(year(timestamp_local) <= year(Sys.time()))
    
    # ------------------- Create new time variable for the GCREW workflow (time2) -------------------------
    # Round time to the nearest minute and figure out the data interval that it should be rounded to
    second_time <- round(second(dat2$timestamp_local)/60)*60
    
    # Estimate time interval
    # Find mode of possible intervals to guess the interval you expect
    # This might backfire on me..........
    # So might ask ML how to deal with several intervals in one file
    
    # Get possible data intervals 
    interval <- diff(dat2$timestamp_local)
    units(interval) <- "mins"
    unique_interval <- unique(interval)
    print(unique_interval)
    
    # Tabulate the number of times each interval value appears
    tab <- tabulate(match(interval, unique_interval))
    print(tab)
    print(n)
    
    # Get the most frequent interval in the dataset
    unique_interval <- unique_interval[tab == max(tab)]
    unique_interval <- round(unique_interval, digits = 0)
    
    # Round timestamps to the most frequently seen interval
    minute_time <- round(minute(dat2$timestamp_local)/unique_interval)*unique_interval
      
      # Create new time variables for different timezones, set logger / program / table to lower -----
      # Add instrument ID column
      dat2 <- dat2 %>%
        mutate(timestamp_local = ymd_hms(timestamp_local, tz = site_timezone),
               time2 = update(timestamp_local, minute = minute_time, second = second_time),
               timestamp_utc = with_tz(time2, tzone = "UTC"),
               logger = tolower(logger),
               program = tolower(program),
               table = tolower(table),
               instrument_id = !!instrument)
      
      # Get only unique values
      dat2 <- dat2 %>%
        distinct_at(vars(-program), .keep_all = T)
    
      # Select only new observations and add ID column -------
      all_data <- read_csv(list.files(output_dir, pattern = "exotable", all.files = FALSE,
                                      full.names = TRUE, recursive = T,
                                      ignore.case = T))
      
      # If there is data in the output folder:
      if(nrow(all_data) > 0){
        # Get most recent ID from data
        last_id <- all_data$id[nrow(all_data)]
        
        # Format all_data timestamp
        all_data <- all_data %>%
          mutate(timestamp_local = ymd_hms(timestamp_local, tz = site_timezone))
        
        # Subset korexo data
        subset_data <- dat2 %>%
          # Keep distinct values only, minus the program column
          distinct_at(vars(-program), .keep_all = T) %>%
          # Filter to keep timestamps that are NOT in all_data timestamp
          filter(!(timestamp_local %in% all_data$timestamp_local)) %>%
          # Create new ids to be (row number) + max number from all_data
          mutate(id = (row_number() + as.numeric(last_id)))
        
      } else{
        # If there's no data in the output folder, create IDs just based on row number
        subset_data <- dat2 %>%
          distinct_at(vars(-program), .keep_all = T) %>%
          mutate(id = row_number())
      }
      
      # Select and order cols we want -------- 
      dat_final <- subset_data %>%
        select(any_of(data_structure$column_name))
      
      # Write filename ---------------
      filename <- paste0("GCREW_weir_exotable_", 
                         date(dat_final$timestamp_local[1]), "_",
                         date(dat_final$timestamp_local[nrow(dat_final)]), ".csv")
      
      full_path <- file.path(output_dir, "normal_working", filename)
      
      # Run tests on duplicated timestamps
      # For n=51 which has like 300 duplicated timestamps, delete the duplicated timestamps
    
      # print(which(duplicated(dat_final$time2)))
      # dup_times <- dat_final$time2[(duplicated(dat_final$time2))]
      # check <- dat_final[dat_final$time2 %in% dup_times,]
      # check2 <- check[order(check$time2),]
      #
      # Write file
      write.table(dat_final, full_path, append = FALSE, quote = FALSE, sep = ",",
                   na = "NA", dec = ".", row.names = FALSE,
                   col.names = TRUE, qmethod = c("escape", "double"))

       # Copy original file to sorted archive folder
       file.copy(files[n], file.path(source_dir, "sorted_archive", folder))
       # Remove original file from sorted_working
       file.remove(files[n])
  } else{
    print("Unexpected columns in data")
  }
}
