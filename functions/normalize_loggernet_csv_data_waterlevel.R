### This function will take data from a raw CSV format directly read from loggernet.
#Reference a design table with a standard set of columns, and then "normalize" the data based on the values of the design table. 

##Arguements: 
#design_table <- the merged and filtered design table for the data that you want to process. 
#table -- the name of the loggernet table that you would like to process
#csv_dir  --  the directory to the raw loggernet data in csv format 

library(data.table)
library(tidyverse)

normalize_loggernet_csv_data_waterlevel <- function(csv_data, design_table, data_dir) {
  
  #ensure genx hydros depth column is in cm, not mm
  if (csv_data$logger[1] == "genxhydros") {
    csv_data <- csv_data %>%
      mutate(depth = depth/10)
  }
  
  #check that there are chamber level variables. If not, you can skip the normalization. 
  non_id_cols <- design_table%>%
    filter(var_norm_split == "key")%>%
    pull(cr1000_name)
  
  if (length(non_id_cols) > 0){
    
    #normalize the data. See below comments for specific steps. 
    normalized_data <- csv_data %>%
      left_join(plotnames, by = "logger")%>%
      relocate(site, .before = 1)
  }
  
  else {
    normalized_data <- csv_data
  }
  
  return(normalized_data)
  
}