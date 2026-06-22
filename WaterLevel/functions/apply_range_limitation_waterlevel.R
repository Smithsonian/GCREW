### This function will check for a ranged defined in the design table for a particular variable based on research name  
#Then apply range limitation to variables that do have a set range and let you know how many points were removed. 

##Arguments: 
#design_table =  the part of the design table that has been indexed to only include info relevant to the table being processed. 
#normalized_data = the raw data table with cr1000_names inserted as headers. 

library(data.table)
library(tidyverse)

apply_range_limitation_waterlevel<- function(design_table, normalized_data){
  
  #List the variables to undergo a rolling clean in this data set: 
  variables_to_clean <- design_table%>%
    filter(!is.na(lower_bound) & !is.na(upper_bound))%>%
    pull(research_name)%>%
    unique()
  
  for (variable in variables_to_clean){
    
    dt <- as.data.table(normalized_data[variable])
    colnames(dt) <- "data"
    
    lower_bound <- design_table%>%
      filter(research_name == variable)%>%
      pull(lower_bound)
    
    upper_bound <- design_table%>%
      filter(research_name == variable)%>%
      pull(upper_bound)
    
    #remove values outside of the upper/lower allowable boundaries
    dt$data <- ifelse(dt$data > upper_bound, NA, dt$data) 
    dt$data <- ifelse(dt$data < lower_bound, NA, dt$data)
    
    print(paste0("Values removed from ",variable," by range limitation: ",
                 sum(is.na(dt$data)) - sum(is.na(normalized_data[variable]))))
    
    #now replace the raw data with the auto-cleaned data
    normalized_data[variable] <- dt$data
    
  }
  
  return(normalized_data)
}

