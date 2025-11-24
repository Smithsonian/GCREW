### This function will take the yes/no option for each vaariable in a table for cleaning and run a rolling mean 
#cleaning with the variables marked "yes", then replace them in the data set. A defined variability constant will be used for each variable. 

##Arguments: 
#design_table =  the part of the design table that has been indexed to only include info relevant to the table being processed. 
#csv_data = the raw data table with cr1000_names inserted as headers. 


apply_rolling_clean <- function(design_table, csv_data){
  
  
  #List the variables to undergo a rolling clean in this data set: 
  variables_to_clean <- design_table%>%
    filter(rolling_clean == "yes")%>%
    pull(cr1000_name)
  
  #get the resolution of the data in minutes from the design table 
  resolution <- unique(design_table$resolution_minutes)
  
  for (variable in variables_to_clean){
    
    dt <- as.data.table(csv_data[variable])
    colnames(dt) <- "data"
    
    variability_constant <- design_table%>%
      filter(cr1000_name == variable)%>%
      pull(variability_constant)
    
    dt[, roll_mean := frollapply(data, (60/resolution), function(x){ mean(x, na.rm = TRUE) } , fill = NA, align = c("center"))]    
    
    #mark upper and lower boundaries of what is considered close to the median. (varies by the variability of the variable)
    dt[,m_data_upper:= roll_mean + variability_constant ,] 
    dt[,m_data_lower:= roll_mean - variability_constant ,]
    
    #remove values outside of the upper/lower allowable boundaries
    dt$data <- ifelse(dt$data > dt$m_data_upper, NA, dt$data) 
    dt$data <- ifelse(dt$data < dt$m_data_lower, NA, dt$data)
    
    print(paste0("Values removed from ",variable," by rolling clean: ",
                 sum(is.na(dt$data)) - sum(is.na(csv_data[variable]))))
    
    #now replace the raw data with the auto-cleaned data
    csv_data[variable] <- dt$data
    
  }
  
  return(csv_data)
}
