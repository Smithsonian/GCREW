### This function will load the design tables for the project and merge them into one big table for processing 

library(tidyverse)
##Arguments: 
#design -- full name/directory of the main design table for the project. 
#plotnames -- full name/directory of the plotname design table for the project.

load_design_table <- function() {
  
  #the experimental design is organized into two documents. The first step is to merge them to one table. 
  merged_design <- left_join(design, plotnames, by = "logger") %>%
    select(-Notes)
  
  #may add if statement for if logger column is reading NA
  
  return(merged_design)
  
}