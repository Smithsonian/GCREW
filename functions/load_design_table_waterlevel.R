### This function will load the design tables for the project and merge them into one big table for processing 

library(tidyverse)
##Arguments: 
#design -- full name/directory of the main design table for the project. 
#plotnames -- full name/directory of the plotnames design table for the project.
#varnames -- full name/directory of the design-type design table for the project.


load_design_table <- function() {
  
  ##Load in the design tables 
  plotnames <- read.csv("design_tables/plotnames.csv")
  varnames <- read.csv("design_tables/design-type.csv")
  design <- read.csv("design_tables/design.csv")
  
  
  #the experimental design is organized into three documents. The first step is to merge them to one table. 
  merged_design <- design %>%
    left_join(plotnames, by = "link")%>%
    left_join(varnames, by = "research_name")
  
  
  return(merged_design)
  
}
