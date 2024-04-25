# Porewater NH4 Compiling and QAQC  ---------------------------------------

# This code compiles processed data from all SEAL runs. 

# Code was modified from "merging NH4 microplate data.R" to 
# Determine samples to rerun if they are above or below the detection limit    
# Perform basic QAQC on complied NH4 data 
    # Remove all dups of sample other than the most recent run 
    # Format compiled annual file for transfer to archive

# Erin Fien 03/30/2023
# Zoe Read 01/19/2024


# Set Up ------------------------------------------------------------------


##Clear environment
rm(list=ls()) 

##Read in packages
library(gtools)
library(ggplot2)
library(Rmisc)
library(dplyr)
library(reshape2)
library(tidyr)

## Change these parameters before running the script##
Year <- 2024
Project <- "SMARTX"


# 2. Set File Path ------------------------------------------------------------------
if (Project=="SMARTX"){
  wd <-(paste0("S:/Biogeochemistry/GCREW/4-SMARTX/3-Data/Porewater",sep=""))
}
if (Project=="CO2xN"){
  wd <-(paste0("S:/Biogeochemistry/GCREW/2-CO2xN Experiment/Porewater",sep=""))
}
if (Project=="CO2xCommunity"){
  wd <-(paste0("S:/Biogeochemistry/GCREW/1-CO2xCommunity Experiment/3-Data/Porewater",sep=""))
}
if (Project=="Phrag"){
  wd <-(paste0("S:/Biogeochemistry/GCREW/3-PhragmitesxCO2xN Experiment/Porewater",sep=""))
}

# Read in Metadata for final output 
setwd(wd)
meta <- read.csv(paste0(wd,"/Annual Porewater Sample Lists/", Project, " Annual Porewater Sample List.csv"))
meta$Year <- Year



# Import Data -------------------------------------------------------------
#List File Names
setwd(paste0(wd,Year,'/Data', sep=''))

library(tidyverse)

filenames <- list.files(path = paste0(wd,Year,'/Data'),
                            recursive = TRUE,
                            pattern = "_processed.csv",
                            full.names = TRUE)
filenames

# This function will add a source file column when importing data. 
read_csv_filename <- function(filename){
  ret <- read.csv(filename)
  ret$Source <- filename 
  ret
}

# Import all files in filenames list
NH4.source <- ldply(filenames, read_csv_filename)

# Remove source file for processing. Source is helpful to know but cumbersome 
NH4 <- subset(NH4.source, select = -Source)
head(NH4)

NH4$Full_ID <- paste(NH4$Plot, NH4$Depth, NH4$Month, NH4$Year, sep = "_")




## Find samples that were reran and select the one that is within range
## Want a separate datafile of samples that were reran and still adl/bdl that we need to rerun again 


##Extract all samples that were reran 
reran <- NH4[duplicated(NH4$Full_ID) | duplicated(NH4$Full_ID, fromLast = TRUE), ]

##Extract the ones that were reran and are now within the range 
reran_fixed <- subset(reran, NH3_range == "Within_Range")
reran_fixed_list <- reran_fixed$Full_ID

##Extract all data that was flagged 
flagged <- (subset(NH4, NH3_range == "bdl" | NH3_range == "adl"))

##Remove samples that were flagged but then were reran and within range 
need_to_rerun <- flagged[!flagged$Full_ID %in% reran_fixed_list, ]

##Check for duplicates 
duplicated(need_to_rerun$Full_ID)

##Remove all duplicates except the last run 
need_to_rerun_nodup <- need_to_rerun[!duplicated(need_to_rerun$Full_ID, fromLast = TRUE), ]

#Export samples that need to be rerun
write.csv(need_to_rerun_nodup, "SMARTX_NH4_need_to_rerun.csv")



##Keep only most recent run of sample on SEAL##
NH4_nodup <- NH4 %>%
  group_by(Full_ID) %>%
  slice_max(Run_date)

NH4_nodup

final <- NH4_nodup
colnames(final)[colnames(final) == "NH3_uM"] ="NH4.uM"




# Final Data QAQC ---------------------------------------------------------
# Merge with Plot Meta Data to put samples in order and check for missing values 
qaqc <- merge(final,meta, by=c("Month","Plot","Depth"), all.y = TRUE)

# Check for missing values
qaqc$flag <- ifelse(is.na(qaqc$NH4.uM) | !complete.cases(qaqc$NH4.uM) | qaqc$NH4.uM == "0", "Missing Value", "OK") 

#Add flag for detection limit
qaqc$"flag.NH3" <- qaqc$NH3_range 

# Subset Samples with QAQC Flags and Export
qaqc.fail <- subset(qaqc, qaqc$flag != "OK" | qaqc$flag.NH3 != "Within_Range")

#Export QAQC File 
setwd(paste0(wd,Year,"/Data", sep=""))
write.csv(qaqc.fail, paste0(Project," Porewater NH4 ", Year, " QAQC.csv"), row.names = FALSE)

{ stop('Check and Manually Fix QAQC Errors')}





# Organize and Export Final Data File -------------------------------------

# Left join with metadata file puts samples in the correct order
meta$"Year" <- Year
meta$Plot <- as.character(meta$Plot)
meta$Depth <- as.character(meta$Depth)
for.transfer <- left_join(meta, final, by=c("Year","Month","Plot","Depth"))

# Replace Negative NH4 Concentration with 0
for.transfer$NH4.uM <-ifelse(for.transfer$NH4.uM <= 0 & for.transfer$NH4.uM != -99 ,0,for.transfer$NH4.uM)


# Replace missing values with -99
for.transfer$NH4.uM <-ifelse(is.na(for.transfer$NH4.uM),"-99",for.transfer$NH4.uM)


# Add Sample-ID
for.transfer$SampleID <- paste(for.transfer$Plot, for.transfer$Depth, sep = "-")




# Reorganize columns with project specific format
# SMARTX
if (Project=="SMARTX"){
  for.transfer <- subset(for.transfer, select = c(Year, Month, Plot, Temperature, CO2, Vegetation, Depth,SampleID, NH4.uM))
}

# CO2xN 
if (Project=="CO2xN"){
  for.transfer <- for.transfer  %>% separate(Plot, into = c("Community", "Chamber"),sep = 1)
  for.transfer <- subset(for.transfer, select = c(Year, Month, Chamber, CO2, Nitrogen, Treatment, Depth,SampleID, NH4.uM))
}

# CO2xCommunity
if (Project=="CO2xCommunity"){
  for.transfer <- for.transfer  %>% separate(Plot, into = c("Community", "Chamber"),sep = 2)
  for.transfer <- subset(for.transfer, select = c(Year, Month, Community, Chamber, Treatment, Depth,SampleID, NH4.uM))
}

# Phrag
if (Project=="Phrag"){
  for.transfer <- for.transfer  %>% separate(Plot, into = c("Community", "Chamber", "Well"),sep = c(1, -1), remove = FALSE)
  for.transfer <- subset(for.transfer, select = c(Year, Month, Chamber, CO2, Nitrogen, Treatment,Well, Depth,SampleID, NH4.uM))
}

## Export data as .csv file
setwd(paste0(wd, Year,'/Data', sep=''))
write.csv(for.transfer,file=paste("0-",Project,' Porewater NH4 ',Year,' (Compiled - For Transfer).csv',sep=''),row.names=F)

