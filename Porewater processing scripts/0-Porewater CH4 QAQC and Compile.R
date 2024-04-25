#  Merging Porewater Ch4 Data and QAQC ------------------------------------

### R script to merge all previously processed csv files from 
### (CH4 porewater processing script.R) for the same year ###

# Created 11 Nov 2020 by SK #
# Updated 13 May 2023 by EF  to add QAQC #
# Updated by ZR 7 August 2023 to only keep reruns #


##Clear environment
rm(list=ls()) 

# 1. Set Up ------------------------------------------------------------------
library(readxl)
library(dplyr)
library(plyr)
library(tidyr)
library(DescTools)
library(tidyverse)


## Change these parameters before running the script##
Year <- 2023
Project <- "SMARTX"

max.CH4 <- 400 # Maximum value of normal CH4 umol range
min.CH4 <- 4 # Minimum value of normal CH4 umol range







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





# 3a. Read in CH4 data --------------------------------------------------
setwd(paste0(wd,'/3-CH4/',Year,'/Data', sep=''))

filenames <- list.files(pattern='_processed.csv')
filenames

# This function will add a source file column when importing data. 
read_csv_filename <- function(filename){
  ret <- read.csv(filename)
  ret$Source <- filename 
  ret
}

# Import all files in filenames list
CH4.source <- ldply(filenames, read_csv_filename)

##Remove reruns
CH4.source <- CH4.source[!grepl("Reruns", CH4.source$Source),]
unique(CH4.source$Source)

# Remove source file for processing. Source is helpful to know but cumbersome 
CH4_orig <- subset(CH4.source, select = -Source)
head(CH4_orig)

#All original samples
CH4_orig$"Type" <- "Original"

# 3b. Read in CH4 Reruns --------------------------------------------------
filenames2 <- list.files(pattern='Reruns)_processed.csv')
filenames2

# Import all files in filenames list
Reruns.source <- ldply(filenames2, read_csv_filename)
Reruns <- subset(Reruns.source, select = -Source)

##Remove std checks and blanks 
Reruns2 <- Reruns[!grepl("CHK", Reruns$Plot),]
Reruns2 <- Reruns2[!grepl("BLANK", Reruns2$Plot),]
Reruns2 <- Reruns2[!grepl("blank", Reruns2$Plot),]

Reruns2$"Type" <- "Rerun"

head(Reruns2)


##Combine original and reran samples
CH4 <- rbind(CH4_orig, Reruns2)



##Continue below to remove original values and only keep reruns

##Extract duplicates
CH4$"ID" <- paste(CH4$Month, "-", CH4$Plot, "-", CH4$Depth)
CH4_dups <- CH4[duplicated(CH4$ID), ]

##Remove std checks and blanks 
CH4_dups <- CH4_dups[!grepl("CHK", CH4_dups$Plot),]
CH4_dups <- CH4_dups[!grepl("BLANK", CH4_dups$Plot),]
CH4_dups <- CH4_dups[!grepl("blank", CH4_dups$Plot),]

#Remove original values
CH4_reruns <- subset(CH4_dups, Type == "Rerun")

##Remove duplicates from original dataframe
CH4_nodups <- CH4 %>%
  group_by(ID) %>%
  filter(n() == 1)

CH4_nodups_reruns <- rbind(CH4_nodups, CH4_reruns)

##Check for duplicates - this should give a datafram with 0 values. 
d <- CH4_nodups_reruns[duplicated(CH4_nodups_reruns$ID), ]

##Rename dataframe
CH4 <- CH4_nodups_reruns






# 4. Check CH4 Values ------------------------------------------------------
# Merge with Plot Meta Data to put samples in order and check for missing values 

CH4.QA <- merge(CH4,meta, by=c("Year","Month","Plot","Depth"), all.y = TRUE)

# Check for missing values
CH4.QA$Missing.Value <- ifelse(is.na(CH4.QA$pw_umol_CH4) | !complete.cases(CH4.QA$pw_umol_CH4) | CH4.QA$pw_umol_CH4 == "0", 1, 0)

# Check for CH4 values outside the normal range 
CH4.QA$Normal.Range <- ifelse(CH4.QA$pw_umol_CH4 < min.CH4, 1,
                       ifelse(CH4.QA$pw_umol_CH4 > max.CH4, 1, 0))

# Subset all rows with failed QAQC
CH4.fail <- subset(CH4.QA, Missing.Value== 1 | Normal.Range == 1 )

# Export QAQC 
setwd(paste0(wd,'/3-CH4/',Year,'/Data', sep=''))
write.csv(CH4.fail, paste0(Project," Porewater CH4 ", Year, " QAQC.csv"), row.names = FALSE)

if(nrow(CH4.fail)!=0) { stop('CH4 values missing or outside normal range.')}






# 5. Generate Output ---------------------------------------------------------

meta$Plot <- as.character(meta$Plot)

# Left join with meta puts samples in the correct order
final <- left_join(meta, CH4, by=c("Year","Month","Plot","Depth"))

# Simplify Column Heading
final$umol_CH4 <- final$pw_umol_CH4

# Add Sample-ID
final$SampleID <- paste(final$Plot, final$Depth, sep = "-")

# Replace missing values with -99
final$umol_CH4 <-ifelse(is.na(final$umol_CH4),"-99",final$umol_CH4)


# Reorganize columns with project specific format
# SMARTX
if (Project=="SMARTX"){
  final <- subset(final, select = c(Year, Month, Day, Plot, Temperature, CO2, Vegetation, Depth,SampleID, umol_CH4, Type))
}

# CO2xN 
if (Project=="CO2xN"){
  final <- final  %>% separate(Plot, into = c("Community", "Chamber"),sep = 1)
  final <- subset(final, select = c(Year, Month, Day, Chamber, CO2, Nitrogen, Treatment, Depth,SampleID, umol_CH4))
}

# CO2xCommunity
if (Project=="CO2xCommunity"){
  final <- final  %>% separate(Plot, into = c("Community", "Chamber"),sep = 2)
  final <- subset(final, select = c(Year, Month, Day, Community, Chamber, Treatment, Depth,SampleID, umol_CH4))
}

# Phrag
if (Project=="Phrag"){
  final <- final  %>% separate(Plot, into = c("Community", "Chamber", "Well"),sep = c(1, -1), remove = FALSE)
  final <- subset(final, select = c(Year, Month, Day, Chamber, CO2, Nitrogen, Treatment,Well, Depth,SampleID, umol_CH4))
}


## Export data as .csv file
setwd(paste0(wd,'/3-CH4/',Year,'/Data', sep=''))
write.csv(final,file=paste('0-',Project,' Porewater CH4 ',Year,' (Compiled - For Transfer).csv',sep=''),row.names=F)


