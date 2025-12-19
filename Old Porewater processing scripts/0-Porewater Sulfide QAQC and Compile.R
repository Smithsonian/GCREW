# POREWATER Sulfide QAQC 

##Clear environment
rm(list=ls()) 


# 1. Set Up ------------------------------------------------------------------
library(readxl)
library(dplyr)
library(plyr)


## Change these parameters before running the script##
Year <- 2016
Project <- "SMARTX"

max.sulfide <- 6.0 # Maximum value of normal Sulfide mM range
min.sulfide <- 0.05 # Minimum value of normal Sulfide mM range


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


# 3. Read in Sulfide Files --------------------------------------------------
setwd(paste0(wd,'/1-Sulfide/',Year,'/Data', sep=''))

filenames <- list.files(pattern='_processed.csv')
filenames

# This function will add a source file column when importing data. 
read_csv_filename <- function(filename){
  ret <- read.csv(filename)
  ret$Source <- filename 
  ret
}

# Import all files in filenames list
sulfide.source <- ldply(filenames, read_csv_filename)

# Remove source file for processing. Source is helpful to know but cumbersome 
sulfide <- subset(sulfide.source, select = -Source)
head(sulfide)

meta$Month <- meta$ï..Month
meta$ï..Month <- NULL




# 4. Check Sulfide Values ------------------------------------------------------
# Merge with Plot Meta Data to put samples in order and check for missing values 

sulfide.QA <- merge(sulfide,meta, by=c("Year","Month","Plot","Depth"), all.y = TRUE)

# Check for missing values
sulfide.QA$Missing.Value <- ifelse(is.na(sulfide.QA$mM_Sulfide) | !complete.cases(sulfide.QA$mM_Sulfide) | sulfide.QA$mM_Sulfide == "0", 1, 0)

# Check for pH values outside the normal range 
sulfide.QA$Normal.Range <- ifelse(sulfide.QA$mM_Sulfide < min.sulfide, 1,
                           ifelse(sulfide.QA$mM_Sulfide > max.sulfide, 1, 0))

# Subset all rows with failed QAQC
sulfide.fail <- subset(sulfide.QA, Missing.Value== 1 | Normal.Range == 1 )

# Export QAQC 
setwd(paste0(wd,'/1-Sulfide/',Year,'/Data', sep=''))
write.csv(sulfide.fail, paste0(Project," Porewater Sulfide ", Year, " QAQC.csv"), row.names = FALSE)

if(nrow(sulfide.fail)!=0) { stop('Sulfide values missing or outside normal range.')}






# 5. Generate Output ---------------------------------------------------------
# Left join with meta puts rows in the correct order and keeps all the rows of meta 
final <- left_join(meta, sulfide, by=c("Year","Month","Plot","Depth"))

# Add Sample-ID
final$SampleID <- paste(final$Plot, final$Depth, sep = "-")

# Replace missing values with -99
final$mM_Sulfide <-ifelse(is.na(final$mM_Sulfide),"-99",final$mM_Sulfide)


# Reorganize columns with project specific format
# SMARTX
if (Project=="SMARTX"){
  final <- subset(final, select = c(Year, Month, Day, Plot, Temperature, CO2, Vegetation, Depth,SampleID, mM_Sulfide))
}

# CO2xN 
if (Project=="CO2xN"){
  final <- final  %>% separate(Plot, into = c("Community", "Chamber"),sep = 1)
  final <- subset(final, select = c(Year, Month, Day, Chamber, CO2, Nitrogen, Treatment, Depth,SampleID, mM_Sulfide))
}

# CO2xCommunity
if (Project=="CO2xCommunity"){
  final <- final  %>% separate(Plot, into = c("Community", "Chamber"),sep = 2)
  final <- subset(final, select = c(Year, Month, Day, Community, Chamber, Treatment, Depth,SampleID, mM_Sulfide))
}

# Phrag
if (Project=="Phrag"){
  final <- final  %>% separate(Plot, into = c("Community", "Chamber", "Well"),sep = c(1, -1), remove = FALSE)
  final <- subset(final, select = c(Year, Month, Day, Chamber, CO2, Nitrogen, Treatment,Well, Depth,SampleID, mM_Sulfide))
}

## Export data as .csv file
setwd(paste0(wd,'/1-Sulfide/',Year,'/Data', sep=''))
write.csv(final,file=paste("0-",Project,' Porewater Sulfide ',Year,' (Compiled - For Transfer).csv',sep=''),row.names=F)

