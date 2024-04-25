##################################################################
### R script to import LTREB and SMARTX Sulfide porewater data from 
### annual excel file and export in umol S /L
##################################################################
## processes one sheet at a time ##

# Created 19 March 2021, adapted from porewater CH4 script 
# Updated 24 March 2023, for new file format. Also add standard log.


# Set Up ------------------------------------------------------------------
# load packages
library(readxl)
library(tidyverse)
library(data.table)



##Clear environment
rm(list=ls()) 

## Change these parameters before running the script##
File <- '5_SMARTX_Sulfide_(May 452)'
Year <- 2016
Project <- "SMARTX"

date_processed <- '10.24.2023'
Personal <- 'ZR'



# Set Working Directory  --------------------------------------------------
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

setwd(wd)





# Import Sulfide Raw Data and Standards ---------------------------------
setwd(paste0(wd,'/1-Sulfide/',Year,'/Data', sep=''))
Sulfide_raw_data <- read_excel(paste0(File,'.xlsx'),range=cell_cols("A:F"))

#Sets all mV values to be numeric, suppressWarnings ignores the "NAs introduced by coersion" warning message
Sulfide_raw_data$mV <- suppressWarnings(as.numeric(Sulfide_raw_data$mV))

#Imports  Sulfide stds from sheet in excel file. 
standards_raw_data <- read_excel(paste0(File,'.xlsx'),range=cell_cols("H:L"))





# Standard Curve ----------------------------------------------------------

# only use standard values that are good (designated "Y" in "keep" column of excel sheet)
standards_raw_data <- subset(standards_raw_data,Keep=="Y") 

# Generate linear model (regression) using X and Y
mod <- lm(StdmV ~ log10_sulfide_ppm,data=standards_raw_data)

# Extract and check R2 (displays value in console). Script will not run if R2 < 0.95
r2 <- summary(mod)$r.squared
print(r2)
if(r2 < 0.95) { stop('Standard curve is not usable: R2 < 0.95')}

# Plot standard curve
plot(StdmV ~ log10_sulfide_ppm,data=standards_raw_data,pch=19)
abline(mod,col='blue')

# Extract y-intercept from standard curve
Intercept <- summary(mod)$coefficients[1]

# Extract slope from standard curve
Slope <- summary(mod)$coefficients[2]





# Calculate Sulfide Concentration  ----------------------------------------

# Convert peak area to ppm Sulfide. ppm S = (area-incercept)/slope
Sulfide_raw_data$Sulfide_ppm <- 10^((Sulfide_raw_data$mV-Intercept)/Slope)


# Convert ppm S to mmol S/L
# S (ppm) is multiplied by two to account for 
# dilution factor caused by using equal amounts of sample and SAOB; 
# Divided by 32.07 (molar mass of Sulfide) to convert to moles of Sulfide.

Sulfide_raw_data$mM_Sulfide <- Sulfide_raw_data$Sulfide_ppm*2/32.07


# mM to uM
# Same units as in LTREB published data (umol S/L)
Sulfide_raw_data$uM_Sulfide <- Sulfide_raw_data$mM_Sulfide*1000








# Export data -------------------------------------------------------------

## Set working directory
setwd(paste0(wd,'/1-Sulfide/',Year,'/Data', sep=''))

## Export data as .csv file
write.csv(Sulfide_raw_data,file=paste0(File,'_processed.csv'),row.names=F)







# Add Standards to Log ----------------------------------------------------

#Read in log file
setwd(paste0(wd,'/1-Sulfide/',Year,'/Data/Processing Log', sep=""))
std_log <- read.csv('Sulfide Processing Log.csv')

#Generate std info for this run
stds_info <- as.data.frame(cbind(
  # When/who did this run
  Date_processed=date_processed,
  Personal=Personal,
  # ID for this run
  Run=File,
  # Range for standards
  STD_Levels=paste(standards_raw_data$StdSulfide_ppm,collapse = ","),
  # Stats for low standards: R2, Slope, Intercept
  R2=round(r2,3),
  Slope=round(Slope,2),
  Intercept=round(Intercept,2)))
  

#Merge and save file
std_new <- rbind(std_log,stds_info)
print(std_new)
write.csv(std_new,file=paste0('Sulfide Processing Log.csv'),row.names=F)







