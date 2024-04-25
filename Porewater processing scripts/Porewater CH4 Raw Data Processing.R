##################################################################
### R script to import LTREB and SMARTX CH4 porewater data from 
### annual excel file and export in umol CH4 /L ###
##################################################################
## processes one sheet at a time ##

# Created 9 Nov 2020 by SK #
# Updated by GN 28 April 2022 to separate high and low std curves #
# Updated by GN 13 May 2022 to add method for customizing std curves #
# Updated by GN 2 June 2022 to save plots of standard curves and 
# create log file with std curve details #
# Updated by EF 3 March 2023 for new file organization #
# Updated by ZR 7 August 2023 for QAQC standardization #
# Assumes that all CH4 is driven into the headspace by shaking #

##################################################################



# Load packages
library(readxl)
library(tidyverse)
library(png)
library(ggplot2)
library(dplyr)
library(data.table)
library(matrixStats)
library(gridExtra)
library(ggpubr)
library(grid)
library(tidyr)
library(stringr)
library(lubridate)


# # Set Up ------------------------------------------------------------------
# # Edit the following lines every time a new file is run 
# 
# # Name of experiment 
# # Year Samples where collected
# # Name of the Excel sheet to be processed
# # Add details about who did this run
# 
Project <- "SMARTX"
Year <- 2023
File <- '9_SMARTX_CH4_(September Reruns)'
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




# Setting Standard Levels -------------------------------------------------
# SMARTX and LTREB use different sets of standards, based on their relative
# porewater concentrations, which means the "low" vs "high" standard curves 
# are also different. As of 5/12/2022, the typical standards are:

# LTREB
# Low: 50-500 ppm
# High: 500-5000 ppm
# Samples <=500 ppm use low curve, samples >500 ppm use high curve

# Low: 100-1000 ppm
# High: 1000-10000 ppm
# Samples <=1000 ppm use low curve, samples >1000 ppm use high curve

# To account for this, the 'max.low.std' value must be set for each experiment
# LTREB: 500
# SMARTX: 1000

if (Project=="SMARTX"){
    max.low.std <- 1000}

if (Project=="CO2xN" | Project=="CO2xCommunity" | Project=="Phrag" | Project=="LTREB"){ 
  max.low.std <- 500 }



# Import Data -------------------------------------------------------------
setwd(paste0(wd,'/3-CH4/',Year,'/Data', sep=''))


# Change name of excel file. Imports raw CH4 data from sheet in excel file. 
CH4_raw_data <- read_excel(paste0(File,'.xlsx'),range=cell_cols("A:L"))


# Import  CH4 stds from sheet in excel file. 
standards_raw_data <- read_excel(paste0(File,'.xlsx'),range=cell_cols("N:Q"))

# Only use standard values that are good (designated "Y" in "keep" column of excel sheet)
standards_raw_data <- subset(standards_raw_data,Keep=="Y") 



# Standard Curves  --------------------------------------------------------
# Define low and high standard curves
# NOTE: This requires that you correctly set max.low.std in Set Up above
standards_raw_data_low = 
  standards_raw_data %>% 
  filter(StdCH4_ppm<=max.low.std)
standards_raw_data_high = 
  standards_raw_data %>% 
  filter(StdCH4_ppm>=max.low.std)

# Generate linear model (regression) using standard area and concentration
mod.low <- lm(StdArea ~ StdCH4_ppm,data=standards_raw_data_low)
summary(mod.low)
mod.high <- lm(StdArea ~ StdCH4_ppm,data=standards_raw_data_high)
summary(mod.high)

# Extract and check R2 (displays value in console). Stop if either R2 < 0.95
r2.low <- summary(mod.low)$r.squared
print(r2.low)
r2.high <- summary(mod.high)$r.squared
print(r2.high)

# Extract y-intercept from standard curve
Intercept.low <- summary(mod.low)$coefficients[1]
Intercept.high <- summary(mod.high)$coefficients[1]

# Extract slope from standard curve
Slope.low <- summary(mod.low)$coefficients[2]
Slope.high <- summary(mod.high)$coefficients[2]

# Plot and save standard curves
setwd(paste0(wd,'/3-CH4/',Year,'/Data/Processing Log', sep=""))

png(paste0(File,'_standards.png'),width=600,height=400)
par(mfrow=c(1,2))
plot(StdArea ~ StdCH4_ppm,data=standards_raw_data_low,pch=19)
abline(mod.low,col='blue')
mtext(side=3,paste0('R2 = ',round(r2.low,3)),line=1.5)
mtext(side=3,paste0('Area = ',round(Slope.low,2),' * Conc. + ',round(Intercept.low,2)))
plot(StdArea ~ StdCH4_ppm,data=standards_raw_data_high,pch=19)
abline(mod.high,col='blue')
mtext(side=3,paste0('R2 = ',round(r2.high,3)),line=1.5)
mtext(side=3,paste0('Area = ',round(Slope.high,2),' * Conc. + ',round(Intercept.high,2)))
dev.off()

if(r2.low < 0.95) { stop('Standard curve is not usable: R2.low < 0.95')}
if(r2.high < 0.95) { stop('Standard curve is not usable: R2.high < 0.95')}



# Calculate CH4 Concentration ---------------------------------------------

# Use the max low std as the cut-off for low vs high std curve
# This is typically 1000 for SMARTX and 500 for LTREB, but needs
# to be set at the beginning of the script
area.cutoff <- mean((max.low.std*Slope.low+Intercept.low),(max.low.std*Slope.high+Intercept.high))



# Convert peak area to ppm CH4. ppm CH4 = (area-intercept)/slope
CH4_raw_data$CH4_ppm <- NA
CH4_raw_data$Area <- as.numeric(CH4_raw_data$Area)
for (i in 1:nrow(CH4_raw_data)) {
  if (is.na(CH4_raw_data$Area[i])) {
    CH4_raw_data$CH4_ppm[i]=NA
  } else {
    if (CH4_raw_data$Area[i]<=area.cutoff) {
      CH4_raw_data$CH4_ppm[i]=(CH4_raw_data$Area[i]-Intercept.low)/Slope.low
    } else {
      CH4_raw_data$CH4_ppm[i]=(CH4_raw_data$Area[i]-Intercept.high)/Slope.high
    }
  }
}


## Dilution correct samples 
#multiply the concentration by the dilution factor
CH4_raw_data$CH4_Conc_ppm_dilcorr <- (CH4_raw_data$CH4_ppm * CH4_raw_data$Dilution_factor_adj)

#check results
CH4_raw_data

# Convert ppm CH4 to total umol CH4 in the sample headspace
# use Ideal Gas Law:  
#   hdsp umol CH4 = ppm CH4*(PV/RT) 
#      P = 1 (pressure in atm)
#      R = 0.082 (gas constant)
#      V = 0.015 (gas syringe volume in L)
#      T = "TempC" column value in Kelvin (equilibration temperature)

CH4_raw_data$hdsp_umol_CH4 <- CH4_raw_data$CH4_Conc_ppm_dilcorr*((1*CH4_raw_data$Sample_mL/1000)/(0.08206*(CH4_raw_data$TempC+273.15)))


# Convert headspace CH4 to dissolved umol/L Ch4 in sample
# Same units as in LTREB published data
CH4_raw_data$pw_umol_CH4 <- CH4_raw_data$hdsp_umol_CH4/(CH4_raw_data$Sample_mL/1000)


#Blanks

#Pull out blanks
Blanks <- CH4_raw_data[CH4_raw_data$Plot %like% "BLANK", ]

#RSV_BTOC
RSV_Blanks <- abs((sd(Blanks$CH4_ppm)/mean(Blanks$CH4_ppm))*100)

if (mean(Blanks$CH4_ppm) > 5) {
  print("rerun")
} else {
  print("continue")
}


#Check Standards

#Pull out Check Standards
CHKSTDS <- CH4_raw_data[CH4_raw_data$Plot %like% "CHK", ]

#RSV_BTOC
RSV_CS <- (sd(CHKSTDS$CH4_ppm)/mean(CHKSTDS$CH4_ppm))*100

if (RSV_CS > 10) {
  print("rerun GC")
} else {
  print("continue")
}


#Check that samples are withing the standard range 
CH4_raw_data$Within_range <- NA
CH4_raw_data$Area <- as.numeric(CH4_raw_data$Area)
for (i in 1:nrow(CH4_raw_data)) {
  if (is.na(CH4_raw_data$Area[i])) {
    CH4_raw_data$CH4_ppm[i]=NA
  } else {
    if (CH4_raw_data$Area[i]<=area.cutoff) {
      CH4_raw_data$CH4_ppm[i]=(CH4_raw_data$Area[i]-Intercept.low)/Slope.low
    } else {
      CH4_raw_data$CH4_ppm[i]=(CH4_raw_data$Area[i]-Intercept.high)/Slope.high
    }
  }
}

CH4_raw_data$CH4_range <- ifelse(CH4_raw_data$CH4_Conc_ppm_dilcorr < 100, "bdl",  ifelse(CH4_raw_data$CH4_Conc_ppm_dilcorr > 10000, "adl", "Within_Range"))


# Export Data -------------------------------------------------------------

## Set working directory
setwd(paste0(wd,'/3-CH4/',Year,'/Data', sep=""))

## Export data as .csv file
write.csv(CH4_raw_data,file=paste0(File,'_processed.csv'),row.names=F)







# Add Standards to Log ----------------------------------------------------

#Read in log file
setwd(paste0(wd,'/3-CH4/',Year,'/Data/Processing Log', sep=""))
std_log <- read.csv('CH4 Processing Log.csv')

#Generate std info for this run
stds <- as.data.frame(cbind(
  # When/who did this run
  Date_processed=date_processed,
  Personal=Personal,
  # ID for this run
  Run=File,
  # Range for low standards 
  Low_std_levels=paste(standards_raw_data_low$StdCH4_ppm,collapse = "."),
  # Stats for low standards: R2, Slope, Intercept
  Low_std_R2=round(r2.low,3),
  Low_std_Slope=round(Slope.low,2),
  Low_std_Intercept=round(Intercept.low,2),
  # Range for high standards
  High_std_levels=paste(standards_raw_data_high$StdCH4_ppm,collapse = "."),
  # Stats for low standards: R2, Slope, Intercept
  High_std_R2=round(r2.high,3),
  High_std_Slope=round(Slope.high,2),
  High_std_Intercept=round(Intercept.high,2)))

#Merge and save file
std_new <- rbind(std_log,stds)
std_new
write.csv(std_new,file=paste0('CH4 Processing Log.csv'),row.names=F)







