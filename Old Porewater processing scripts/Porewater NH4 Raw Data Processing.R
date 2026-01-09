#######################################################################################
### R script to import LTREB and SMARTX Ammonium porewater data from 
### annual excel file and export in umol S /L
##############################################################################################

# Created 11 August 2023, adapted from Stephanie J. Wilson's and Alia Al-Haj's porewater NH4 script 
# Updated 19 April 2024, for use with all projects. 


# Set Up ------------------------------------------------------------------

##Clear environment
rm(list=ls()) 

# load packages
library(ggplot2)
library(dplyr)
library(data.table)
library(matrixStats)
library(gridExtra)
library(ggpubr)
library(grid)
library(readxl)
library(tidyverse)
library(tidyr)

## Change these parameters before running the script##
File <- 'September_2023_SMARTX_NH4_Run1'
Year <- 2024
Project <- "SMARTX"

date_processed <- '04.19.2024'
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


# Import SEAL Ammonium Raw Data and Standards ---------------------------------
setwd(paste0(wd,'/4-NH4/',Year,'/Data', sep=''))
dat <- read_excel(paste0(File,'.xlsx'))

#Quick look at dataframe
head(dat)

#take out only the columns that we need 
dat1 <- dat[ ,c(1,4,6,7, 12, 13, 15)]

# assigning new names to the columns of the data frame
colnames(dat1) <- c('Run_Info','Sample_Name','Conc', "Abs", "Units", "Test", "Run_date")
head(dat1)

#Pull out standards 
stds <- dat1[dat1$Sample_Name %like% "Standard", ]
head(stds)

#Pull out samples 
samp <- dat1[dat1$Sample_Name %like% "_", ]
head(samp)

#Pull out pe check 
pe <- dat1[dat1$Sample_Name %like% "pe", ]
head(pe)

#Add in an if then statement that tells us if samples are within the range of the test
samp$NH3_range <- ifelse(samp$Abs<min(stds$Abs), "bdl",  ifelse(samp$Abs>max(stds$Abs), "adl", "Within_Range")) 

#Check that all samples are "Within_Range"
samp$NH3_range 



##Third Party Standard Check##
pechk_pred_NH3 = 0.948

#Calculate recovery of pechk
pe$Recovery <- ((pe$Conc)/pechk_pred_NH3)*100

##Check that pe recovery is 80-120% of predicted concentration
pe$Recovery



## Constants 

N_mw <- 18.039   # molecular weight of NH3 

Con1 <- 1000       # conversion factor value

Con2 <- 1000000    # conversion factor value 


## Convert Data from mg/L to uM 
head(samp)

samp$Conc_uM <- (((as.numeric(samp$Conc))/Con1)/N_mw)*Con2
head(samp)


## Pull all data back together

#pull out the columns we want from each dataframe 
all_data <- samp[ ,c("Sample_Name", "Conc", "Abs", "Run_date", "NH3_range", "Conc_uM") ]
head(all_data)

colnames(all_data) <- c("Sample_Name", "NH3_mgL", "Absorbance", "Run_date", "NH3_range", "NH3_uM")
head(all_data)



## Take an initial look at concentrations 

#plot data to get a sense of any outliers 
NH3look <- ggplot(data=all_data, aes(x=Sample_Name, y=NH3_uM)) +
  geom_bar(stat="identity") + 
  theme_classic() + ylim(-10, 200) + 
  theme(legend.position="none") + 
  ggtitle("Sample NH3 Concentrations")
NH3look



## Pull out data you need, make IDs 
head(all_data)

#pull the sample ID and separate it by the underscores 
all_data <- all_data %>% separate(Sample_Name, c("Plot", "Depth", "Month", "Year"), "_")

##Add the SampleID
all_data$SampleID <- paste(all_data$Plot, all_data$Depth, sep = "-")
head(all_data)



# Export data -------------------------------------------------------------

## Set working directory
setwd(paste0(wd,'/4-NH4/',Year,'/Data', sep=''))

## Export data as .csv file
write.csv(all_data, file=paste0(File,'_processed.csv'),row.names=F)



