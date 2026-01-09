# Dionex Data Processing Code  --------------------------------------------

# This code formats Dionex raw data files, calculates salinity and checks 
# duplicate and spiked samples. 

# By: SJW
# Updated: 4/14/2023 for GCREW Samples EFien 





# DAILY SET-UP ------------------------------------------------------------
# Load data.table package
library(ggplot2)
library(dplyr)
library(data.table)
library(matrixStats)
library(gridExtra)
library(ggpubr)
library(grid)
library(reshape2) # Erin Added
library(readxl)
library(plyr)

#### Edit the following lines every time a new file is run 
####

# Name of experiment 
# Year Samples where collected
# Name of the Excel sheet to be processed
# Add details about who did this run
# Year Samples where collected
Project <- "SMARTX" 
Year <- 2023
File <- '8_SMARTX_SO4_Cl_(May 332 Reruns)'
date.run <- '11.02.2023'
run.by <- 'ZR'
date.processed <- '11.02.2023'
processed.by <- 'ZR'

Dilution <- 100 # Sample Dilution factor. Determined with lab protocol. 
cutoff <- 10 # Cut off level for evaluating Duplicate %Diff and CV  

####
#### 

# Set Working Directory
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






# 1. Import and Format Dionex Raw Data --------------------------------------------------
setwd(paste0(wd,'/5-Sulfate & Chloride/',Year,'/Data', sep=''))

# Imports raw data from excel file. 
raw <- read_excel(paste0(File,'.xlsx'),range = cell_rows(c(4, NA)))

# Check Raw File Format 
if(ncol(raw) != 13) { stop('Incorrect Number of Columns: Check raw data file format.')}

## If raw file was not saved with the proper format the wrong data will be processed. 
raw <- raw[ ,c(2,5,7,10,12)]
head(raw)

## Name and format columns correctly. NA introduced is OKAY! 
colnames(raw) <- c( "Sample_ID", "SO4_ppm", "SO4_area","Cl_ppm", "Cl_area" )
raw$Sample_ID <- as.factor(raw$Sample_ID)
raw$SO4_ppm <- as.numeric(raw$SO4_ppm)
raw$SO4_area <- as.numeric(raw$SO4_area)
raw$Cl_ppm <- as.numeric(raw$Cl_ppm)
raw$Cl_area <- as.numeric(raw$Cl_area)
raw <- as.data.frame(raw)
head(raw)

## Remove empty lines 
all_dat <- raw[!(is.na(raw$Sample_ID) | raw$Sample_ID==""), ]
head(all_dat)


##Blanks 

## Pull out blanks and check
blanks <- all_dat[grepl("Lab Blank", all_dat$Sample_ID),]
head(blanks)


#Check mean of blanks to make sure they are low *CV doesn't work for this bc concentrations are so low*

blanks_chk_S <- blanks %>%
  group_by(Sample_ID) %>%
  summarise(mean = mean(SO4_ppm, na.rm = TRUE), sd = sd(SO4_ppm, na.rm = TRUE))
blanks_chk_S$cv <- (blanks_chk_S$sd/blanks_chk_S$mean)*100
blanks_chk_S$flag <-  ifelse(blanks_chk_S$mean <0.1, 'YES', 'NO, rerun')
head(blanks_chk_S)

blanks_chk_Cl <- blanks %>%
  group_by(Sample_ID) %>%
  summarise(mean = mean(Cl_ppm, na.rm = TRUE), sd = sd(Cl_ppm, na.rm = TRUE))
blanks_chk_Cl$cv <- (blanks_chk_Cl$sd/blanks_chk_Cl$mean)*100
blanks_chk_Cl$flag <-  ifelse(blanks_chk_Cl$mean <0.1, 'YES', 'NO, rerun')
head(blanks_chk_Cl)




# 2. Extract Standards -------------------------------------------------------

## Pull out standards and check
stds <- all_dat[grepl("Standard", all_dat$Sample_ID),]
head(stds)

stds$SO4_area <- as.numeric(stds$SO4_area)
stds$Cl_area <- as.numeric(stds$Cl_area)


#Check Coefficient of Variation for standards and checks
stds_chk_S <- ddply(stds,~Sample_ID,summarise,mean=mean(SO4_ppm),sd=sd(SO4_ppm))
stds_chk_S$cv <- (stds_chk_S$sd/stds_chk_S$mean)*100
stds_chk_S$flag <-  ifelse(stds_chk_S$cv <10, 'YES', 'NO, rerun')
head(stds_chk_S)

stds_chk_Cl <- ddply(stds,~Sample_ID,summarise,mean=mean(SO4_ppm),sd=sd(SO4_ppm))
stds_chk_Cl$cv <- (stds_chk_Cl$sd/stds_chk_Cl$mean)*100
stds_chk_Cl$flag <-  ifelse(stds_chk_Cl$cv <5, 'YES', 'NO, rerun')
head(stds_chk_Cl)

#Graph data
PSO4_STDS <- ggplot(stds, aes(x=SO4_area, y=SO4_ppm)) + 
  geom_point(size = 3) +
  geom_smooth(method=lm)
PSO4_STDS

PCl_STDS <- ggplot(stds, aes(x=Cl_area, y=Cl_ppm)) + 
  geom_point(size = 3) +
  geom_smooth(method=lm)
PCl_STDS

#Calculate Slope, Intercept
lmS <- lm(stds$SO4_area ~ stds$SO4_ppm)
SO4_sum <- summary(lmS)
SO4_Slope <- SO4_sum$coefficients[2, 1] 
SO4_Intercept <- SO4_sum$coefficients[1, 1]
SO4_R2 <- SO4_sum$adj.r.squared
SO4_Analysis <- "Sulfate"

lmCl <- lm(stds$Cl_area ~ stds$Cl_ppm)
Cl_sum <- summary(lmCl)
Cl_Slope <- Cl_sum$coefficients[2, 1] 
Cl_Intercept <- Cl_sum$coefficients[1, 1] 
Cl_R2 <- Cl_sum$adj.r.squared
Cl_Analysis <- "Chloride"

##Put data into slope QAQC file
# defining a row 
rowSO4 <- data.frame(SO4_Analysis,SO4_Slope,SO4_Intercept,SO4_R2)
rowCl <- data.frame(Cl_Analysis,Cl_Slope,Cl_Intercept,Cl_R2)




# 3. Calculate mmol/L Concentrations -----------------------------------------
#remove standards from sample dataframe
sample.dat <- all_dat[!grepl("Standard", all_dat$Sample_ID),]
head(sample.dat)

## Constants needed: 
#molecular weight of Chloride: 35.45 
clmw <- 35.45 
#molecular weight of Sulfur: 32.06
smw  <- 32.06 

## Convert to mmol/L
sample.dat$SO4_mM <-(sample.dat$SO4_ppm/smw)
sample.dat$Cl_mM  <-(sample.dat$Cl_ppm/clmw)
head(sample.dat)

## Calculate Salinity 
# calculated using the Knudsen equation 
# Salinity = 0.03 + 1.8050 * Chlorinity
# Ref: A Practical Handbook of Seawater Analysis by Strickland & Parsons (P. 11)
# =((1.807*Cl_ppm)+0.026)/1000

sample.dat$salinity <- ((1.8070 * sample.dat$Cl_ppm) + 0.026) / 1000
head(sample.dat)






# 4. Pair Duplicated Samples -------------------------------------------------
# Separate Sample ID's into Plot, Depth, Month and Test(DUP or Spike)
# Sample_Id must be in the correct format for this to work 
id=with(sample.dat,colsplit(Sample_ID,'_',c('Plot','Depth', 'Month','Test')))
head(id)

# Add the ID's back together without DUP or Spike so the dups and spikes  
# can be paired with their matching original samples
sample.dat <- cbind(sample.dat,id)
sample.dat$ID <- paste(sample.dat$Plot, sample.dat$Depth, sample.dat$Month, sep="_")
rm(id) # don't need id dataframe anymore
head(sample.dat)
 
# Extract the ID's of all the duplicated (or spiked) samples 
ID.QA <- as.data.frame(sample.dat$ID[duplicated(sample.dat$ID)])
colnames(ID.QA) <- c('ID')
head(ID.QA)

# Merge these back together so we have the data for only rows that are dups or spikes 
sample.QA <- merge(sample.dat, ID.QA)
sample.QA <- sample.QA %>% distinct()
sample.QA <- subset(sample.QA, select=-c(Plot, Depth, Month))
head(sample.QA)

# Use the Test Column to Separate original and duplicated samples
sample.QA$Test <- as.factor(sample.QA$Test)

sample.OG<- subset(sample.QA, Test==c('')) #original 
head(sample.OG)

sample.dups <- subset(sample.QA, Test=='DUP') #duplicates 
colnames(sample.dups) <- c('ID', 'Sample_ID_Dup', "SO4_ppm_dup","SO4_area_dup", 'Cl_ppm_dup', "Cl_area_dup",'SO4_mM_dup', 'Cl_mM_dup', 'Salinity_dup','Test')
head(sample.dups)

# Merge by ID so that the Dup values are New columns
dups.QA <- merge(sample.OG, sample.dups, by="ID")
head(dups.QA)






# 5. Check Duplicate % Difference     ----------------------------------------

## Calculate % Difference Based on mM 
dups.QA$SO4_dups_chk <- ((abs(dups.QA$SO4_mM-dups.QA$SO4_mM_dup))/((dups.QA$SO4_mM+dups.QA$SO4_mM_dup)/2))*100
dups.QA$SO4_dups_flag <-  ifelse(dups.QA$SO4_dups_chk <cutoff, 'YES', 'NO, rerun')

dups.QA$Cl_dups_chk <- ((abs(dups.QA$Cl_mM-dups.QA$Cl_mM_dup))/((dups.QA$Cl_mM+dups.QA$Cl_mM_dup)/2))*100
dups.QA$Cl_dups_flag <-  ifelse(dups.QA$Cl_dups_chk <cutoff, 'YES', 'NO, rerun')
head(dups.QA)


## PLOT
#plot dups output as a bar graph to easily check - want any over 10% to be red need to work on this 
dups.Sbar <- ggplot(data = dups.QA, aes(x = ID, y = SO4_dups_chk)) +
  geom_bar(stat = 'identity', aes(fill = ifelse(abs(SO4_dups_chk)<cutoff, 'No', 'Yes'))) + 
  scale_fill_manual(values = c('darkgreen','darkgray')) + 
  theme_bw() + labs(x= "Sample ID", y="Difference Between SO4 Duplicates (%)") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +  
  theme(legend.position="none") +  geom_hline(yintercept=cutoff, linetype="dashed", 
                                              color = "black", linewidth=1)

dups.Clbar <- ggplot(data = dups.QA, aes(x = ID, y = Cl_dups_chk)) +
  geom_bar(stat = 'identity', aes(fill = ifelse(abs(Cl_dups_chk)<cutoff, 'No', 'Yes'))) + 
  scale_fill_manual(values = c('darkgreen','darkgray')) + 
  theme_bw() + labs(x= "Sample ID", y="Difference Between Cl Duplicates (%)") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +  
  theme(legend.position="none") +  geom_hline(yintercept=cutoff, linetype="dashed", color = "black", linewidth=1)

ggarrange(dups.Sbar, dups.Clbar,ncol=2, nrow=1)


# Check for percent of "no, reruns" to see if it  would warrant reruns 
dups.SO4bad <- sum(dups.QA$SO4_dups_flag=="NO, rerun", na.rm = T)
dups.SO4percent <- (dups.SO4bad/nrow(dups.QA))*100 # Calculate percent bad dups

dups.Clbad <- sum(dups.QA$Cl_dups_flag=="NO, rerun", na.rm = T)
dups.Clpercent <- (dups.Clbad/nrow(dups.QA))*100 # Calculate percent bad dups

# Runs with more than 20% bad duplicates should be rerun. 
if(dups.SO4percent > 20) { stop('Duplicate Test Fails: Greater than 20% of duplicates do not match.')}
if(dups.Clpercent > 20) { stop('Duplicate Test Fails: Greater than 20% of duplicates do not match.')}







# 6. Check Duplicate CV   -------------------------------------------------
# CV equals the sd between the two duplicates, divided by the mean and multiplied by 100 

## SULFATE 
cv.calc <- as.data.frame(dups.QA$SO4_mM)
cv.calc$dups <- dups.QA$SO4_mM_dup
# Calculate SD
cv.calc$sds <- apply(cv.calc,1,sd) 

dups.QA$SO4_dups_cv <- (cv.calc$sds)/((dups.QA$SO4_mM+dups.QA$SO4_mM_dup)/2) * 100 # Calculate CV 
dups.QA$SO4_dups_cv_flag <-  ifelse(dups.QA$SO4_dups_cv <cutoff, 'YES', 'NO, rerun')

## CHLORIDE 
cv.calc <- as.data.frame(dups.QA$Cl_mM)
cv.calc$dups <- dups.QA$Cl_mM_dup
# Calculate SD
cv.calc$sds <- apply(cv.calc,1,sd)

dups.QA$Cl_dups_cv <- (cv.calc$sds)/((dups.QA$Cl_mM+dups.QA$Cl_mM_dup)/2) * 100
dups.QA$Cl_dups_cv_flag <-  ifelse(dups.QA$Cl_dups_cv <cutoff, 'YES', 'NO, rerun')
head(dups.QA)


# Plots 
#plot dups output as a bar graph to easily check - want any over 10% to be red need to work on this 
dups.Sbar <- ggplot(data = dups.QA, aes(x = Sample_ID, y = SO4_dups_cv)) +
  geom_bar(stat = 'identity', aes(fill = ifelse(abs(SO4_dups_cv)<cutoff, 'No', 'Yes'))) + 
  scale_fill_manual(values = c('darkgreen','darkgray')) + 
  theme_bw() + labs(x= "Sample ID", y="CV Between SO4 Duplicates (%)") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +  
  theme(legend.position="none") +  geom_hline(yintercept=cutoff, linetype="dashed", color = "black", size=1)

dups.Clbar <- ggplot(data = dups.QA, aes(x = Sample_ID, y = Cl_dups_cv)) +
  geom_bar(stat = 'identity', aes(fill = ifelse(abs(Cl_dups_cv)<cutoff, 'No', 'Yes'))) + 
  scale_fill_manual(values = c('darkgreen','darkgray')) + 
  theme_bw() + labs(x= "Sample ID", y="CV Between Cl Duplicates (%)") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +  
  theme(legend.position="none") +  geom_hline(yintercept=cutoff, linetype="dashed",color = "black", size=1)

ggarrange(dups.Sbar, dups.Clbar,ncol=2, nrow=1)


# Check for percent of "no, reruns" to see if it  would warrant reruns 
dups.SO4bad.cv <- sum(dups.QA$SO4_dups_cv_flag=="NO, rerun", na.rm = T)
dups.SO4percent.cv <- (dups.SO4bad.cv/nrow(dups.QA))*100 # Calculate percent bad dups

dups.Clbad.cv <- sum(dups.QA$Cl_dups_cv_flag=="NO, rerun", na.rm = T)
dups.Clpercent.cv <- (dups.Clbad.cv/nrow(dups.QA))*100 # Calculate percent bad dups

# Runs with more than 20% bad duplicates should be rerun. 
if(dups.SO4percent.cv > 20) { stop('Duplicate Test Fails: Greater than 20% of duplicates do not match.')}
if(dups.Clpercent.cv > 20) { stop('Duplicate Test Fails: Greater than 20% of duplicates do not match.')}





# 7. Check Spike % Recovery --------------------------------------------------
# Extract Spikes  
sample.spk <- subset(sample.QA, Test=='SPK')
sample.spk <- subset(sample.spk, select=c('ID','SO4_mM','SO4_area'))
colnames(sample.spk) <- c('ID', "SO4_mM_spk","SO4_area_spk")
head(sample.spk)

# Merge by ID with unspiked samples so that the spikes are new columns
spk.QA <- merge(sample.OG, sample.spk, by="ID")
head(spk.QA)

#Calculate the spike concentration and calculate the spike recovery
spkconc <- (250/smw)       # in mM
spkvol  <- 10              # in uL
spkvol  <- spkvol/1000000  # in L

#spike for these samples was 10uL of the 250mM standard
spk.QA$SO4_spk_Conc <- (spkconc)*spkvol    # mmoles of SO4 
head(spk.QA)

#Specify dilution factors and initial amount of sample added
spk.QA$Dilution  <- Dilution
#Set Sample volumes in uL 
spk.QA$SampleVol <- 1500
#change sample volume to L 
spk.QA$SampleVol <- spk.QA$SampleVol/1000000
head(spk.QA)

#Calculate total SO4 in the sample in mmoles
spk.QA$SO4_Total_unspkd <- (spk.QA$SO4_mM/100)*(spk.QA$SampleVol) 

# Calculate total SO4 in spiked sample in mmoles
spk.QA$SO4_Total_spkd <- (spk.QA$SO4_mM_spk/100.66667)*(spk.QA$SampleVol+spkvol) 

#Calculate percent recovery 
spk.QA$SO4_expctd_spkd <-  (spk.QA$SO4_Total_unspkd + spk.QA$SO4_spk_Conc)
spk.QA$spk_recovery <-    (spk.QA$SO4_Total_spkd/spk.QA$SO4_expctd_spkd)*100
spk.QA$SO4_spks_flag <-  ifelse(spk.QA$spk_recovery <=120 & spk.QA$spk_recovery >=80 , 'Pass', 'Fail')  #fix 

head(spk.QA)


#plot spk recoveries output as a bar graph to easily check - want any over 10% to be red need to work on this 
spk.bar <- ggplot(data = spk.QA, aes(x = Sample_ID, y = spk_recovery, fill=SO4_spks_flag)) +
  geom_bar(stat = 'identity') + 
  theme_bw() + labs(x= "Sample ID", y="Spike Recovery (%)") + 
  geom_hline(yintercept=80, linetype="dashed", color = "black", linewidth=1) + 
  geom_hline(yintercept=120, linetype="dashed", color = "black", linewidth=1)+
  scale_fill_manual(values = c("Pass" = "darkgreen", "Fail" = "darkred"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position="none")
spk.bar


#Check for percent of "no, reruns" to see if it  would warrant reruns 
spk.bad <- sum(spk.QA$SO4_spks_flag=="Fail", na.rm = T)
spk.percent <- (spk.bad/nrow(dups.QA))*100 # Calculate percent bad dups

# Runs with more than 20% bad duplicates should be rerun. 
if(spk.percent > 20) { stop('Spike Test Fails: Greater than 20% of Spikes have poor recovery.')}






# 8. Export Data ------------------------------------------------------------
final <- sample.dat
final$Year <- Year

final <- subset(final, select = c(Year, Month, Plot, Depth, Test, SO4_ppm, SO4_mM, Cl_ppm, Cl_mM, salinity))


# setwd(paste0(wd,'/5-Sulfate & Chloride/',Year,'/Data', sep=''))
write.csv(final,file=paste(File,'_processed.csv',sep=''),row.names=F)




# 9. Add Standards to Log ----------------------------------------------------
#Read in log File
setwd(paste0(wd,"/5-Sulfate & Chloride/",Year,"/Data/Processing Log", sep=""))
std_log <- read.csv('Sulfate & Chloride Processing Log.csv')

#Generate std info for this run
stds_info <- as.data.frame(cbind(
  # When/who did this run
  Date_processed=date.processed,
  Personal=processed.by,
  # ID for this run
  Run=File,
  # Number of Duplicates
  N_DUP=nrow(dups.QA),
  # Percent SO4 Fail (percent difference or CV?)
  SO4_DUP_Fail=round(dups.SO4percent.cv,2),
  # Percent Cl Fail
  Cl_DUP_Fail=round(dups.Clpercent.cv,2), 
  # Number of Spikes
  N_SPK=nrow(spk.QA),
  # Percent Spike Fail
  SPK_Fail=round(spk.percent,2)))

std_all <- cbind(stds_info, rowSO4, rowCl)
std_all

#Merge and save File
std_new <- rbind(std_log,std_all)
print(std_new)
write.csv(std_new,file=paste0('Sulfate & Chloride Processing Log.csv'),row.names=F)


