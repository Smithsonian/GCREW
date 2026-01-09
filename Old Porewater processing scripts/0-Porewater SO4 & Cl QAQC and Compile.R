#  Merging Porewater Ch4 Data and QAQC ------------------------------------

### R script to merge all previously processed csv files from 
### (CH4 porewater processing script.R) for the same year ###

# Created 11 Nov 2020 by SK #
# Updated 13 May 2023 by EF  to add QAQC #




# 1. Set Up ------------------------------------------------------------------
library(readxl)
library(dplyr)
library(plyr)
library(ggplot2)
library(ggpubr)
library(tidyr)

Project <- "SMARTX" 
Year <- 2023
date.processed <- '11.02.2023'
Personal <- 'ZR'

Dilution <- 100 # Sample Dilution factor. Determined with lab protocol. 
cutoff <- 10 # Cut off level for evaluating Duplicate %Diff and CV  

min.SO4 <- 0.05 #mM
max.SO4 <- 30.0 #mM
min.Cl  <- 0.05 #mM
max.Cl  <- 300  #mM




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





# 3. Read in Sulfate & Chloride Files --------------------------------------------------
setwd(paste0(wd,'/5-Sulfate & Chloride/',Year,'/Data', sep=''))

filenames <- list.files(pattern='_processed.csv')
filenames

# This function will add a source file column when importing data. 
read_csv_filename <- function(filename){
  ret <- read.csv(filename)
  ret$Source <- filename 
  ret
}

# Import all files in filenames list
sc.source <- ldply(filenames, read_csv_filename)

# Remove source file for processing. Source is helpful to know but cumbersome 
sample.dat <- subset(sc.source, select = -Source)
head(sample.dat)





# 4. Pair Duplicated Samples -------------------------------------------------
# Separate Sample ID's into Plot, Depth, Month and Test(DUP or Spike)

sample.dat$ID <- paste(sample.dat$Plot, sample.dat$Depth, sample.dat$Month, sep="_")
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
colnames(sample.dups) <- c('ID', 'Year', "Test","SO4_ppm_dup",'SO4_mM_dup', 'Cl_ppm_dup', 'Cl_mM_dup', 'Salinity_dup')
head(sample.dups)

# Merge by ID so that the Dup values are New columns
dups.QA <- merge(sample.OG, sample.dups, by="ID")
head(dups.QA)









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
dups.Sbar <- ggplot(data = dups.QA, aes(x = reorder(ID, -SO4_dups_cv), y = SO4_dups_cv)) +
  geom_bar(stat = 'identity', aes(fill = ifelse(abs(SO4_dups_cv)<cutoff, 'No', 'Yes'))) + 
  scale_fill_manual(values = c('darkgreen','darkgray')) + 
  theme_bw() + labs(x= "Sample ID", y="CV Between SO4 Duplicates (%)") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +  
  theme(legend.position="none") +  geom_hline(yintercept=cutoff, linetype="dashed", color = "black", size=1)

dups.Clbar <- ggplot(data = dups.QA, aes(x = reorder(ID, -Cl_dups_cv), y = Cl_dups_cv)) +
  geom_bar(stat = 'identity', aes(fill = ifelse(abs(Cl_dups_cv)<cutoff, 'No', 'Yes'))) + 
  scale_fill_manual(values = c('darkgreen','darkgray')) + 
  theme_bw() + labs(x= "Sample ID", y="CV Between Cl Duplicates (%)") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +  
  theme(legend.position="none") +  geom_hline(yintercept=cutoff, linetype="dashed",color = "black", size=1)

ggarrange(dups.Sbar, dups.Clbar,ncol=2, nrow=1)



# Export Duplicate Files ------------------------------------------------------
dup.final <- subset(dups.QA, select = c(Year.x, ID, SO4_mM, SO4_mM_dup,SO4_dups_cv, Cl_mM, Cl_mM_dup, Cl_dups_cv))

setwd(paste0(wd,'/5-Sulfate & Chloride/',Year,'/Data/Processing Log', sep=""))
write.csv(dup.final,file=paste0('Log of Duplicate CV ',Year,'.csv'),row.names=F)

jpeg(file=paste0('Duplicate CV ',Year,'.jpeg'), width = 1000, height = 600)
ggarrange(dups.Sbar, dups.Clbar,ncol=2, nrow=1)
dev.off()




# 7. Check Spike % Recovery --------------------------------------------------
# Extract Spikes  
sample.spk <- subset(sample.QA, Test=='SPK')
sample.spk <- subset(sample.spk, select=c('ID','SO4_mM'))
colnames(sample.spk) <- c('ID', "SO4_mM_spk")
head(sample.spk)

# Merge by ID with unspiked samples so that the spikes are new columns
spk.QA <- merge(sample.OG, sample.spk, by="ID")
head(spk.QA)

#molecular weight of sulfur: 32.06
smw  <- 32.06 
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
spk.QA$SO4_Total_spkd <- (spk.QA$SO4_mM_sp/100.66667)*(spk.QA$SampleVol+spkvol) 

#Calculate percent recovery 
spk.QA$SO4_expctd_spkd <-  (spk.QA$SO4_Total_unspkd + spk.QA$SO4_spk_Conc)
spk.QA$spk_recovery <-    (spk.QA$SO4_Total_spkd/spk.QA$SO4_expctd_spkd)*100
spk.QA$SO4_spks_flag <-  ifelse(spk.QA$spk_recovery <=120 & spk.QA$spk_recovery >=80 , 'Pass', 'Fail')  #fix 
head(spk.QA)


#plot spk recoveries output as a bar graph to easily check - want any over 10% to be red need to work on this 
spk.bar <- ggplot(data = spk.QA, aes(x = ID, y = spk_recovery, fill=SO4_spks_flag)) +
  geom_bar(stat = 'identity') + 
  theme_bw() + labs(x= "Sample ID", y="Spike Recovery (%)") + 
  geom_hline(yintercept=80, linetype="dashed", color = "black", linewidth=1) + 
  geom_hline(yintercept=120, linetype="dashed", color = "black", linewidth=1)+
  scale_fill_manual(values = c("Pass" = "darkgreen", "Fail" = "darkred"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  theme(legend.position="none")
spk.bar


# Export Spike Files ------------------------------------------------------
spk.final <- subset(spk.QA, select = -c(Test,SO4_ppm, Cl_ppm, Cl_mM, salinity))

setwd(paste0(wd,'/5-Sulfate & Chloride/',Year,'/Data/Processing Log', sep=""))
write.csv(spk.final,file=paste0('Log of Spike Recovery ',Year,'.csv'),row.names=F)

jpeg(file=paste0('Spike Recovery ',Year,'.jpeg'), width = 800, height = 600)
spk.bar
dev.off()
  





# 4. Check Sulfate & Chloride Values ------------------------------------------------------
# Merge with Plot Meta Data to put samples in order and check for missing values 
sc.dat <- subset(sample.dat, Test=="")
sc.dat$Month <- ifelse(sc.dat$Month=='MAY','May', 
                ifelse(sc.dat$Month=='Jul','July', 
                ifelse(sc.dat$Month=='SEPT','September',
                ifelse(sc.dat$Month=='Sept','September', sc.dat$Month))))

sc.QA <- merge(sc.dat,meta, by=c("Year","Month","Plot","Depth"), all.y = TRUE)

# Check for missing values
sc.QA$Missing.Value <- ifelse(is.na(sc.QA$SO4_mM)
                            |!complete.cases(sc.QA$SO4_mM)
                            |sc.QA$SO4_mM == "0"
                            |is.na(sc.QA$Cl_mM)
                            |!complete.cases(sc.QA$Cl_mM)
                            |sc.QA$Cl_mM == "0", 1, 0)

# Check for Sulfate & Chloride values outside the normal range 
sc.QA$Normal.Range <- ifelse(sc.QA$SO4_mM < min.SO4, 1,
                      ifelse(sc.QA$SO4_mM > max.SO4, 1,0))
sc.QA$Normal.Range <- ifelse(sc.QA$Cl_mM  < min.Cl, 1,
                      ifelse(sc.QA$Cl_mM  > max.Cl, 1, sc.QA$Normal.Range))

# Subset all rows with failed QAQC
sc.fail <- subset(sc.QA, Missing.Value== 1 | Normal.Range == 1 )

# Export QAQC 
setwd(paste0(wd,'/5-Sulfate & Chloride/',Year,'/Data', sep=''))
write.csv(sc.fail, paste0(Project," Porewater Sulfate & Chloride ", Year, " QAQC.csv"), row.names = FALSE)

if(nrow(sc.fail)!=0) { stop('SO4 or Cl values missing or outside normal range.')}






# 5. Generate Output ---------------------------------------------------------

# Left join with meta puts samples in the correct order
meta$Plot <- as.character(meta$Plot)
final <- left_join(meta, sc.dat, by=c("Year","Month","Plot","Depth"))

# Add Sample-ID
final$SampleID <- paste(final$Plot, final$Depth, sep = "-")

# Replace missing values (NA in both anolytes) with -99 
# NOT ALL NA = Missing Values, Dionex software generates NAs when a peak is not identified
# this may be due to a concentration below detection limit
final$SO4_mM <-ifelse(is.na(final$SO4_mM)&is.na(final$Cl_mM),"-99",final$SO4_mM)
final$Cl_mM <-ifelse(final$SO4_mM=="-99"&is.na(final$Cl_mM),"-99",final$Cl_mM)
final$salinity <-ifelse(final$SO4_mM=="-99"&final$Cl_mM=="-99","-99",final$salinity)


# Reorganize columns with project specific format
# SMARTX
if (Project=="SMARTX"){
  final <- subset(final, select = c(Year, Month, Plot, Temperature, CO2, Vegetation, Depth,SampleID, SO4_mM, Cl_mM, salinity))
}

# CO2xN 
if (Project=="CO2xN"){
  final <- final  %>% separate(Plot, into = c("Community", "Chamber"),sep = 1)
  final <- subset(final, select = c(Year, Month, Chamber, CO2, Nitrogen, Treatment, Depth,SampleID,SO4_mM, Cl_mM, salinity))
}

# CO2xCommunity
if (Project=="CO2xCommunity"){
  final <- final  %>% separate(Plot, into = c("Community", "Chamber"),sep = 2)
  final <- subset(final, select = c(Year, Month, Community, Chamber, Treatment, Depth,SampleID, SO4_mM, Cl_mM, salinity))
}

# Phrag
if (Project=="Phrag"){
  final <- final  %>% separate(Plot, into = c("Community", "Chamber", "Well"),sep = c(1, -1), remove = FALSE)
  final <- subset(final, select = c(Year, Month, Chamber, CO2, Nitrogen, Treatment,Well, Depth,SampleID, SO4_mM, Cl_mM, salinity))
}



## Export data as .csv file
setwd(paste0(wd,'/5-Sulfate & Chloride/',Year,'/Data', sep=''))
write.csv(final,file=paste('0-',Project,' Porewater Sulfate & Chloride ',Year,' (Compiled - For Transfer).csv',sep=''),row.names=F)


