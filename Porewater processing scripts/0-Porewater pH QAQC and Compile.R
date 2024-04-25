# POREWATER pH QAQC 


# 1. Set Up ------------------------------------------------------------------
library(readxl)
library(dplyr)
library(tidyr)


## Change these parameters before running the script##
Year <- 2023
Project <- "SMARTX"

max.ph <- 8.0 # Maximum value of normal pH range
min.ph <- 6.2 # Minimum value of normal pH range


# Set File Path ------------------------------------------------------------------
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
setwd(paste0(wd,'/2-pH/',Year, sep=''))
meta <- read.csv(paste0(wd,"/Annual Porewater Sample Lists/", Project, " Annual Porewater Sample List.csv"))
meta$Year <- Year





# 2. Read in Monthly pH Files --------------------------------------------------
setwd(paste0(wd,'/2-pH/',Year,'/Data', sep=''))
filenames <- list.files(pattern='.xlsx', recursive = TRUE) # Error will occur if files are open
filenames

# Set up empty dataframe
ph.all <- data.frame(matrix(ncol=7))
names(ph.all) <- c('Year','Month','Day','Plot','Depth','pH','Slope')

# Read in data for each month and combine them 
for (i in 1:length(filenames)) {
  data=read_excel(filenames[i])[,1:7]
  ph.all=rbind(ph.all,data)
}

ph.all <- ph.all[-1,] # remove extra row in dataframe
rm(data)






# 3. Check Slopes ---------------------------------------------------------
slope.QA <- ph.all 
if(all(is.na(slope.QA$Slope))) { stop('Slope values missing.')}

slope.QA$slope.QA <- ifelse(slope.QA$Slope<90, 1, 0)

# Subset all rows with failed QAQC
slope.fail <- subset(slope.QA, slope.QA== 1)

slope.fail
if(nrow(slope.fail)!=0) { stop('Slope values missing or outside normal range.')}






# 4. Check pH Values ------------------------------------------------------
# Merge with Plot Meta Data to put samples in order and check for missing values 

ph.QA <- merge(ph.all,meta, by=c("Year","Month","Plot","Depth"), all.y = TRUE)
ph.QA <- subset(ph.QA, select=-Slope)

# Check for missing values
ph.QA$Missing.Value <- ifelse(is.na(ph.QA$pH) | !complete.cases(ph.QA$pH) | ph.QA$pH == "0", 1, 0)

# Check for pH values outside the normal range 
ph.QA$Normal.Range <- ifelse(ph.QA$pH < min.ph, 1,
                      ifelse(ph.QA$pH > max.ph, 1, 0))

# Subset all rows with failed QAQC
pH.fail <- subset(ph.QA, Missing.Value== 1 | Normal.Range == 1 )
pH.fail <- subset(pH.fail, select = c(Year, Month, Day, Plot, Depth, pH, Missing.Value, Normal.Range)) # select useful columns 
pH.fail

# Export QAQC 
setwd(paste0(wd,'/2-pH/',Year,'/Data', sep=''))
write.csv(pH.fail, paste0(Project," Porewater pH ", Year, " QAQC.csv"), row.names = FALSE)

if(nrow(pH.fail)!=0) { stop('pH values missing or outside normal range.')}






# 5. Generate Output ---------------------------------------------------------
# Left join with meta puts rows in the correct order and keeps all the rows of meta 
final <- left_join(meta, ph.all, by=c("Year","Month","Plot","Depth"))

# Add Sample-ID
final$SampleID <- paste(final$Plot, final$Depth, sep = "-")

# Replace missing values with -99
final$pH <-ifelse(is.na(final$pH),"-99",final$pH)


# Reorganize columns with project specific format
# SMARTX
if (Project=="SMARTX"){
  final <- subset(final, select = c(Year, Month, Day, Plot, Temperature, CO2, Vegetation, Depth,SampleID, pH))
}

# CO2xN 
if (Project=="CO2xN"){
  final <- final  %>% separate(Plot, into = c("Community", "Chamber"),sep = 1)
  final <- subset(final, select = c(Year, Month, Day, Chamber, CO2, Nitrogen, Treatment, Depth,SampleID, pH))
}

# CO2xCommunity
if (Project=="CO2xCommunity"){
  final <- final  %>% separate(Plot, into = c("Community", "Chamber"),sep = 2)
  final <- subset(final, select = c(Year, Month, Day, Community, Chamber, Treatment, Depth,SampleID, pH))
}

# Phrag
if (Project=="Phrag"){
  final <- final  %>% separate(Plot, into = c("Community", "Chamber", "Well"),sep = c(1, -1), remove = FALSE)
  final <- subset(final, select = c(Year, Month, Day, Chamber, CO2, Nitrogen, Treatment,Well, Depth,SampleID, pH))
}


## Export data as .csv file
write.csv(final,file=paste('0-',Project,' Porewater pH ',Year,' (Compiled - For Transfer).csv',sep=''),row.names=F)

