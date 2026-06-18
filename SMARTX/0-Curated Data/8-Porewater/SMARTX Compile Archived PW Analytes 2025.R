# Compile All Archived Porewater Analyte Data By Experiment
# By: Erin Fien (05.04.2023)
# Edited by Zoe Read (01/26/2024)

##Clear Environment
rm(list=ls()) 

##Read in packages
library(dplyr)

# Set Working Directory --------------------------------------------------------
# General Porewater Archive Working Directory
wd <- "S:/Biogeochemistry/GCREW/4-SMARTX/0-Curated Data/8-Porewater"


# Import All Analyte Data ------------------------------------------------------
# Manually Edit File Names with Year and Publication Dates

# 1-Sulfide 
setwd(paste0(wd,"/1-Sulfide", sep=""))
sulfide <- read.csv("SMARTX Master Porewater Sulfide 2025 (published 02-13-2026).csv")
sulfide$Day_sulfide <- sulfide$Day
sulfide$Day <- NULL
sulfide <- sulfide %>%
  mutate(Month = factor(Month, levels = month.name)) 
sulfide <- sulfide[with(sulfide, order(Year, Month, Plot, Depth)),]
sulfide <- sulfide %>% 
  group_by(Year, Month, SampleID) %>% 
  dplyr::mutate(dupe = n()>1)
sulfide_dups <- subset(sulfide, dupe == "TRUE")

# 2-pH
setwd(paste0(wd,"/2-pH", sep=""))
pH <- read.csv("SMARTX Master Porewater pH 2025 (published 01-09-2026).csv")
pH$Day_pH <- pH$Day
pH$Day <- NULL
pH <- pH %>%
  mutate(Month = factor(Month, levels = month.name)) 
pH <- pH[with(pH, order(Year, Month, Plot, Depth)),]
pH <- pH %>% 
  group_by(Year, Month, SampleID) %>% 
  dplyr::mutate(dupe = n()>1)
pH_dups <- subset(pH, dupe == "TRUE")

# 3-CH4
setwd(paste0(wd,"/3-CH4", sep=""))
CH4 <- read.csv("SMARTX Master Porewater Methane 2025 (published 02-13-2026).csv")
CH4$Day_CH4 <- CH4$Day
CH4$Day <- NULL
CH4 <- CH4 %>%
  mutate(Month = factor(Month, levels = month.name)) 
CH4 <- CH4[with(CH4, order(Year, Month, Plot, Depth)),]
CH4 <- CH4 %>% 
  group_by(Year, Month, SampleID) %>% 
  dplyr::mutate(dupe = n()>1)
CH4_dups <- subset(CH4, dupe == "TRUE")

# 4-NH4
setwd(paste0(wd,"/4-NH4", sep=""))
NH4 <- read.csv("SMARTX Master Porewater NH4 2025 (published 02-24-2026).csv")
NH4$X <- NULL
NH4 <- NH4 %>%
  mutate(Month = factor(Month, levels = month.name)) 
NH4 <- NH4[with(NH4, order(Year, Month, Plot, Depth)),]
NH4 <- NH4 %>% 
  group_by(Year, Month, SampleID) %>% 
  dplyr::mutate(dupe = n()>1)
NH4_dups <- subset(NH4, dupe == "TRUE")

# 5-Sulfate & Chloride
setwd(paste0(wd,"/5-Sulfate & Chloride", sep=""))
SO4.Cl <- read.csv("SMARTX Master Porewater SO4 & Cl 2025 (published 02-13-2026).csv")
SO4.Cl$Day_SO4.Cl <- SO4.Cl$Day
SO4.Cl$Day <- NULL
SO4.Cl <- SO4.Cl %>%
  mutate(Month = factor(Month, levels = month.name)) 
SO4.Cl <- SO4.Cl[with(SO4.Cl, order(Year, Month, Plot, Depth)),]
SO4.Cl <- SO4.Cl %>% 
  group_by(Year, Month, SampleID) %>% 
  dplyr::mutate(dupe = n()>1)
SO4.Cl_dups <- subset(SO4.Cl, dupe == "TRUE")

##Change either SO4 or Cl from -99 to 0 if the other one is present. B/c Dionex gives value na instead of 0 when values is below detection limit, even though the sample was run. 
SO4.Cl$SO4 <- as.numeric(SO4.Cl$SO4)
SO4.Cl$Cl <- as.numeric(SO4.Cl$Cl)

SO4.Cl$SO4 <- ifelse(SO4.Cl$Cl > 0 & is.na(SO4.Cl$SO4) | SO4.Cl$Cl > 0 & SO4.Cl$SO4 == -99, 0, SO4.Cl$SO4)
SO4.Cl$Cl <- ifelse(SO4.Cl$SO4 > 0 & is.na(SO4.Cl$Cl) | SO4.Cl$SO4 > 0 & SO4.Cl$Cl == -99, 0, SO4.Cl$Cl)

##Change negative values to zero
SO4.Cl$SO4 <- ifelse(SO4.Cl$SO4 < 0 & SO4.Cl$SO4 > -99, 0, SO4.Cl$SO4)
SO4.Cl$Cl <- ifelse(SO4.Cl$Cl < 0 & SO4.Cl$Cl > -99, 0, SO4.Cl$Cl)


# Merge Analytes ---------------------------------------------------------------
# Add pH 
all <- full_join(sulfide, pH, by=c('Year','Month', 'Plot','Temperature','CO2','Vegetation','Depth','SampleID'))

# Add CH4 
all <- full_join(all, CH4, by=c('Year','Month','Plot','Temperature','CO2','Vegetation','Depth','SampleID'))

# Add NH4
all <- full_join(all, NH4, by=c('Year','Month','Plot','Temperature','CO2','Vegetation','Depth','SampleID'))

# Add SO4 and Cl
all <- full_join(all, SO4.Cl, by=c('Year','Month','Plot','Temperature','CO2','Vegetation','Depth','SampleID'))


##Keep only the lowest (sample) date
all$Sample_Date <- apply(all[,c("Day_sulfide", "Day_pH", "Day_CH4")], 1, min, na.rm = TRUE) 

##Replace infinity sample dates with NA 
all[sapply(all, is.infinite)] <- NA

##Make all -99 values NA 
all[all==-99] <- NA

##Replace all other negative values with 0 
all[all < 0] <- 0

##Remove other Dates 
all$Day_sulfide <- NULL
all$Day_pH <- NULL 
all$Day_CH4 <- NULL
all$dupe <- NULL
all$dupe.x <- NULL
all$dupe.x.x <- NULL
all$dupe.y <- NULL
all$dupe.y.y <- NULL

##Replace NA values with -99 
all [is.na(all)] <- -99

##Sort by year, month, chamber, and depth 
all <- all %>%
  mutate(Month = factor(Month, levels = month.name)) 

all <- all[with(all, order(Year, Month, Plot, Depth)),]

all

all <- subset(all, select = c(Year, Month, Sample_Date, SampleID, Plot, Depth, Temperature,  CO2, Vegetation,  
                              H2S, pH, CH4, NH4, SO4, Cl, Salinity))


# Export Data ------------------------------------------------------------------
setwd(paste0(wd,"/0-All Porewater Data", sep=""))
write.csv(all, "SMARTX Master Porewater All Data 2016-2025 (published 02-24-2026).csv", row.names = F)


# ##Calculate percentage of missing data in each column for each Month/Year 
# 
# ##Make all -99 values NA 
# all[all==-99] <- NA
# 
# 
# all
# ##Group by Month/Year and calculate %NA values
# NA_H2S <- all %>%
#   group_by(Year, Month) %>%
#   summarise(H2S_NA = 100 * sum(is.na(H2S)/length(H2S)))
# 
# NA_pH <- all %>%
#   group_by(Year, Month) %>%
#   summarise(pH_NA = 100 * sum(is.na(pH)/length(pH)))
# 
# NA_CH4 <- all %>%
#   group_by(Year, Month) %>%
#   summarise(CH4_NA = 100 * sum(is.na(CH4)/length(CH4)))
# 
# NA_NH4 <- all %>%
#   group_by(Year, Month) %>%
#   summarise(NH4_NA = 100 * sum(is.na(NH4)/length(NH4)))
# 
# NA_SO4 <- all %>%
#   group_by(Year, Month) %>%
#   summarise(SO4_NA = 100 * sum(is.na(SO4)/length(SO4)))
# 
# NA_Cl <- all %>%
#   group_by(Year, Month) %>%
#   summarise(Cl_NA = 100 * sum(is.na(Cl)/length(Cl)))
# 
# NA_Salinity <- all %>%
#   group_by(Year, Month) %>%
#   summarise(Salinity_NA = 100 * sum(is.na(Salinity)/length(Salinity)))
# 
# NA_all <- full_join(NA_H2S, NA_pH, by=c('Year','Month'))
# NA_all <- full_join(NA_all, NA_CH4, by=c('Year','Month'))
# NA_all <- full_join(NA_all, NA_NH4, by=c('Year','Month'))
# NA_all <- full_join(NA_all, NA_SO4, by=c('Year','Month'))
# NA_all <- full_join(NA_all, NA_Cl, by=c('Year','Month'))
# NA_all <- full_join(NA_all, NA_Salinity, by=c('Year','Month'))
# 
# NA_all <- NA_all %>% 
#   mutate_if(is.numeric, round)
# 
# NA_100 <- subset(NA_all, H2S_NA == 100 | pH_NA == 100 | CH4_NA == 100 | NH4_NA == 100 | SO4_NA == 100 | Cl_NA == 100 | Salinity_NA== 100)
# 
# keep <- apply(NA_100[3:9], 1, function(x) length(unique(x[!is.na(x)])) != 1)
# NA_100_select <- NA_100[keep, ]
# NA_100_select[, 3:9][NA_100_select[3:9]!=100] <- NA
# 
# 
# # Export Percent Missing Data
# setwd(paste0(wd,"/0-All Porewater Data", sep=""))
# write.csv(NA_all, "SMARTX Percent Missing Data 2016-2023.csv", row.names = F)
# write.csv(NA_100_select, "SMARTX Months and Years with All Data Missing 2016-2023.csv", row.names = F)
