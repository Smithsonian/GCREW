proc import datafile="S:\Biogeochemistry\Marsh CO2xCommunity Study\Plant Biomass & CN Data\Biomass Data\2015\Census & Harvest Data\4-CO2xComm C4 Harvest (for transfer).xlsx"
out=Harvest
dbms=excel
replace;
sheet="Data";
data HarvestQAQC; set Harvest;
if SP_Count=-99 then SP_Count=.;
if DI_Count=-99 then DI_Count=.;
if SP_Total_Mass=-99 then SP_Total_Mass=.;
if DI_Total_Mass=-99 then DI_Total_Mass=.;
if SP_Count>58 then SPCFlag=1;
if DI_Count>31 then DICFlag=1;
if SP_Total_Mass>8.9 then SPMFlag=1;
if DI_Total_Mass>5.1 then DIMFlag=1;
if Other_1_Count>35 or Other_2_Count>35 then OCFlag=1;
if Other_1_Mass>11.9 or Other_2_Mass>11.9 then OMFlag=1;
proc export data=HarvestQAQC
outfile='S:\Biogeochemistry\Marsh CO2xCommunity Study\Plant Biomass & CN Data\Biomass Data\Census-Harvest Toolkit\Data transfer QAQC\C4HarvestQAQC' replace dbms=excel;
run;
