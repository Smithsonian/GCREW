proc import datafile="S:\Biogeochemistry\Marsh CO2xCommunity Study\Plant Biomass & CN Data\Biomass Data\2015\Census & Harvest Data\2-2015 CO2xComm SC Density (for transfer).xlsx"
out=Density
dbms=excel
replace;
sheet="Data";
data DensityQAQC; set Density;
if Stem_Count=-99 then Stem_Count=.;
if Stem_Count>748 then SCFlag=1;
proc export data=DensityQAQC
outfile='S:\Biogeochemistry\Marsh CO2xCommunity Study\Plant Biomass & CN Data\Biomass Data\Census-Harvest Toolkit\Data transfer QAQC\DensityQAQC' replace dbms=excel;
run;
