proc import datafile="S:\Biogeochemistry\Marsh CO2xCommunity Study\Plant Biomass & CN Data\Root Data\CO2xComm Root Ingrowth (for transfer).xlsx"
out=Root
dbms=excel
replace;
sheet="Data";
data RootQAQC; set Root;
if Root_Mass=-99 then Root_Mass=.;
if C3_Rhiz_Mass=-99 then C3_Rhiz_Mass=.;
if C4_Rhiz_Mass=-99 then C4_Rhiz_Mass=.;
if Root_Mass>4.38 then RFlag=1;
if C3_Rhiz_Mass>7.37 then C3RFlag=1;
if C4_Rhiz_Mass>1.37 then C4RFlag=1;
proc export data=RootQAQC
outfile='S:\Biogeochemistry\Marsh CO2xCommunity Study\Plant Biomass & CN Data\Biomass Data\Census-Harvest Toolkit\Data transfer QAQC\RootQAQC' replace dbms=excel;
run;
