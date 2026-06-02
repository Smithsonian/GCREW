proc import datafile="S:\Biogeochemistry\Marsh CO2xCommunity Study\Plant Biomass & CN Data\Biomass Data\2015\Census & Harvest Data\3-2015 CO2xComm SC Harvest (for transfer).xlsx"
out=Harvest
dbms=excel
replace;
sheet="Data";
data HarvestQAQC; set Harvest;
if Total_Height=-99 then Total_Height=.;
if Green_Height=-99 then Green_Height=.;
if Width=-99 then Width=.;
if Total_Height<Green_Height then HFlag=1;
if Total_Height>223 or 0<Total_Height<5 then HFlag=1;
if Width>8.4 or 0<Width<0.2 then WFlag=1;
PMass=-0.04027+0.00005033*(Total_Height)**2+0.01563*(Width)**2+0.0000008535*(Total_Height)**2*(Width)**2;
if Total_Height=0 or Width=0 then PMass=0;
if (PMass/Total_Mass)>2 or 0<(PMass/Total_Mass)<0.5 then MFlag=1;
proc export data=HarvestQAQC
outfile='S:\Biogeochemistry\Marsh CO2xCommunity Study\Plant Biomass & CN Data\Biomass Data\Census-Harvest Toolkit\Data transfer QAQC\HarvestQAQC' replace dbms=excel;
run;
