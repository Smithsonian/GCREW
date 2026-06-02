proc import datafile="S:\Biogeochemistry\Marsh CO2xCommunity Study\Plant Biomass & CN Data\Biomass Data\2015\Census & Harvest Data\1-2015 CO2xComm SC Census (for transfer).xlsx"
out=Census
dbms=excel
replace;
sheet="Data";
data CensusQAQC; set Census;
if Rep=99 then delete;
Miner=-99;
Rust=-99;
Flower=-99;
if Total_Height=-99 then Total_Height=.;
if Green_Height=-99 then Green_Height=.;
if Width=-99 then Width=.;
if Total_Height<Green_Height then HFlag=1;
if Total_Height>223 or 0<Total_Height<5 then HFlag=1;
if Width>8.4 or 0<Width<0.2 then WFlag=1;
proc export data=CensusQAQC
outfile='S:\Biogeochemistry\Marsh CO2xCommunity Study\Plant Biomass & CN Data\Biomass Data\Census-Harvest Toolkit\Data transfer QAQC\CensusQAQC' replace dbms=excel;
run;

data QuadDensity (rename=(Total_Height=Quad_Density) drop=Green_Height Width Cut_Stem); set Census;
if Rep ne 99 then delete;
if Quad_Density>166 then QDFlag=1;

proc export data=QuadDensity
outfile='S:\Biogeochemistry\Marsh CO2xCommunity Study\Plant Biomass & CN Data\
Biomass Data\Census-Harvest Toolkit\Data transfer QAQC\DensityQAQC' replace dbms=excel;

data ChamberDensity (rename=(Quad_Density=Stem_Count); set QuadDensity;
proc means data=QuadDensity; var Stem_Count; by year community chamber treatment;
output out=ChamberDensity (drop = _type_ _freq_)  sum=Stem_Count 
run;

proc export data=ChamberDensity
outfile='S:\Biogeochemistry\Marsh CO2xCommunity Study\Plant Biomass & CN Data\
Biomass Data\Census-Harvest Toolkit\Data transfer QAQC\ChamberDensity' replace dbms=excel;
run;
/*
Chamber_Area=0.47;
proc print;
run;


proc sort; by year community treatment quadrat;



