/****************************************************************************************/
/*************** 2020 QA/QC PROGRAMS FOR ERROR CHECKING ROOT INGROWTH DATA **************/
/****************************************************************************************/

/*This program was created in Apr 2020 by Pat Megonigal based on the 2016 version of the program which
includes a new category of rhizomes called "Other_Rhiz_Mass". All observations were set to -99 (missing
data) because no specific observations were made for this category. That will change in 2020 when root
sorters will be instructed to note whether any of the rhizomes are not either sedge or grass*/

proc import datafile="S:\Biogeochemistry\GCREW\1-CO2xCommunity Experiment\1-Plant Biomass & CN Data\
Root Biomass Data\2020\CO2xComm Root Ingrowth (filled).csv"
out=Root (drop=Notes)
dbms=dlm
replace;
delimiter=",";
/****************************************************************************************/
/********** STEP 1 -- RUN QA/QC PROGRAMS FOR ERROR CHECKING ROOT INGROWTH DATA **********/
/****************************************************************************************/
data RootQAQC; set Root;
/*Assign Missing Values*/
if Root_Mass=-99 then Root_Mass=.;
if C3_Rhiz_Mass=-99 then C3_Rhiz_Mass=.;
if C4_Rhiz_Mass=-99 then C4_Rhiz_Mass=.;
if Other_Rhiz_Mass=-99 then Other_Rhiz_Mass=.;

/*Assign Error Flags*/
if Root_Mass<4.38 then RFlag=0;
if Root_Mass>4.38 then RFlag=1;
if C3_Rhiz_Mass<7.37 then C3RFlag=0; else C3RFlag=1;
if C4_Rhiz_Mass<1.37 then C4RFlag=0; else C4RFlag=1;
if Other_Rhiz_Mass eq 0 or Other_Rhiz_Mass eq . then OTRFlag=0; else OTRFlag=1; /*This is a rare category so any entries should be double checked*/
if RFlag=0 and C3RFlag=0 and C4RFlag=0 and OTRFlag=0 then delete; /*Keep observations where any one of the flags is marked as 1*/

proc export data=RootQAQC
outfile='S:\Biogeochemistry\GCREW\1-CO2xCommunity Experiment\1-Plant Biomass & CN Data\Root Biomass Data\
2020\RootQAQC.csv' replace dbms=dlm; delimiter=",";
run;

/***************************************************************************************/
/************* STEP 2 -- GENERATE ROOT INGROWTH OUTPUT FILE FOR TRANSFER ***************/
/***************************************************************************************/

data RootTransfer; set Root;
Total_Rhiz_Mass=-99;
Root_Rhiz_Mass=-99;
 
proc export data=RootTransfer
outfile='S:\Biogeochemistry\GCREW\1-CO2xCommunity Experiment\1-Plant Biomass & CN Data\Root Biomass Data\
2020\CO2xComm Root Ingrowth (For Transfer).csv'
replace
dbms=dlm;
delimiter=",";
run;
