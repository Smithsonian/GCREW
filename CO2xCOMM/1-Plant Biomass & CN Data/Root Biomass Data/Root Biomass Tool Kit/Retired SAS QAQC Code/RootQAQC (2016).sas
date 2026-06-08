proc import datafile="S:\Biogeochemistry\Marsh CO2xCommunity Study\Plant Biomass & CN Data\Root Data\CO2xComm Root Ingrowth (filled template).xlsx"
out=Root
dbms=excel
replace;
sheet="Data";
/****************************************************************************************/
/********** STEP 1 -- RUN QA/QC PROGRAMS FOR ERROR CHECKING ROOT INGROWTH DATA **********/
/****************************************************************************************/
data RootQAQC; set Root;
/*Assign Missing Values*/
if Root_Mass=-99 then Root_Mass=.;
if C3_Rhiz_Mass=-99 then C3_Rhiz_Mass=.;
if C4_Rhiz_Mass=-99 then C4_Rhiz_Mass=.;

/*Assign Error Flags*/
if Root_Mass<4.38 then RFlag=0;
if Root_Mass>4.38 then RFlag=1;
if C3_Rhiz_Mass<7.37 then C3RFlag=0;
if C3_Rhiz_Mass>7.37 then C3RFlag=1;
if C4_Rhiz_Mass<1.37 then C4RFlag=0;
if C4_Rhiz_Mass>1.37 then C4RFlag=1;
if RFlag=0 and C3RFlag=0 and C4RFlag=0 then delete; /*Keep observations where any one of the flags is marked as 1*/
proc export data=RootQAQC
outfile='S:\Biogeochemistry\Marsh CO2xCommunity Study\Plant Biomass & CN Data\Root Data\RootQAQC' replace dbms=excel;
run;

/***************************************************************************************/
/************* STEP 2 -- GENERATE ROOT INGROWTH OUTPUT FILE FOR TRANSFER ***************/
/***************************************************************************************/

data RootTransfer; set Root;
Total_Rhiz_Mass=-99;
Root_Rhiz_Mass=-99;
 
proc export data=RootTransfer
outfile='S:\Biogeochemistry\Marsh CO2xCommunity Study\Plant Biomass & CN Data\Root Data\RootIngrowth (for transfer)' replace dbms=excel;
run;
