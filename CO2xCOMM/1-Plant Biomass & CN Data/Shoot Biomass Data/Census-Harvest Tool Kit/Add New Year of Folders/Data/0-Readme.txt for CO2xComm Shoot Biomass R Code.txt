Readme.txt for CO2xComm Shoot Biomass Data
C.Nitsch 9.17.2024


The two RMD files that use R-studio to run are listed below:
1-CensusQAQC
3-HarvestQAQC


The 1-CensusQAQC file runs QA/QC programs for error checking and transferring census data, runs QA/QC programs for error checking density data, and generates the census and density output files for transfer. Upon opening the file, be sure to update the "year=" line of code on line 19 to set the year. This step is crucial to ensure csv files are being read and exported into the proper year folder and not overriding old files. 


The 3-HarvestQAQC file runs programs for error checking and file preparation of harvest data by running QA/QC programs for error checking CH4 Harvest data and generating C4 harvest output file for transfer. Upon opening the file, be sure to update the "year=" line of code on line 19 to set the year. This step is crucial to ensure csv files are being read and exported into the proper year folder and not overriding old files. 



Note: For 2023 data, this is the year that we switched from SAS data processing / QAQC to using R. Within the data files of this year, you will see output files from both SAS and R (distinguished by _R at the end of exported .csv files). 