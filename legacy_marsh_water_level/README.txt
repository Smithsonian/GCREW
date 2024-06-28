Selina Cheng

Code in here is pretty messy and not incorporated into a workflow. Sorry..
Code in here brings together several different water level data sources (from "DOE Warming Experiment" and "S:\Biogeochemistry\GCREW\0-GCREW-Wide Data\Meteorology\[YEAR]\Marsh Water Level\Raw Data")

data in the "data" folder comes from "S:\Biogeochemistry\GCREW\0-GCREW-Wide Data\Meteorology\[YEAR]\Marsh Water Level\Raw Data"

Roughly, this brings those data together and separates them by year and assigns column names so that they are in line with the current Loggernet system.


compare_water_level.R -- this is the script that created marsh_water_level_2016_2022.csv from all the data that I compiled in the "data" folder
waterlevel_old_to_new.R -- takes old C3 and C4 2017 and 2018 data from Dropbox/DOE Warming Experiment and changes it to make it more similar to contemporary loggernet data
separate_water_level.R -- separates out marsh_water_level_2016_2022.csv into individual years of data and saves it in Dropbox/GCREW_RESEARCHER_DATA/gcrew_waterlevel/yearly as the c3_floatgauge.