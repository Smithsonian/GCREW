A YouTube tutorial of how to process GCREW Loggernet data is here: https://youtu.be/ic1lrDtWzUg
A YouTube tutorial of how to process GCREW water level data is here: https://youtu.be/C_Yw1Bbu4oA
A YouTube tutorial explaining my GCREW Marsh Outlet sonde processing: https://youtu.be/HvkK3svQfiw

scripts folder:
-- scripts here can be used to process any kind of Loggernet data, just need a "design document", which are saved in "design documents" folder.
You should only need to open the script 0-GCREW-workflow-execute.R, which draws on the functions created in 0-GCREW-workflow-functions.R

design documents folder:
-- here is where we store the design documents that map loggernet variables to the experimental design. They are very important!!

Most of the Loggernet data we process is from GCREW at Dropbox\GCREW_LOGGERNET_DATA

The data that is generated at each intermediate step is saved in Dropbox\GCREW_LOGGERNET_WORKFLOW. 
The final data generated is saved in Dropbox\GCREW_RESEARCHER_DATA

gcrew-data-workflow.html is an example of how the workflow functions.

missing_export_var_SMARTX_design and missing_vars_SMARTX_design is a list of variables that appear in the SMARTX export tables but don't appear in the design document so that might need to be fixed? idk


To process water level data:
The water level data is adjusted using the Aquatroll offsets measured in Loggernet processing/design documents/AquaTROLL offsets.xlsx
- First process water level data using the normal GCREW workflow up to the "yearly" folder in Dropbox / GCREW_RESEARCHER_DATA/gcrew_waterlevel/yearly
- Run 1_combine_waterlevel.R. This script combines the water level tables from different sites into one dataset (one in wide formm, one in long form) and saves it in GCREW_RESEARCHER_DATA/gcrew_waterlevel/combined
- Run 2_fill_waterlevel.R. This script fills in water level data that is missing at different sites using the Aquatroll offsets. The script creates long and wide versions of that data, saved in GCREW_RESEARCHER_DATA/gcrew_waterlevel/combined


-Selina Cheng