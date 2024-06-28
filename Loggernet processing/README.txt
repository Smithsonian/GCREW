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