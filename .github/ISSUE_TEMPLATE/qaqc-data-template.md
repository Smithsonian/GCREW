---
name: QAQC Data Template
about: Step-by-step guide through QAQC of data.
title: 'ANALYSIS: Experiment: Month YYYY QAQC '
labels: documentation
assignees: gnoyce, wilsonsj100

---

**Please follow this checklist when QAQCing data**

<ins> Start Up </ins>
- [ ] Pull the code down to GitHub desktop and create a new branch 
- [ ] Copy the template and change the name to include the YYYYMM of data being analyzed 
- [ ] Save this file to the YYYY folder 
- [ ] Save any new data files in the Raw Data folder (check format) 
- [ ] Open new RMD file and edit the author line at the top to reflect Month YYYY being analyzed 

<ins> Run Information </ins>
- [ ] Change the run date (found in raw data file) 
- [ ] Change the sample year (YYYY)
- [ ] Enter the sample month being analyzed 
- [ ] Input your name as the user 
- [ ] Enter the raw datafile names, double check they match the month 
- [ ] Check QAQC and Sample Logs are for the correct year 
- [ ] Change the final path to match the YYYYMM at the end 
- [ ] If there are notes at this stage, add them, if not leave blank 

<ins> Set Up </ins>
- [ ] Check that your standard concentrations are correct 
- [ ] Ensure check standard concentrations match the run 
- [ ] Ensure spike concentrations match what was run 
- [ ] Review flag and cutoff values (should be no need to change, but review) 
- [ ] Ensure and third party check values match expected values for that year 

**Start running code:** 

<ins> Metadata </ins>
- [ ] After metadata is read in, check the dataframe to see its formatted correctly

<ins> Read in Data </ins>
- [ ] After data is read in, check dataframe to check formatting 
- [ ] Double check columns appear correct 

<ins> Standard Curves </ins>
- [ ] Check that standard curves graphs look reasonable 
- [ ] Do the curves pass QAQC testing? If not, add info to the Run Notes 
- [ ] Compare slopes to those previously run, look for drift, report in Run Notes if needed 
- [ ] Check that the log file has been appended to 

<ins> Checks & Check Standards </ins>
- [ ] If there are third party checks, look for passing QAQC, if not add to Run Notes 
- [ ] If there is a reduction efficiency test, look for passing QAQC, if not add to Run Notes 
- [ ] Note if check standards pass RSV testing, report in Run Notes if not
- [ ] Look at check standards graph, look for drift and assess concentrations 
- [ ] Check if the check standards pass, if not add to Run Notes 
- [ ] If a flag should be generated, check the sample dataframe to see it was added 

<ins> Blanks </ins>
- [ ] Look at blanks graph, look to see if they are below the lower quartile of samples
- [ ] Check if the blanks pass the QAQC tests, if not add to Run Notes 
- [ ] If a flag should be generated, check the sample dataframe to see it was added 

<ins> Duplicates </ins>
- [ ] Look at the duplicates plot, assess quality of duplicates 
- [ ] Check to see if duplicates pass QAQC tests, if not add to Run Notes 
- [ ] If a flag should be generated, check the sample dataframe to see it was added 

<ins> Spikes (if applicable) </ins>
- [ ] Look at the spikes plot, assess quality make any needed notes 
- [ ] Check to see if spikes pass QAQC tests, if not add to Run Notes 
- [ ] If a flag should be generated, check the sample dataframe to see it was added 

<ins> Matrix Effects (if applicable) </ins>
- [ ] Check to see if matrix effect tests pass QAQC 
- [ ] If a flag should be generated, check the sample dataframe to see it was added 

<ins> Sample Flagging </ins>
- [ ] After running sample flagging, check to see if flags were generated in the dataframe 

<ins> Checking for Samples in Metadata </ins>
- [ ] Were all samples present in metadata? If not, add to Run Notes and to the end of this issue to be checked against field datasheet or look for a potential input issue 

<ins> Visualizing the Data </ins>
- [ ] Look at the sample data and consider if it makes sense, are there any samples that seem much higher or lower? If you suspect an issue, add it to this issue and the Run Notes to be looked at by a reviewer 

<ins> Write Out Data </ins>
- [ ] Check the final file path again (beware of writing over other files) 
- [ ] Check the final dataframe to check it looks correct 
- [ ] Check that the data file was created in the Processed Data folder 

<ins> Close Out </ins>
- [ ] Add any last minute run notes 
- [ ] Save the RMD file you are working in 
- [ ] Knit the file 
- [ ] Review the pdf that has been created, if you find any errors or needed notes add then save code and knit again
- [ ] Close the pdf, move it to the Run Summaries to be Reviewed folder 
- [ ] Close the RMD file 
- [ ] Push these changes up to Github and create a PR 
- [ ] Tag this issue in the PR as  "see issue #NN"
- [ ] Add a relevant reviewer to the PR 
- [ ] submit this issue and then submit the PR
