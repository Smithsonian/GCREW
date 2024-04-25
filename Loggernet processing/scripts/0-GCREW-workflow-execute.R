## GCREW EXPERIMENTS
## Selina Cheng - originated July 26, 2022. Last modified October 26, 2022

# This script runs all the functions that were created in "0-GCREW-workflow-functions.R"
# To find the places in this script that the user should edit, "Find" or "Ctrl + F" the phrase "user input".
# These "User input" sections are the only areas where the user should need to edit variables.

# You can use this script to process several of the main Loggernet tables, listed below:
# - GENX ARDLOG and Export
# - GCREW MET
# - SMARTX C3LOG and C4LOG Export 
# - SMARTX CO2LOG
# - SMARTX Redox
# - All waterlevel tables

## ======================== Load functions =====================================================
# Source functions and packages from "0-SMARTX-origami-functions.R"
source("workflow scripts/0-GCREW-workflow-functions.R")

## =========================== USER INPUT SECTION ==================================================
# The user should edit the following variables as seen fit

# Set source directory for initial draw of raw data
source_dir <- file.path("C:", "Users", Sys.getenv("USERNAME"), "Dropbox (Smithsonian)", "GCREW_LOGGERNET_DATA")

# File path at which you want to save all intermediary output for this script
mid_path <- file.path("C:", "Users", Sys.getenv("USERNAME"), "Dropbox (Smithsonian)", "GCREW_LOGGERNET_WORKFLOW")

# File path where usable data is made available for researchers (monthly/yearly)
out_path <- file.path("C:", "Users", Sys.getenv("USERNAME"), "Dropbox (Smithsonian)", "GCREW_RESEARCHER_DATA")

# Logger x project document for file organization
logger_role <- file.path("design documents", "logger_x_project.csv")

# ------------ Load design documents. Comment out and comment in the correct + corresponding documents -----------
# SMARTX design
design_table <- file.path("design documents", "SMARTX_design.csv")
plot_names <- file.path("design documents", "SMARTX_plotnames.csv")
# 
# # # GENX design
# design_table <- file.path("design documents", "GENX_design.csv")
# plot_names <- file.path("design documents", "GENX_plotnames.csv")
# 
# # GENX organs design  
# design_table <- file.path("design documents", "organs_design.csv")
# plot_names <- file.path("design documents", "organs_plotnames.csv")
# 
# # Water level design
# design_table <- file.path("design documents", "waterlevel_design.csv")
# plot_names <- file.path("design documents", "site_plotnames.csv")

# # # GCREW Met design
# design_table <- file.path("design documents", "gcrewmet_design.csv")
# plot_names <- file.path("design documents", "site_plotnames.csv")

# # GCREW Marsh Outlet Sontektable design
# design_table <- file.path("design documents", "sontektable_design.csv")
# plot_names <- file.path("design documents", "site_plotnames.csv")

# # Redox design
# design_table <- file.path("design documents", "redox_design.csv")
# plot_names <- file.path("design documents", "redox_plotnames.csv")

## =========================== Initial steps, which can apply to any tables in loggernet =========================================
# Step 1: Create directory file paths and create the directories
my_dirs <- dir_names(mid_path)
create_dirs(my_dirs)

#  -------------- Step 2: Copy files from source directory to raw directory ----------------------
# When running this a second time, file_import should just overwrite files of the same name
# Takes 13 min to import 530 files
pattern <- "C4LOG_Export_2024"
start <- Sys.time()
file_import(source_dir, my_dirs$raw, pattern)
end <- Sys.time()
end-start

# ---------------- Step 3: Run process function to convert files to .csv and rename them ------------
# 45 min to process 530 files
start_time <- Sys.time()
process(my_dirs$raw, my_dirs$processed)
end_time <- Sys.time()
end_time - start_time

# ---------------- Step 4: Run sorter function, which sorts all files into appropriate subfolders ---------------
sorter(my_dirs$processed, my_dirs$sorted)

# ======================= Below steps (normalization) are only for tables listed here ===================
# - GENX ARDLOG and Export
# - GENX ORGANF and ORGANB Export
# - GCREW MET
# - SMARTX C3LOG and C4LOG Export 
# - SMARTX CO2LOG
# - SMARTX Redox
# - All waterlevel tables

# ---------------------------- Step 5: Normalize data --------------------------------------------------
# USER INPUT SECTION
# Enter the logger and table that you want to normalize.
# Also enter the increment for how often the data has been collected for that table
# You should only normalize loggers/tables together when they have the same increment. 
# You can enter several loggers or tables as a character vector.
# For example, logger <- c("c3log", "c4log") and table <- c("export", "check") will run all combinations of logger and table that exist in files
# BUT you can only run tables at the same time if they have the same increment

# Export table increment = 15 min
# Control tables are only run sometimes if ever
# Increment for control is 1 min
# GENX ARDLOG is 1 min

logger <- c("c4log")
table <- c("export")

# Increment in minutes
increment <- 15

# Step 5a: Obtain directories that will be normalized
dirs_for_norm <- norm_files(my_dirs$sorted, logger, table)

# Step 5b: Normalize the files
# 11 min to normalize a year's worth of c3log and c4log export tables
start_time <- Sys.time()
norm_gcrew(dirs_for_norm, my_dirs$normal, design_table, plot_names, increment)
end_time <- Sys.time()
end_time - start_time

# ---------------------- Step 6: Parse data files and save individual data files by month ---------------------
# USER INPUT SECTION 
# Enter the table that you want to aggregate by month
# You can enter several tables as vectors.
# I added this user input section to make it more manual, but honestly it could just be automated...
# ...to aggregate all of the files in the normal_working directory.
# For example, table <- c("export", "check")
# table <- c("co2log")
table <- c("export")

# Step 6a: Obtain directories that will be aggregated by months
files_for_month <- monthly_files(my_dirs$normal, table)

# Step 6b: Aggregate the data by month and save to researcher data folder
start_time <- Sys.time()
monthlymanage(files_for_month, my_dirs$monthly, out_path, logger_role)
end_time <- Sys.time()
end_time - start_time

# ---------------------- Step 7, bundle: Aggregates monthly data tables together into yearly data by logger/table ----------------
# One logger at a time
logger <- "c4log"
table <- "export"

start_time <- Sys.time()
bundle(my_dirs$monthly, out_path, logger_role, logger, table)
end_time <- Sys.time()
end_time - start_time

# =============================== Run these functions as needed =====================================================
# HEADERCHECK:
# Extracts all variable names from LoggerNet file and saves it in loggernetcol.txt
# USER INPUT SECTION
# Enter the logger and table that you want to extract headers from
# You can enter several loggers or tables as a character vector.
# For example, logger <- c("c3log", "c4log") and table <- c("export", "check")
logger <- c("C3LOG", "c4log")
table <- c("check", "export")
# Enter the directory that you want to take files from
header_dir <- paste0(mid_path, "/1_processed/processed_archive")

headercheck(header_dir, mid_path, logger, table)

# Dup removal
# USER INPUT SECTION
# Set source_dir to whatever you want to read things in from to remove duplicates
source_dir <- out_path
dup_removal(source_dir)

