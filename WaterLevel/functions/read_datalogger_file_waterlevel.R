## Function read_datalogger_file originally sourced from COMPASS code produced by Ben Bond-Lamberty https://github.com/COMPASS-DOE/sensor-data-pipeline/blob/main/pipeline/L0-utils.R

# filename <- path to the raw loggernet data file 
library(tidyverse)

read_datalogger_file_waterlevel <- function(filename, quiet = FALSE, ...) {
  
  # Parse line one to extract logger and table names
  dat <- read_lines(filename)
  header_split <- strsplit(dat[1], ",")[[1]]
  header_split <- gsub("\"", "", header_split) # remove quotation marks
  format_name <- header_split[1] # first field of row 1
  program_name <- header_split[6]
  tableheader_name <- header_split[length(header_split)]
  
  
  # We have no time zone information, so read the timestamp as character
  if(length(list(...))) {
    x <- read_csv(I(dat[-c(1, 3, 4)]), ...)
  } else {
    x <- read_csv(I(dat[-c(1, 3, 4)]),
                  # don't want timestamp parsed to a datetime at this point
                  col_types = list(TIMESTAMP = col_character()))
  }
  info <- tibble(Table = rep(tolower(tableheader_name), nrow(x)),
                 Format = rep(format_name, nrow(x)),
                 Program = rep(tolower(program_name), nrow(x)))
  as_tibble(cbind(info, x))
  
}