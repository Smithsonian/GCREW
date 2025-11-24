##this script knits the rmd
# Set Pandoc path
Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files (x86)/pandoc-3.7.0.1")
setwd("C:/Users/LaGorgaL.S/Documents/Chapata_Stem/daily_checks")
library(rmarkdown)
render("C:/Users/LaGorgaL.S/Documents/Chapata_Stem/daily_checks/chapada_checks.Rmd")