
cdph_pkg_list <- c("data.table"
                   #, "plotly"
                   , "DT"
                   , "dplyr"
                   , "lubridate"
                   , "ggplot2"
                   , "xlsx"
                   , "XLConnect"
                   #, "XLConnectJars" for 32-bit support
                   , "foreign"
                   , "openxlsx"
                   , "readxl"
                   , "Rcpp"
                   , "writexl"
                   , "WriteXLS"
                   , "scales"
                   , "Rcapture"
                   , "RColorBrewer"
                   , "epitools"
                   , "stringr"
                   , "multiplex"
                   , "utils"
                   , "tibble"
                   , "tidyr"
                   , "tidycensus"
                   , "tigris"
                   , "tidyverse"
                   , "tidyxl"
                   , "sas7bdat"
                   , "haven"
                   , "tibble"
                   #, "devtools"
                   #, "utils"
)
cdph_pkg_list_new <- cdph_pkg_list[!(cdph_pkg_list %in% installed.packages()[,"Package"])]
if(length(cdph_pkg_list_new)>0) {
  install.packages(cdph_pkg_list_new, dependencies = TRUE)
}
lapply(cdph_pkg_list, require, character.only = TRUE)

#Standard global environment functions
ifrm <- function(obj, env = globalenv()) {
  obj <- deparse(substitute(obj))
  if(exists(obj, envir = env)) {
    rm(list = obj, envir = env)
  }
}

#date or string of dates convert to week of year
getWeek <- function(date_input){
  date_output <- strftime(c(date_input), format = "%V")
  return(date_output)
}
