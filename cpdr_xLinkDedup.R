#####Load Packages #####
cdph_pkg_path <- file.path("H:", "CPDR","programs", "cdph_install_pkg.R")
source(cdph_pkg_path)

cdph_pkg_list <- c("RODBC", "RSQLite", "odbc")
cdph_pkg_list_new <- cdph_pkg_list[!(cdph_pkg_list %in% installed.packages()[,"Package"])]
if(length(cdph_pkg_list_new)>0) {
  install.packages(cdph_pkg_list_new, dependencies = TRUE)
}
lapply(cdph_pkg_list, require, character.only = TRUE)

#####Downloading data from SQL server#####
# require(RODBC)
# require(RSQLite)
# require(odbc)
odbc::odbcListDrivers()
#CPDR_Main, CPDR_Main_RO, CPDR_XML_Import, ParkinsonsMain
#Data Download
dbconnection <- odbcDriverConnect(paste("Driver=ODBC Driver 17 for SQL Server"
                                        #, "Server=10.226.229.100"
                                        , "Server=PHDCDTMSSQLIT01"
                                        , "Database=CPDR_Main_RS" #orCPDR_Main_RS
                                        #, "Uid=ParkinsonsDMSReportsUA"
                                        #, "Pwd=SqlPassword"
                                        , "trusted_connection=yes" #For Windows authentication method not SQL Server Authentication
                                        , sep = "; ")
)

# dbconnection <- odbcDriverConnect("Driver={SQLite}; Server=10.226.229.100; Database=CPDR_Main ;Uid=pcip; Pwd=n3wP@55w0rd; trusted_connection=yes")
#cpdr_db_names <- sqlQuery(dbconnection, paste("select * from master.sys.databases;"))
#cpdr_db_names

cpdr_df <- sqlQuery(dbconnection, paste("select * from [CPDR_Main_RS].[dbo].[vw_incident];"))

odbcClose(dbconnection)

#####Data Cleanup for CPDR#####
cpdr_df_tmp <- cpdr_df[(cpdr_df$AddressState %in% c("CA", "CALIFORNIA"))|cpdr_df$AddressState %in% zipcode[zipcode$state %in% "CA",]$zip,]


#####Variable creation for CPDR#####
#III. Additional modifications to data for cleaning and deduplication
#a. Changing date of Diagnosis to a readable date format
#Formatting change? code below no longer necessary
#cpdr_df_tmp[which(nchar(as.character(cpdr_df_tmp$DateOfDiagnosis))==6),]$DateOfDiagnosis <- paste(cpdr_df_tmp[which(nchar(as.character(cpdr_df_tmp$DateOfDiagnosis))==6),]$DateOfDiagnosis, "01", sep = "")

#Formatting Date columns
cpdr_df_tmp$DateOfDiagnosis <- as.Date(as.character(cpdr_df_tmp$DateOfDiagnosis), format = "%Y%m%d")
cpdr_df_tmp$ageDx <- as.numeric(year(cpdr_df_tmp$DateOfDiagnosis) - as.numeric(year(cpdr_df_tmp$DateOfBirth)))
cpdr_df_tmp$DateOfDiagnosis <- as.POSIXct(cpdr_df_tmp$DateOfDiagnosis)

#Removing any dates that exist outside of the current time frame
cpdr_df_tmp[which(year(cpdr_df_tmp$DateOfDiagnosis) > year(Sys.Date())|year(cpdr_df_tmp$DateOfDiagnosis) < 1920),]$DateOfDiagnosis <- NA
# initdata_tmp$DateOfDiagnosis <-as.POSIXlt(initdata_tmp$DateOfDiagnosis)
#b. Changing Date of Birth to a readable date format
cpdr_df_tmp[which(nchar(as.character(cpdr_df_tmp$DateOfBirth))!=8),]$DateOfBirth <- format(as.Date(as.character(cpdr_df_tmp[which(nchar(as.character(cpdr_df_tmp$DateOfBirth))!=8),]$DateOfBirth), format = '%y%m%d'), "19%y%m%d")
cpdr_df_tmp$DateOfBirth <- as.character(cpdr_df_tmp$DateOfBirth)
cpdr_df_tmp$DateOfBirth <- as.Date(cpdr_df_tmp$DateOfBirth, format = "%Y%m%d")
#as.POSIXct(as.character(droplevels(cpdr_df_unique$DateLoaded)), origin = "1970-01-01")
# initdata_tmp$DateOfDiagnosis <-as.POSIXlt(initdata_tmp$DateOfDiagnosis)
#c. Adding age and age category columns
cpdr_df_tmp$age <- year(as.Date(Sys.Date(), format = "%Y-%m-%d")) - year(cpdr_df_tmp$DateOfBirth)
#remove ages <0
cpdr_df_tmp[cpdr_df_tmp$age <= 0 & !is.na(cpdr_df_tmp$age),]$age <- NA
cpdr_df_tmp$ageGrp <- cut(cpdr_df_tmp$age, breaks = c(0, 54, 59, 64, 69, 74, 79, 84, Inf), na.rm = TRUE
                          , labels = c("0-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")
                          , include.lowest = TRUE)

#d. Changing county code to readable county format
require(maps)
data(county.fips)
ca_fips <- county.fips[which(substring(county.fips$fips, 1, 1)=='6'),]
ca_fips$fips <- substr(ca_fips$fips, 2, 4)
ca_fips$polyname <- gsub("california,", "", ca_fips$polyname)
cpdr_df_tmp$AddressCountyCode <- sprintf("%03d", cpdr_df_tmp$AddressCountyCode)
cpdr_df_tmp$AddressCountyNme <- NA
cpdr_df_tmp$AddressCountyNme <- ca_fips[match(cpdr_df_tmp$AddressCountyCode, ca_fips$fips),]$polyname

#create smaller cpdr_df_tmp to work with deduplication (has old unique records and new cpdr_df_tmp records)
cpdr_df_tmp_small <- cpdr_df_tmp[(!cpdr_df_tmp$IncidentID %in% record_df[[2]][!is.na(record_df[[2]]$Record.2.IncidentID),]$IncidentID)|(cpdr_df_tmp$IncidentID %in% unique(record_df[[3]]$IncidentID)),]

write.csv(cpdr_df_tmp, file = file.path("H:", "CPDR", "data", "cpdr_db_snapshot", paste("cpdr_db_", year(Sys.Date()), sprintf("%02d", month(Sys.Date())), "_clean.csv", sep = "")), row.names = FALSE)
write.csv(cpdr_df_tmp_small, file = file.path("H:", "CPDR", "data", "cpdr_db_snapshot", paste("cpdr_db_", year(Sys.Date()), sprintf("%02d", month(Sys.Date())), "_small.csv", sep = "")), row.names = FALSE)

#####Open Match*Pro for deduplication#####
system(paste(file.path("C:/Users/PIp/MatchPro/MatchPro/MatchPro.exe")), wait = FALSE)
#C:\Users\PIp\MatchPro\MatchPro\MatchPro.exe




cpdr_df_unique <- read.csv(file.path("D:", "Peter", "Data", "CPDR_snapshot", paste("non_matches_cpdr_db_", year(Sys.Date()), sprintf("%02d", month(Sys.Date())), ".csv", sep = "")))
cpdr_df_unique <- read.csv(file.path("D:", "Peter", "Data", "CPDR_snapshot", "cpdr_df_202004_test", paste("non_matches_cpdr_db_", year(Sys.Date()), sprintf("%02d", month(Sys.Date())), ".csv", sep = "")))
cpdr_df_unique <- read.csv(file.path("D:", "Peter", "Data", "CPDR_snapshot", "cpdr_db_202004_test", paste("non_matches_cpdr_db_", year(Sys.Date()), sprintf("%02d", month(Sys.Date())), ".csv", sep = "")))
cpdr_df_unique <- cpdr_df_tmp_small[cpdr_df_tmp_small$IncidentID %in% cpdr_df_unique$IncidentID,]

#####Select data from the deduplicated files#####




colnames(cpdr_df_unique)
cpdr_df_unique <- cpdr_df_tmp_small[cpdr_df_tmp_small$IncidentID %in% cpdr_df_unique$IncidentID,]
cpdr_df_unique$DateLoaded <- cpdr_df_tmp_small[match(cpdr_df_unique$IncidentID, cpdr_df_tmp_small$IncidentID),]$DateLoaded
cpdr_df_unique$DateOfDiagnosis <- cpdr_df_tmp_small[match(cpdr_df_unique$IncidentID, cpdr_df_tmp_small$IncidentID),]$DateOfDiagnosis
#Identify any additional cases not necessarily captured by deduplication (make sure prior Record.2.IncidentID is removed)
init_df <- cpdr_df_tmp[!(cpdr_df_tmp$IncidentID %in% cpdr_df_unique$IncidentID)&
                         (!cpdr_df_tmp$Record.2.IncidentID %in% cpdr_df_unique$IncidentID)&
                         (is.na(cpdr_df_tmp$Record.2.IncidentID)),]
#init_df <- init_df[!duplicated(init_df[,c("LastName", "FirstName", "DateOfBirth")]),]
nrow(init_df[!(cpdr_df_unique[,c("LastName", "FirstName", "DateOfBirth")] %in% init_df[,c("LastName", "FirstName", "DateOfBirth")]),])
init_df <- init_df[!duplicated(init_df[,c("LastName", "FirstName", "DateOfBirth")]),]
#Matches that were unique...aka non-matches
cpdr_df_unique <- read.csv(file.path("D:", "Peter", "Data", "CPDR_snapshot", paste("non_matches_cpdr_db_", year(Sys.Date()), sprintf("%02d", month(Sys.Date())), ".csv", sep = "")))
cpdr_df_unique <- read.csv(file.path("D:", "Peter", "Data", "CPDR_snapshot", "cpdr_df_202004_test", paste("non_matches_cpdr_db_", year(Sys.Date()), sprintf("%02d", month(Sys.Date())), ".csv", sep = "")))
cpdr_df_unique <- read.csv(file.path("D:", "Peter", "Data", "CPDR_snapshot", "cpdr_db_202004_test", paste("non_matches_cpdr_db_", year(Sys.Date()), sprintf("%02d", month(Sys.Date())), ".csv", sep = "")))
cpdr_df_unique <- cpdr_df_tmp_small[cpdr_df_tmp_small$IncidentID %in% cpdr_df_unique$IncidentID,]
cpdr_df_unique$DateLoaded <- cpdr_df_tmp_small[match(cpdr_df_unique$IncidentID, cpdr_df_tmp_small$IncidentID),]$DateLoaded
cpdr_df_unique$DateOfDiagnosis <- cpdr_df_tmp_small[match(cpdr_df_unique$IncidentID, cpdr_df_tmp_small$IncidentID),]$DateOfDiagnosis
#Identify any additional cases not necessarily captured by deduplication (make sure prior Record.2.IncidentID is removed)
init_df <- cpdr_df_tmp[!(cpdr_df_tmp$IncidentID %in% cpdr_df_unique$IncidentID)&
                         (!cpdr_df_tmp$Record.2.IncidentID %in% cpdr_df_unique$IncidentID)&
                         (is.na(cpdr_df_tmp$Record.2.IncidentID)),]
init_df <- init_df[!duplicated(init_df[,c("LastName", "FirstName", "DateOfBirth")]),]
#Go through deduplication of init_df again if not selecting quick unique files from init_df
#init_df$DateOfDiagnosis <- as.Date(init_df$DateOfDiagnosis)
init_df$DateOfDiagnosis <- as.POSIXct(init_df$DateOfDiagnosis)
View(init_df)


colnames(bind_rows(cpdr_df_unique, init_df[!colnames(init_df) %in% c("Record.2.IncidentID", "Total.Score")]))
cpdr_df_unique <- bind_rows(cpdr_df_unique, init_df[!colnames(init_df) %in% c("Record.2.IncidentID", "Total.Score")])
colnames(left_join(cpdr_df_unique, record_df[[3]][,c("IncidentID", "lat", "lon", "rs_lat", "rs_lon", "dist")], by = "IncidentID", all = TRUE))
cpdr_df_unique <- left_join(cpdr_df_unique, record_df[[3]][,c("IncidentID", "lat", "lon", "rs_lat", "rs_lon", "dist")], by = "IncidentID", all = TRUE)
nrow(cpdr_df_unique[!duplicated(cpdr_df_unique$IncidentID),])
colnames(cpdr_df_unique[!duplicated(cpdr_df_unique$IncidentID),])
#Filling provider addresses not filled with current provider info if available
cpdr_df_unique[cpdr_df_unique$rs_lat %in% NA,]$rs_lon <- cpdr_provider_gis[match(cpdr_df_unique[cpdr_df_unique$rs_lat %in% NA,]$ReportingFacilityName, cpdr_provider_gis$ReportingFacilityName),]$rs_lon
cpdr_df_unique[cpdr_df_unique$rs_lat %in% NA,]$rs_lat <- cpdr_provider_gis[match(cpdr_df_unique[cpdr_df_unique$rs_lat %in% NA,]$ReportingFacilityName, cpdr_provider_gis$ReportingFacilityName),]$rs_lat
colnames(cpdr_df_unique[!duplicated(cpdr_df_unique$IncidentID),])
cpdr_df_unique <- cpdr_df_unique[!duplicated(cpdr_df_unique$IncidentID),]
write.csv(cpdr_df_tmp, file = file.path("D:", "Peter", "Data", "CPDR_snapshot", paste("cpdr_db_", year(Sys.Date()), sprintf("%02d", month(Sys.Date())), "_clean.csv", sep = "")), row.names = FALSE)



record_df[[1]] <- cpdr_df
record_df[[2]] <- cpdr_df_tmp
record_df[[5]] <- match_key
colnames(cpdr_df_unique)
#after updating addresses
write.csv(cpdr_df_unique, file = file.path("D:", "Peter", "Data", "CPDR_snapshot", paste("cpdr_index_", year(Sys.Date()), sprintf("%02d", month(Sys.Date())), ".csv", sep = "")), row.names = FALSE)
record_df[[3]] <- cpdr_df_unique
record_df[[4]] <- cpdr_provider_gis
# Need to revise for updated dataset
save(record_df, file = file.path("D:", "Peter", "Data", "CPDR_snapshot", "cpdr_db_202003.Rdata"))
#Couple different approaches to getting unique counts of facilities by inidividual patients
#1.




####Scrap data to review:
bind_rows(cpdr_df_unique, init_df[!colnames(init_df) %in% c("Record.2.IncidentID", "Total.Score")])
View(init_df)
#Identify any additional cases not necessarily captured by deduplication (make sure prior Record.2.IncidentID is removed)
init_df <- cpdr_df_tmp[(!cpdr_df_tmp$IncidentID %in% cpdr_df_unique$IncidentID)&
                         (!cpdr_df_tmp$Record.2.IncidentID %in% cpdr_df_unique$IncidentID)&
                         (is.na(cpdr_df_tmp$Record.2.IncidentID)),]
#init_df <- init_df[!duplicated(init_df[,c("LastName", "FirstName", "DateOfBirth")]),]
nrow(init_df[!(cpdr_df_unique[,c("LastName", "FirstName", "DateOfBirth")] %in% init_df[,c("LastName", "FirstName", "DateOfBirth")]),])
#Go through deduplication of init_df again if not selecting quick unique files from init_df
#init_df$DateOfDiagnosis <- as.Date(init_df$DateOfDiagnosis)
init_df$DateOfDiagnosis <- as.POSIXct(init_df$DateOfDiagnosis)
View(init_df)
init_df[!duplicated(init_df[,c("LastName", "FirstName", "DateOfBirth")]),]
View(init_df[!duplicated(init_df[,c("LastName", "FirstName", "DateOfBirth")]),])




cpdr_df_tmp %>% filter(!is.na(Total.Score)) %>% group_by(Record.2.IncidentID) %>% summarise(uniqueProviders = n_distinct(ReportingFacilityName))

#####Import deduplicate files for selection and cleanup#####


#####Create data files for archive#####
save(record_df, file = file.path("D:", "Peter", "Data", "CPDR_snapshot", paste0("cpdr_db_", format(Sys.Date(), "%Y%m%d"), ".Rdata", sep = "")))

#####Upload data for the SQL Server#####


#####Files created for RStudio Server (Skylab)#####
cpdr_index <- cpdr_df_unique
# cpdr_index$DateDeathYr <- year()
cpdr_index$DateOfDeath <- as.character(cpdr_index$DateOfDeath)
#cpdr_index[!is.na(cpdr_index$DateOfDeath),]$DateOfDeath <- as.character(cpdr_index[!is.na(cpdr_index$DateOfDeath),]$DateOfDeath)
cpdr_index[(grepl("/", cpdr_index$DateOfDeath) & nchar(as.character(cpdr_index$DateOfDeath)) <= 7),]$DateOfDeath <- format(as.Date(strptime(as.character(cpdr_index[(grepl("/", cpdr_index$DateOfDeath) & nchar(as.character(cpdr_index$DateOfDeath)) <=7),]$DateOfDeath), format = "%M/%Y")), "%Y%m%d")
cpdr_index[(grepl("/", cpdr_index$DateOfDeath)),]$DateOfDeath                                                   <- format(as.Date(strptime(as.character(cpdr_index[(grepl("/", cpdr_index$DateOfDeath)),]$DateOfDeath), format = "%m/%d/%y")), "%Y%m%d")
cpdr_index$DtDeathYr <- NA
cpdr_index[!is.na(cpdr_index$DateOfDeath),]$DtDeathYr <- year(as.Date(cpdr_index[!is.na(cpdr_index$DateOfDeath),]$DateOfDeath, format = "%Y%m%d"))
cpdr_index$DtDeathWk <- NA
cpdr_index$DtDeathWk <- getWeek(as.Date(cpdr_index$DateOfDeath, format = "%Y%m%d"))
cpdr_index$DtDeathMnth <- NA
cpdr_index$DtDeathMnth <- month(as.Date(cpdr_index$DateOfDeath, format = "%Y%m%d"), label = TRUE, abbr = TRUE)
cpdr_index$DateOfDeath <- format(as.Date(cpdr_index$DateOfDeath, format = "%Y%m%d"), '%Y-%m-%d')
cpdr_index$DtDxWk <- NA
cpdr_index$DtDxWk <- getWeek(as.Date(cpdr_index$DateOfDiagnosis, format = "%Y-%m-%d"))
cpdr_index$DtDxMnth <- NA
cpdr_index$DtDxMnth <- month(as.Date(cpdr_index$DateOfDiagnosis, format = "%Y-%m-%d"), label = TRUE, abbr = TRUE)
cpdr_index$DtDxYr <- NA
cpdr_index$DtDxYr <- year(as.Date(cpdr_index$DateOfDiagnosis, format = "%Y-%m-%d"))
cpdr_index[,!(colnames(cpdr_index) %in% c("LastName", "FirstName", "MiddleName", "MedicalRecordNumber", "SSN", "DateOfDeath", "AddressStreet"))]
colnames(cpdr_index[,!(colnames(cpdr_index) %in% c("LastName", "FirstName", "MiddleName", "MedicalRecordNumber", "SSN", "DateOfDeath", "AddressStreet"))])

cpdr_index <- cpdr_index[,!(colnames(cpdr_index) %in% c("LastName", "FirstName", "MiddleName", "MedicalRecordNumber", "SSN", "DateOfDeath", "AddressStreet"))] #DoD may be linked with publicly available death registry records

#save new cpdr_index file and facility data
save(cpdr_index, file = file.path("D:", "Peter", "Data", "CPDR_snapshot", paste0("cpdr_shiny_", format(Sys.Date(), "%Y%m%d"), ".Rdata", sep = "")))

