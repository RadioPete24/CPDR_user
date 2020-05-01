gis_pkg_path <- file.path("C:", "CPDR", "programs", "gis_install_pkg.R")
source(gis_pkg_path)

temp_df <- cpdr_df_unique[cpdr_df_unique$lat %in% NA & !grepl("PO BOX", cpdr_df_unique$AddressStreet),]
temp_df$AddressStreet <- droplevels(temp_df$AddressStreet)
location <- temp_df[,c("AddressStreet", "AddressCity", "AddressState", "AddressZipCode")]

location <- paste(location$AddressStreet, location$AddressCity, location$AddressState, location$AddressZipCode, sep = ", ")



url_base <- "https://geocoding.geo.census.gov/geocoder/locations/onelineaddress?address=" #eg. https://geocoding.geo.census.gov/geocoder/locations/onelineaddress?address=3728+GLENEAGLES+DR%2C+STOCKTON%2C+CA%2C+95219&benchmark=4
url_query <- location %>% stringr::str_trim() %>% str_replace_all(",", "%2C") %>% str_replace_all(" ", "+")
url_query <- paste0(url_query, "&benchmark=4")
url_query <- url_query %>% URLencode(reserved = FALSE) %>% c("address" = .)
#url_query <- location %>% str_trim() %>% str_replace_all("%", " +") #(",", "%2C)(" ", "+") %>% URLencode(reserved = FALSE) %>% c("address" = .)
url_address <- paste(url_base, url_query, sep = "")
url_address <- stringr::str_c(url_base, url_query)
#url <- str_c(names(url_query), url_query, sep = "=", collapse = "&") #Collapse all addresseses to one string for batch?
loc <- lst()
for(i in 1:length(url_address)){
  response <- httr::GET(url_address[i])
  geom_coord <- httr::content(response)
  if(length(geom_coord$result$addressMatches)==0){
    lon <- NA
    lat <- NA
  } else {
    lon <- geom_coord$result$addressMatches[[1]]$coordinates$x #X is longitude
    lat <- geom_coord$result$addressMatches[[1]]$coordinates$y #Y is latitude
  }
  loc[[i]] <- cbind(lon, lat)
}
loc <- do.call(rbind, loc)
loc <- as.data.frame(loc)
proc_time <- Sys.time() - start_time
for(i in 1:length(url_address)){
  response <- httr::GET(url_address[i])
  geom_coord <- httr::content(response)
  if(length(geom_coord$result$addressMatches)==0){
    lon <- NA
    lat <- NA
  } else {
    lon <- geom_coord$result$addressMatches[[1]]$coordinates$x #X is longitude
    lat <- geom_coord$result$addressMatches[[1]]$coordinates$y #Y is latitude
  }
  loc[[i]] <- cbind(lon, lat)
}
loc <- do.call(rbind, loc)
loc <- as.data.frame(loc)
proc_time <- Sys.time() - start_time