# Code by Emily Hanson with consultation and assistance from Lance Taylor
# May 2024

# Here we are collecting and cleaning data for additional industries
# Instead of searching all of Canada, we only search the markets selected in cleandata.R plus a 5 km buffer

library(tidyverse)
library(ggplot2)
library(sf)
library(lwgeom)
library(mapsapi)
library(googleway)

folder <- "G:\\My Drive\\0_Western2ndYear\\Class_IO_Daniel\\PS 2\\Untitled folder\\"

####--------------------------------------------------- Get Search Points and Radius from Market_Data ----------------------####-----

# import marketData
Markets_Data <- folder%>%
  paste0('Markets_Data.rds')%>%
  readRDS

#Find max radius of population centres in our market   
markets <- Markets_Data %>%
  mutate ( circle  = st_minimum_bounding_circle(Markets_Data$geometry) ) %>%
  select( circle , geometry, DGUID)

max_area <- max(st_area(markets$circle))
max_radius <- as.numeric(sqrt( max_area / pi))

#covert polygon geo into point geo
sf_points <- markets %>% st_centroid() %>% select( geometry)

#convert to lat lng
sf_points <- sf_points %>% st_transform(crs = 'WGS84')

#convert back from shapefile to simple data frame
my_points <- data.frame(matrix( ncol = 2, nrow = nrow(sf_points)))
for (i in 1:nrow(sf_points)){
  my_points[i,2] <- sf::st_bbox(sf_points[i,1])[1]
  my_points[i,1] <- sf::st_bbox(sf_points[i,1])[2]
}

####--------------------------------------------------- Call API ----------------------####-----

## ------------------- Dentists -------------
#collected May 4
# Shuning Google payment key
#key = ""
#set_key(key)
# 
# Dentist_DATA <- list()
# for(i in 1:nrow(my_points)){
#   jesusVar <<- i # for debug
#   Dentist_DATA[[i]] <- googleway::google_places(search_string = "Dentist", location = c(my_points[i,1],my_points[i,2]) , radius = max_radius +5010)
#   Sys.sleep(1+runif(1)) # To calm down API
# }
# write_rds(Dentist_DATA, 'G:\\My Drive\\0_Western2ndYear\\Class_IO_Daniel\\PS 2\\Untitled folder\\Data\\Dentist_DATA.rds')
# DoNotDeleteData1 <- Dentist_DATA

## ------------------ Funeral ------------
# Collected May 4
# Nalinda Google payment key
# key = ""
# set_key(key)
#  
#  Funeral_DATA <- list()
#  for(i in 1:nrow(my_points)){
#    jesusVar <<- i # for debug
#    Funeral_DATA[[i]] <- googleway::google_places(search_string = "Funeral Home", location = c(my_points[i,1],my_points[i,2]) , radius = max_radius +5010)
#    Sys.sleep(1+runif(1)) # To calm down API
#  }
#  write_rds(Funeral_DATA, 'G:\\My Drive\\0_Western2ndYear\\Class_IO_Daniel\\PS 2\\Untitled folder\\Data\\Funeral_DATA.rds')
#  DoNotDeleteData2 <- Funeral_DATA

## ------------------ Funeral ------------
# Collected May 4
# Nalinda Google payment key
#  key = ""
#  set_key(key)
#   
#   Mech_DATA <- list()
#   for(i in 1:nrow(my_points)){
#     jesusVar <<- i # for debug
#     Mech_DATA[[i]] <- googleway::google_places(search_string = "Mechanic", location = c(my_points[i,1],my_points[i,2]) , radius = max_radius +5010)
#     Sys.sleep(1+runif(1)) # To calm down API
#   }
# write_rds(Mech_DATA, 'G:\\My Drive\\0_Western2ndYear\\Class_IO_Daniel\\PS 2\\Untitled folder\\Data\\Mech_DATA.rds')
#   DoNotDeleteData3 <- Mech_DATA

##--------------------------------------------------------------------- Clean New & Get Counts Data ------------------------------ ## -----

geo_prov <- folder%>%
  paste0('Data\\ShapeFile_prov\\lpr_000a21a_e.shp')%>%
  read_sf

Markets_Data_moreInd <- Markets_Data %>% 
  rename( nVets = numberOfEstablishments, nvets_1kmBuffer = numberOfEstablishments_1kmBuffer)

##-------------------------- Dentists -------

estab_dentist_df <- Dentist_DATA%>%
  map_dfr(function(x) x$results)

# Simple data cleaning 
estab_dentist_df$geometry <- estab_dentist_df$geometry$location[,c('lat', 'lng')]
estab_dentist_df <- estab_dentist_df%>%
  group_by(place_id)%>%
  transmute(formatted_address, 
            lat = geometry$lat, 
            lng = geometry$lng,
            name, 
            business_status, 
            types = types%>%
              unlist%>%
              paste0(collapse = '___'), 
            permanently_closed)%>%
  ungroup

# drop entries: duplicates and permanently closed establishments
estab_dentist_df <- estab_dentist_df %>% distinct(estab_dentist_df$place_id, .keep_all = TRUE)
estab_dentist_df <- estab_dentist_df %>% distinct(estab_dentist_df$formatted_address , .keep_all = TRUE)
estab_dentist_df <- estab_dentist_df %>% filter(is.na(estab_dentist_df$permanently_closed ))

# Make into shapefile using PCS_Lambert_Conformal_Conic reference system
estab_dentist_sf <- st_as_sf(estab_dentist_df, coords = c("lng","lat"), crs = 'WGS84')
estab_dentist_sf <- st_transform(estab_dentist_sf, crs = st_crs(geo_prov))

#drop points outside Canadian provinces 
estab_dentist_sf <- st_intersection(estab_dentist_sf,geo_prov%>%filter(PRUID < 60))

Markets_Data_moreInd <- Markets_Data_moreInd%>%
  mutate(nDentist = st_intersects({.},estab_dentist_sf)%>%
           sapply(function(x) x%>%length))

Markets_Data_moreInd <- Markets_Data_moreInd%>% st_buffer(1000) %>%
  mutate(nDentists_1kmBuffer = st_intersects({.},estab_dentist_sf)%>%
           sapply(function(x) x%>%length))

Markets_Data_moreInd <- Markets_Data_moreInd%>% st_buffer(2000) %>%
  mutate(nDentists_2kmBuffer = st_intersects({.},estab_dentist_sf)%>%
           sapply(function(x) x%>%length))

Markets_Data_moreInd <- Markets_Data_moreInd%>% st_buffer(5000) %>%
  mutate(nDentists_5kmBuffer = st_intersects({.},estab_dentist_sf)%>%
           sapply(function(x) x%>%length))

##-------------------------- Funeral ------

estab_funeral_df <- Funeral_DATA%>%
  map_dfr(function(x) x$results)

# Simple data cleaning 
estab_funeral_df$geometry <- estab_funeral_df$geometry$location[,c('lat', 'lng')]
estab_funeral_df <- estab_funeral_df%>%
  group_by(place_id)%>%
  transmute(formatted_address, 
            lat = geometry$lat, 
            lng = geometry$lng,
            name, 
            business_status, 
            types = types%>%
              unlist%>%
              paste0(collapse = '___'), 
            permanently_closed)%>%
  ungroup

# drop entries: duplicates and permanently closed establishments
estab_funeral_df <- estab_funeral_df %>% distinct(estab_funeral_df$place_id, .keep_all = TRUE)
estab_funeral_df <- estab_funeral_df %>% distinct(estab_funeral_df$formatted_address , .keep_all = TRUE)
estab_funeral_df <- estab_funeral_df %>% filter(is.na(estab_funeral_df$permanently_closed ))

# Make into shapefile using PCS_Lambert_Conformal_Conic reference system
estab_funeral_sf <- st_as_sf(estab_funeral_df, coords = c("lng","lat"), crs = 'WGS84')
estab_funeral_sf <- st_transform(estab_funeral_sf, crs = st_crs(geo_prov))

#drop points outside Canadian provinces 
estab_funeral_sf <- st_intersection(estab_funeral_sf,geo_prov%>%filter(PRUID < 60))

Markets_Data_moreInd <- Markets_Data_moreInd%>%
  mutate(nfuneral = st_intersects({.},estab_funeral_sf)%>%
           sapply(function(x) x%>%length))

Markets_Data_moreInd <- Markets_Data_moreInd%>% st_buffer(1000) %>%
  mutate(nfunerals_1kmBuffer = st_intersects({.},estab_funeral_sf)%>%
           sapply(function(x) x%>%length))

Markets_Data_moreInd <- Markets_Data_moreInd%>% st_buffer(2000) %>%
  mutate(nfunerals_2kmBuffer = st_intersects({.},estab_funeral_sf)%>%
           sapply(function(x) x%>%length))

Markets_Data_moreInd <- Markets_Data_moreInd%>% st_buffer(5000) %>%
  mutate(nfunerals_5kmBuffer = st_intersects({.},estab_funeral_sf)%>%
           sapply(function(x) x%>%length))

mrk_csv <- as.data.frame(Markets_Data_moreInd) %>% select(-geometry) %>% apply(2, as.character)
write.csv(mrk_csv, "G:\\My Drive\\0_Western2ndYear\\Class_IO_Daniel\\PS 2\\Untitled folder\\Markets_Data_moreInd.csv")

##-------------------------- Mech ------

estab_mech_df <- Mech_DATA%>%
  map_dfr(function(x) x$results)

# Simple data cleaning 
estab_mech_df$geometry <- estab_mech_df$geometry$location[,c('lat', 'lng')]
estab_mech_df <- estab_mech_df%>%
  group_by(place_id)%>%
  transmute(formatted_address, 
            lat = geometry$lat, 
            lng = geometry$lng,
            name, 
            business_status, 
            types = types%>%
              unlist%>%
              paste0(collapse = '___'), 
            permanently_closed)%>%
  ungroup

# drop entries: duplicates and permanently closed establishments
estab_mech_df <- estab_mech_df %>% distinct(estab_mech_df$place_id, .keep_all = TRUE)
estab_mech_df <- estab_mech_df %>% distinct(estab_mech_df$formatted_address , .keep_all = TRUE)
estab_mech_df <- estab_mech_df %>% filter(is.na(estab_mech_df$permanently_closed ))

# Make into shapefile using PCS_Lambert_Conformal_Conic reference system
estab_mech_sf <- st_as_sf(estab_mech_df, coords = c("lng","lat"), crs = 'WGS84')
estab_mech_sf <- st_transform(estab_mech_sf, crs = st_crs(geo_prov))


#drop points outside Canadian provinces 
estab_mech_sf <- st_intersection(estab_mech_sf,geo_prov%>%filter(PRUID < 60))

Markets_Data_moreInd <- Markets_Data_moreInd%>%
  mutate(nmech = st_intersects({.},estab_mech_sf)%>%
           sapply(function(x) x%>%length))

Markets_Data_moreInd <- Markets_Data_moreInd%>% st_buffer(1000) %>%
  mutate(nmechs_1kmBuffer = st_intersects({.},estab_mech_sf)%>%
           sapply(function(x) x%>%length))

Markets_Data_moreInd <- Markets_Data_moreInd%>% st_buffer(2000) %>%
  mutate(nmechs_2kmBuffer = st_intersects({.},estab_mech_sf)%>%
           sapply(function(x) x%>%length))

Markets_Data_moreInd <- Markets_Data_moreInd%>% st_buffer(5000) %>%
  mutate(nmechs_5kmBuffer = st_intersects({.},estab_mech_sf)%>%
           sapply(function(x) x%>%length))

mrk_csv <- as.data.frame(Markets_Data_moreInd) %>% select(-geometry) %>% apply(2, as.character)
write.csv(mrk_csv, "G:\\My Drive\\0_Western2ndYear\\Class_IO_Daniel\\PS 2\\Untitled folder\\Markets_Data_moreInd.csv")


