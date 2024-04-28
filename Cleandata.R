# Code by Emily Hanson with consultation and assistance from Lance Taylor
# April 2024

# notes: 
#     - st_buffer is the most computationally expensive cammand in this file
#     - 

# SET FOLDER for imports
folder <- "/home/emilyhanson/Desktop/Io_PS2/Data/"

library(tidyverse)
sf_use_s2(FALSE) #helps st_intersection not throw errors


## ----------------------------------------------------------- IMPORT ---------------------------------------- ##----

##------Import Establishment Data (extracted from Getdata.R)
EstablishmentData_ON <- folder%>%
    paste0('Vet_Data_canada.rds')%>%
    readRDS
estab_df <- EstablishmentData_ON%>%
    map_dfr(function(x) x$results)
#rm(EstablishmentData_ON)

##------Import Geographies as shape files (possibly not all will be used)
geo_prov <- folder%>%
    paste0('ShapeFile_prov/lpr_000a21a_e.shp')%>%
    read_sf
# geo_cen_div <- folder%>%
#     paste0('ShapeFile_census/lcd_000a21a_e.shp')%>%
#     read_sf
# geo_csd <- folder%>%
#     paste0('ShapeFile_CSD/lcsd000a21a_e.shp')%>%
#     read_sf
geo_cma <- folder%>%
    paste0('ShapeFile_cma/lcma000a21a_e.shp')%>%
    read_sf
geo_popCen <- folder%>%
    paste0('ShapeFile_popCentre/lpc_000a21a_e.shp')%>%
    read_sf

##------Import Market Characteristics by Population center
# Must convert from long to wide

selected_char_ID <- c(1,2,42,76,77) # desired characteristics from data file for C1_COUNT_TOTAL
selected_char_ID_rate <- c(42) # desired characteristics from data file for C10_RATE_TOTAL
# if you change these lists, you will need to change the rename line at end of this code block

mrkChar_bypopCen <- folder%>%
    paste0('PopData_bypopCenter/98-401-X2021009_English_CSV_data.csv')%>%
    read_csv(col_types = cols(.default = "c", CHARACTERISTIC_ID = "d", C1_COUNT_TOTAL = "d", C10_RATE_TOTAL = "d"))

mrkChar_bypopCen$C10_RATE_TOTAL <- ifelse(mrkChar_bypopCen$CHARACTERISTIC_ID %in% selected_char_ID_rate,
                                        mrkChar_bypopCen$C10_RATE_TOTAL, NA)
mrkChar_bypopCen <- mrkChar_bypopCen%>%
    filter(CHARACTERISTIC_ID %in% selected_char_ID )%>%
    select(DGUID, GEO_NAME, CHARACTERISTIC_NAME, C1_COUNT_TOTAL, C10_RATE_TOTAL)

mrkChar_bypopCen <- mrkChar_bypopCen%>%
    pivot_wider(names_from = CHARACTERISTIC_NAME, values_from = c(C1_COUNT_TOTAL, C10_RATE_TOTAL))%>%
    select(-any_of({.}%>%
                   sapply(function(x) mean(is.na(x)))%>%
                   subset(. == 1)%>%
                   names))
rm(selected_char_ID, selected_char_ID_rate)

# Change this if you add characteristics 
mrkChar_bypopCen <- mrkChar_bypopCen%>%rename("Pop2021" = 3, "Pop2016" = 4, 
                                          "Single_detached_count" = 5, 
                                          "Avg_fam_size " = 6,
                                          "Avg_children_if_children" = 7,
                                          "Single_detached_rate" = 8)

##------Import Market Population by CSD instead of population center
# popData_CSD <- folder %>%
#    paste0('PopData_byCSD/98100002.csv')%>%read_csv
# popData_CSD<- popData_CSD%>%
#   mutate(my_CDID = substring(DGUID, 10,14))%>%
#   mutate(my_CSDID = substring(DGUID, 10,17))%>%
#   subset(popData_CSD, select = c(2, 3, 5, 7, 31, 32))%>%
#   rename("Pop2021" =3, "Pop2016" = 4)



## ------------------------------------------ Clean Establishment Data ---------------------------------------- ##----

# Simple data cleaning 
estab_df$geometry <- estab_df$geometry$location[,c('lat', 'lng')]
estab_df <- estab_df%>%
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
estab_df <- distinct(estab_df)%>%
      filter(is.na(permanently_closed))

# Make into shapefile using PCS_Lambert_Conformal_Conic reference system
estab_sf <- st_as_sf(estab_df, coords = c("lng","lat"), crs = 'WGS84')
estab_sf <- st_transform(estab_sf, crs = st_crs(geo_prov))
rm(estab_df)

#drop points outside Canadian provinces 
estab_sf <- st_intersection(estab_sf,geo_prov%>%filter(PRUID < 60))

## ------------------------------------------ Work with Market Level Data ---------------------------------------- ##----
# Change mindset from data of establishments to data of markets / places

##------Merge market characteristics into geo shape files
# merge market char into population center geo file
markets <- full_join(geo_popCen, mrkChar_bypopCen, by = "DGUID") %>%
  select( -PCUID, -PCPUID, -DGUIDP, -PCTYPE, -PCCLASS)%>%filter(PRUID < 60)%>%
  group_by(DGUID)%>%
  slice_head(n = 1)%>%
  ungroup

# duplicate places
# markets %>% filter(DGUID %in% subset(markets, duplicated(markets$DGUID))$DGUID)

# merge market char(population) into csd geo file
#geo_csd <- left_join(geo_csd, popData_CSD, by = c("CSDUID" ="my_CSDID" ))
#geo_csd <- geo_csd%>%rename("Pop2021" =7)%>%rename("Pop2016" = 8)


##------Impose market requirements on population centers

# restrict to population centers outside and 20 km away from CMA
geo_cma_buffer <- geo_cma %>%
      filter(PRUID < 60) %>%
      st_buffer(20000)
markets_exclude_cma <- st_intersection(markets, geo_cma_buffer)%>%
  distinct(DGUID, exclude_reason = "Inside CMA plus 20 km buffer")

# restrict to population centers further than 10 km away from another population center
temp_df <- st_nearest_feature(markets)%>%
  as.data.frame(DGUID = markets$DGUID)
temp_df2 <- data.frame(DGUID = markets$DGUID, 
           DGUIDnearest = markets$DGUID[temp_df[[1]]],
           dist = st_distance(markets%>%
                         select(DGUID),
                       markets[temp_df[[1]],]%>%
                         select(DGUID),
                       by_element = TRUE))
temp_df3 <- temp_df2 %>% filter(as.numeric(dist) > 10000)
rm(temp_df, temp_df2)

markets_exclude <- markets%>%
  filter(! DGUID %in% temp_df3 $DGUID)%>%
  mutate(exclude_reason = "Within 10km of another population center")%>%
  bind_rows(markets%>%
              filter(DGUID %in% markets_exclude_cma$DGUID)%>%
              mutate(exclude_reason = "Inside CMA plus 20 km buffer"))%>%
  group_by(DGUID)%>%
  mutate(exclude_reason = paste(unique(exclude_reason), collapse = '; '))%>%
  slice_head(n = 1)%>%
  ungroup
rm(markets_exclude_cma)

markets_desired <- markets%>%filter(! DGUID %in% markets_exclude$DGUID)

##------Count establishments in each of my markets
markets_desired <- markets_desired%>%
  mutate(numberOfEstablishments = st_intersects({.},estab_sf)%>%
           sapply(function(x) x%>%length))

markets_desired <- markets_desired%>% st_buffer(1000) %>%
  mutate(numberOfEstablishments_1kmBuffer = st_intersects({.},estab_sf)%>%
           sapply(function(x) x%>%length))

##------My Market should now have all the data needed for market analysis


## ------------------------------------------ Checks, Excluded Market Statistics, Pretty Pictures ------------------------------------- ##-----

# Count establishments total and thrown away

count_est_exclude1 <- st_intersects(st_union(geo_cma_buffer),estab_sf)
count_est_exclude1 <- length(count_est_exclude1[[1]])

count_est_exclude2 <- markets_exclude%>%
    filter(exclude_reason == "Within 10km of another population center" ) %>%
    st_union()%>%
    st_intersects(estab_sf)
count_est_exclude2 <- length(count_est_exclude2[[1]])

# count est outside CMA buffer and outside my markets 
nrow(estab_sf) - count_est_exclude1 - count_est_exclude2 - length(markets_desired$numberOfEstablishments_1kmBuffer)

geo_prov%>%
  filter(PRUID < 60)%>%
  ggplot()+
  geom_sf()+
  geom_sf(data = markets_desired%>%
            mutate(numberOfVets= factor(numberOfEstablishments)),
          aes(col = numberOfVets), size = 2)+
  geom_sf(data = markets_exclude,
          size = 2)


