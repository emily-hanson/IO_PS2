# Code by Emily Hanson with consultation and assistance from Lance Taylor
# April 2024

# notes: 
#     - st_buffer is the most computationally expensive cammand in this file
#     - 

# SET FOLDER for imports
folder <- "G:\\My Drive\\0_Western2ndYear\\Class_IO_Daniel\\PS 2\\Untitled folder\\"

library(tidyverse)
library(tidyverse)
library(ggplot2)
library(sf)
sf_use_s2(FALSE) #helps st_intersection not throw errors


## ----------------------------------------------------------- IMPORT ---------------------------------------- ##----

##------Import Establishment Data (extracted from Getdata.R)
Vet_DATA<- folder%>%
    paste0('\\Data\\Google Data Scrape\\Vet_Data_canada.rds')%>%
    readRDS
estab_df <- Vet_DATA%>%
    map_dfr(function(x) x$results)
#rm(EstablishmentData_ON)

##------Import Geographies as shape files (possibly not all will be used)
geo_prov <- folder%>%
    paste0('Data\\ShapeFile_prov\\lpr_000a21a_e.shp')%>%
    read_sf
geo_cma <- folder%>%
    paste0('Data\\ShapeFile_cma\\lcma000a21a_e.shp')%>%
    read_sf
geo_popCen <- folder%>%
    paste0('Data\\ShapeFile_popCentre\\lpc_000a21a_e.shp')%>%
    read_sf

##------Import Market Characteristics by Population center
# Must convert from long to wide

# desired characteristics from data file for C1_COUNT_TOTAL
selected_char_ID <- c(1, 2, 3, 4, 5, 6, 7, 24, 37, 40, 41, 42, 57, 59, 77, 106, 113, 115, 121, 128, 123, 206, 208, 214, 216, 1403, 1412, 1414, 1415, 1416, 1469, 1471, 1486, 1487, 1488, 1489, 1494, 1495, 1528, 1529, 1684, 1998, 1999, 2000, 2002, 2009, 2223, 2224, 2225, 2226, 2227, 2228, 2229, 2230, 2593, 2594, 2595, 2596, 2597, 2598, 2599, 2600, 2601, 2602, 2603, 2604, 2607, 2608, 2609, 2610, 2611, 2612, 2613, 2614, 2615, 2616)
selected_char_ID_rate <- c(42) # desired characteristics from data file for C10_RATE_TOTAL
# if you change these lists, you will need to change the rename line at end of this code block

mrkChar_bypopCen <- folder%>%
    paste0('Data\\PopData_bypopCenter\\98-401-X2021009_English_CSV_data.csv')%>%
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
mrkChar_bypopCen <- mrkChar_bypopCen

## -----------------------------------------------Merge adn Market Restrictions ---------------------------------------- ##----

##------Merge market characteristics into geo shape files
# merge market char into population center geo file
markets <- full_join(geo_popCen, mrkChar_bypopCen, by = "DGUID") %>%
  select( -PCUID, -PCPUID, -DGUIDP, -PCTYPE, -PCCLASS)%>%filter(PRUID < 60)%>%
  group_by(DGUID)%>%
  slice_head(n = 1)%>%
  ungroup

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


##------Export 
  
Markets_Data <- markets_desired
write_rds(Markets_Data , "G:\\My Drive\\0_Western2ndYear\\Class_IO_Daniel\\PS 2\\Untitled folder\\Markets_Data.rds")
Markets_Data_csv <- as.data.frame(Markets_Data) %>% select(-geometry) %>% apply(2, as.character)
write.csv(Markets_Data_csv, "G:\\My Drive\\0_Western2ndYear\\Class_IO_Daniel\\PS 2\\Untitled folder\\Markets_Data_csv.csv")




