library(tidyverse)
MoonopolyData_ON <- readRDS("~/Desktop/Io_PS2/MoonopolyData_ON.rds")

#Import Vet Data (extracted from Getdata.R)
happyCows <- MoonopolyData_ON%>%
  map_dfr(function(x) x$results)

# import census division 
census_division <- read_sf("/home/emilyhanson/Desktop/Io_PS2/ShapeFile_census/lcd_000b16a_e.shp")


# Work with simpler data
happyCows$geometry <- happyCows$geometry$location[,c('lat', 'lng')]
happyCows1 <- happyCows%>%
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

# drop permanently closed
happyCows1 <- happyCows1 %>% filter(is.na(permanently_closed))

# drop duplicate entries 
happyCows1 <- distinct(happyCows1)

# Import Ontario Shapefile 
prov <- read_sf("/home/emilyhanson/Desktop/Io_PS2/ShapeFile_Prov/lpr_000a16a_e.shp")%>%
  st_transform(crs = 'WGS84')
prov%>%filter(PRUID == 35)%>%st_convex_hull()

# Make into shapefile
df.shapeCows <- st_as_sf(happyCows1, coords = c("lng","lat"))
st_crs(df.shapeCows) <- st_crs(prov)

#drop points in square grid outside Ontario
shapeCows <- st_intersection(df.shapeCows,prov%>%filter(PRUID == 35))


#Draw a Map
census_division%>%
  head%>%
  ggplot()+
  geom_sf()+
  geom_sf(data = shapeCows)


#Draw a Map
prov%>%
   head%>%
   ggplot()+
   geom_sf()+
   geom_sf(data = shapeCows)

