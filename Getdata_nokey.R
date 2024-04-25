library(tidyverse)
library(ggplot2)
library(sf)
library(mapsapi)
library(googleway)

# You need a Google key for the API to link billing
key = ""
set_key(key)

#import statcan geos 
#econ_regions <- read_sf("/home/emilyhanson/Desktop/Io_PS2/ShapeFile_econ/ler_000a16a_e.shp")
#census_division <- read_sf("/home/emilyhanson/Desktop/Io_PS2/ShapeFile_census/lcd_000b16a_e.shp")
#csd <- read_sf("/home/emilyhanson/Desktop/Io_PS2/ShapeFile_CSD/lcsd000a16a_e.shp")

prov <- read_sf("/home/emilyhanson/Desktop/Io_PS2/ShapeFile_Prov/lpr_000a16a_e.shp")%>%
  st_transform(crs = 'WGS84')

#Filter only Ontario
prov%>%filter(PRUID == 35)%>%st_convex_hull()  

# Create a lat long grid of point that covers all Ontario
y <- seq(from = 41.67695, to = 89.99943, by = 0.58)
x <- seq(from = -141.0181, to = -52.5823, by = 0.45)
xy <- expand.grid(x, y)

#convert grid to shape file
df.SPxy <- st_as_sf(xy, coords = c("Var1","Var2"))
st_crs(df.SPxy) <- st_crs(prov)

#drop points in square grid outside Ontario
ontario_points <- st_intersection(df.SPxy,prov%>%filter(PRUID == 35))

#convert back from shapefile to simple data frame
my_points <- data.frame(matrix( ncol = 2, nrow = nrow(ontario_points)))
for (i in 1:nrow(my_points)){
  my_points[i,2] <- sf::st_bbox(ontario_points[i,1])[1]
  my_points[i,1] <- sf::st_bbox(ontario_points[i,1])[2]
}

# CALL GOOGLE API
VET_DATA <- list()
for(i in 1:nrow(my_points)){
  jesusVar <<- i # for debug
  VET_DATA[[i]] <- googleway::google_places(search_string = "veterinarian", location = c(my_points[i,1],my_points[i,2]) , radius = 50000)
  Sys.sleep(1) # To calm down API
}

write_rds(VET_DATA, '/home/emilyhanson/Desktop/Io_PS2/MoonopolyData_ON.rds')

# Other code used to plot shape file thingies
# 
# ggplot(prov) +
#   geom_sf(fill = "#69b3a2", color = "white") +
#   theme_void()
# 
# csd%>%head%>%st_centroid() -> temp
# csd%>%
#   head%>%
#   ggplot()+
#   geom_sf()+
#   geom_sf(data = ontario_points)
# 
# nanai <- csd%>%
#   filter(SACCODE == 938)
# 
# nanaiPoint <- nanai%>%st_centroid()
# 
# nanai%>%
#   lwgeom::st_minimum_bounding_circle()%>%
#   ggplot()+
#   geom_sf(aes(col = CSDNAME), alpha = .1)+
#   geom_sf(data = nanai, aes(col = CSDNAME), alpha = .2)+
#   geom_sf(data = nanaiPoint, aes(col = CSDNAME))
#   
# 
# nanaiConv <- nanai%>%
#   st_convex_hull()
# 
# nanaiConvPoint <- nanaiConv%>%
#   st_centroid()
# 
# nanaiConv%>%
#   ggplot()+
#   geom_sf()+
#   geom_sf(data = nanaiConv)
# 
