# Code by Emily Hanson with consultation and assistance from Lance Taylor
# April 2024

library(tidyverse)
library(ggplot2)
library(sf)
library(mapsapi)
library(googleway)

folder <- "/home/emilyhanson/Desktop/Io_PS2/Data/"

# Emily's Google payment key
key = ""
set_key(key)

#import statcan geos 
geo_prov <- folder%>%
  paste0('ShapeFile_Prov/lpr_000a16a_e.shp')%>%
  read_sf%>%
  st_transform(crs = 'WGS84')

# Create a lat long grid of point that covers all Ontario
y <- seq(from = 41.0, to = 62.5, by = 0.58) #Southern point (ON) to Northern (Quebec)
x <- seq(from = -141.05, to = -52.0, by = 0.45)  #canada is this wide
xy <- expand.grid(x, y)

#convert grid to shape file
df.SPxy <- st_as_sf(xy, coords = c("Var1","Var2"))
st_crs(df.SPxy) <- st_crs(geo_prov)

# Filter only provinces (not territories)
points <- st_intersection(df.SPxy,geo_prov%>%filter(PRUID <60))

#convert back from shapefile to simple data frame
my_points <- data.frame(matrix( ncol = 2, nrow = nrow(points)))
for (i in 1:nrow(my_points)){
  my_points[i,2] <- sf::st_bbox(points[i,1])[1]
  my_points[i,1] <- sf::st_bbox(points[i,1])[2]
}

# CALL GOOGLE API
VET_DATA <- list()
for(i in 1:nrow(my_points)){
  jesusVar <<- i # for debug
  VET_DATA[[i]] <- googleway::google_places(search_string = "veterinarian", location = c(my_points[i,1],my_points[i,2]) , radius = 50000)
  Sys.sleep(1) # To calm down API
}

write_rds(VET_DATA, '/home/emilyhanson/Desktop/Io_PS2/Vet_Data_canada.rds')
