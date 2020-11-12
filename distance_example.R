### Measuring minimal distance between the nearest polygon for 
### multipolygons in different coordinate reference systems.

setwd("C:/Users/socap/OneDrive/Documentos/R/Gasto Público en Áreas Naturales Protegidas/distance_example")
library(sf)

#We bring the mulipolygon file for Aguascalientes federal districts.
DAgs=st_read("DISTRITO.shp")
ANPs=read_sf("182ANP_Geo_ITRF08_Agosto_2020.shp")
