### Measuring minimal distance between the nearest polygon for 
### multipolygons in different coordinate reference systems.


# 1. Libraries -----------------------------------------------------------
setwd("C:/Users/socap/OneDrive/Documentos/R/Gasto Público en Áreas Naturales Protegidas/distance_example")
library(dplyr)
library(ggplot2)
library(gridExtra)
library(nngeo)


# 2. Aguascalientes ("DAgs") ----------------------------------------------
# 2.1 Using Simple Features -----------------------------------------------
library(sf)

#We bring the mulipolygon file for Aguascalientes federal districts and National Protected Areas (ANPs).
DAgs=st_read("ags/DISTRITO.shp")
ANPs=read_sf("SHAPE_ANPS/182ANP_Geo_ITRF08_Agosto_2020.shp")
st_crs(DAgs) # WGS 84 / UTM zone 13N
st_crs(ANPs) # MEXICO_ITRF_2008

## Transforming into 4326
ANPs_84=st_transform(ANPs, 4326)
ANPs_84$IND=1:182 #INDEX

DAgs_84=st_transform(DAgs, 4326)
DAgs$IND=101:103  #FED INDEX

## Proximity Analysis
PA=data.frame(INDEX=c(101,102,103))


#Closest polygon
start = Sys.time()
NEAR_ANP=st_nearest_feature(st_cast(DAgs_84), st_cast(ANPs_84))
PA$NEAR_ANP=NEAR_ANP

S_NEAR_ANP=ANPs_84%>%slice(NEAR_ANP) #SLICE BASED ON NEAREST FEATURE
PA$N_NEAR_ANP=S_NEAR_ANP$NOMBRE

#Distance to closest polygon
min_dist=st_distance(DAgs_84, S_NEAR_ANP, by_element=TRUE)
PA$min_dist=min_dist

end = Sys.time()
end - start # Time difference of 7.182168 mins (for 3x182 features)


# 2.2.1 Visualizations ----------------------------------------------------


##Visualization
###Original crs.
Vis_O1=ggplot()+geom_sf(data=DAgs, fill="white")+
         geom_sf(data=ANPs%>%slice(NEAR_ANP), fill="green")+
         coord_sf()
###Modified crs - WGS_84.
Vis_841=ggplot()+geom_sf(data=DAgs_84, fill="white")+
  geom_sf(data=ANPs_84%>%slice(NEAR_ANP), fill="green")+
  coord_sf()

##Comparison
grid.arrange(Vis_O1, Vis_841, ncol=2) #No visible distortion.

###NNGEO
library(nngeo)
start = Sys.time()
result = st_nn(DAgs_84, ANPs_84, k = 1, parallel = 5, returnDist = TRUE)
end = Sys.time()
end - start #Terminated command at 24.83 min,

# 3. Ciudad de México, SF, and errors. --------------------------------

##Larger Shapefile
DCdmx=st_read("df/DISTRITO.shp")
DCdmx_84=st_transform(DCdmx, 4326)

PB=data.frame(INDEX=c(901:924))

#Closest polygon
start = Sys.time()
Near_ANP=st_nearest_feature(st_cast(DCdmx_84), st_cast(ANPs_84))
PB$NEAR_ANP=NEAR_ANP

S_NEAR_ANP=ANPs_84%>%slice(NEAR_ANP) #SLICE BASED ON NEAREST FEATURE
PB$N_NEAR_ANP=S_NEAR_ANP$NOMBRE

#Distance to closest polygon
min_dist=st_distance(DCdmx_84, S_NEAR_ANP, by_element=TRUE)
PB$min_dist=min_dist

end = Sys.time()
end - start #19.554 mins (24x182 features)

# 3.1 Visualization and Error in st_near() --------------------------------

##Visualization
###Original crs.
Vis_O2=ggplot()+geom_sf(data=DCdmx, fill="white")+
  geom_sf(data=ANPs%>%slice(NEAR_ANP), fill="green")+
  coord_sf()
###Modified crs - WGS_84.
Vis_842=ggplot()+geom_sf(data=DCdmx_84, fill="white")+
  geom_sf(data=ANPs_84%>%slice(NEAR_ANP), fill="green")+
  coord_sf()

##Comparison
grid.arrange(Vis_O2, Vis_842, ncol=2) #No visible distortion, but polygons are
                                      #not the nearest. See below:

print(ANPs_84$ESTADOS) #We search for "Ciudad de México"
Vis_843=ggplot()+geom_sf(data=DCdmx_84, fill="white")+
  geom_sf(data=subset(ANPs_84, ANPs_84$ESTADOS== "Ciudad de México"), fill="green")+
  coord_sf()
Vis_843

ANPs_cdmx=subset(ANPs_84, ANPs_84$NOMBRE=="Ciudad de México") #nearest ANP to cdmx.

Vis_844= ggplot() +
  geom_sf(data=st_cast(DCdmx_84, fill="white"))+
  geom_sf(data=st_cast(ANPs_s), fill="green")+
  coord_sf()
Vis_844

grid.arrange(Vis_843, Vis_844, ncol=2) #st_cast() removes "Ciudad de México" polygons.


