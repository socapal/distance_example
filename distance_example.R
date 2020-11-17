### Measuring minimal distance between the nearest polygon for 
### multipolygons in different coordinate reference systems.


# 1. Libraries -----------------------------------------------------------
setwd("C:/Users/socap/OneDrive/Documentos/R/Gasto Público en Áreas Naturales Protegidas/distance_example")
library(dplyr)
library(ggplot2)
library(gridExtra)
library(nngeo)

# ? should be accents!

# 2. Ciudad de México, SF, and errors. --------------------------------

ANPs=read_sf("SHAPE_ANPS/182ANP_Geo_ITRF08_Agosto_2020.shp") #National Parks SHP
DCdmx=st_read("df/DISTRITO.shp") #Mexico City's Districts SHP.

st_crs(Cdmx) # WGS 84 / UTM zone 14N
st_crs(ANPs) # MEXICO_ITRF_2008

## Transforming into 4326
ANPs_84=st_transform(ANPs, 4326)
ANPs_84$IND=1:182 #Indexing to quickly recognize each ANPs.

DCdmx_84=st_transform(DCdmx, 4326)

#Proximity Analysis Dataframe
PA=data.frame(INDEX=c(901:924))

# 2.1 Using Simple Features ------------------------------------------------
start = Sys.time() #We will measure computational time.

#Closest polygon
Near_ANP=st_nearest_feature(DCdmx_84, ANPs_84)
PA$NEAR_ANP=Near_ANP

S_NEAR_ANP=ANPs_84%>%slice(Near_ANP) #SLICE BASED ON NEAREST FEATURE
PA$N_NEAR_ANP=S_NEAR_ANP$NOMBRE

#Distance to closest polygon
min_dist=st_distance(DCdmx_84, S_NEAR_ANP, by_element=TRUE)
PA$min_dist=min_dist
min_dist

end = Sys.time()
end - start    #19.554 mins (24x182 for CLOSEST features)
  
# 2.2 Using rgeos ---------------------------------------------------------
  library(rgeos)
  
  ANPs_84p=as(ANPs_84,"Spatial")
  DCdmx_84p=as(DCdmx_84,"Spatial")
  
  start=Sys.time()
  distance=apply(gDistance(DCdmx_84p, ANPs_84p, byid=TRUE), 2, min)
  end=Sys.time()
  end-start           #22.56 minutes for 24x182 for ALL features.

# gDistance by default returns the cartesian minimum distance between the two geometries
##in the units of the current projection
  
ggplot()+geom_sf(data=DCdmx_84, aes(fill=factor(distrito)))+
        geom_sf(data=ANPs_cdmx, fill="green")+
        coord_sf()
distance




# 3. Aguascalientes ("DAgs") ----------------------------------------------
# 3.1 Using Simple Features -----------------------------------------------
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
DAgs$IND=101:103  #INDEX

## Proximity Analysis
PA=data.frame(INDEX=c(101,102,103))

#This code takes forever and doesn't give back minimal distance,
##but it could be extracted through row min functions. It still takes 3+ hours for
## a 3x182 feature. 

# dist=st_distance(DAgs_84, ANPs_84)


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
end - start # Time difference of 7.182168 mins ( 3x182 for CLOSEST features)


# 3.1.2 Visualizations ----------------------------------------------------


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



# 3.2 Using nngeo ---------------------------------------------------------

library(nngeo)
start = Sys.time()
result = st_nn(DAgs_84, ANPs_84, k = 1, parallel = 5, returnDist = TRUE)
end = Sys.time()
end - start #Terminated command at 24.83 min with no results yet. 

# 3.3 Using rgeos ---------------------------------------------------------

library(rgeos)
ANPs_84p=as(ANPs_84,"Spatial")
DAgs_84p=as(DAgs_84,"Spatial")

start=Sys.time()
distance=apply(gDistance(DAgs_84p, ANPs_84p, byid=TRUE), 2, min)
end=Sys.time()
end-start           #9.377 minutes for 3x182 for ALL features.

system('CMD /C "ECHO The R process has finished running && PAUSE"',   
       invisible=FALSE, wait=FALSE)

