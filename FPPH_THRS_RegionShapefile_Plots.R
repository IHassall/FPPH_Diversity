##First attempt at shading regions according to diversity


#Load packages
require(rgdal)
require(ggplot2)
require(dplyr)
require(sf)
library(sp)

#Read in NUTS level 1 shapefile
shp<-readOGR("J:/GISprojects/Ecosystems Analysis/FPPH/THRS indicators/Data/NUTS/Download 10.01.2020/NUTS_Level_1_January_2018_Full_Clipped_Boundaries_in_the_United_Kingdom/NUTS_Level_1_January_2018_Full_Clipped_Boundaries_in_the_United_Kingdom.shp")
summary(shp@data)

#Draw map without attributes
map<-ggplot()+geom_polygon(data=shp,aes(x=long,y=lat,group=group),colour="black",fill=NA)
map+theme_void()

################################################################################
##SPECIES DIVERSITY

#Shade regions 
#Need to generate new unique attribute (eg using rownames())
#Use data in shapefile and add row names as unique attribute
shp@data<-shp@data%>%mutate(id=row.names(.))
summary(shp@data)
#shp@data contains Region names (nuts118nm) and lat long data plus id column (row names)

#Create data frame summarising stats
shp_df<-broom::tidy(shp,region="id")
#shp_df contains long, lat and group information as well as id column

#Join the data frame and shapefile data file together by matching id 
df_join<-shp_df%>%left_join(shp@data,by=c("id"="id"))

#Add species diversity column to df_join data frame
df_join$Species_H<-NA
#Species diversity data for regions obtained from FPPH_THRS_NFI_RegionsDiv_ALL.R script
##Regions
##Shannon
##NW England	2.513925
##NE England	2.128601
##Yorkshire and Humber	2.640980
##E Midlands	2.436963
##E England	2.490435
##SE and London	2.462370
##SW England	2.592493
##W Midlands	2.609478

#Fill Species_H column with values according to regions
df_join<-df_join%>%mutate(Species_H=case_when(nuts118nm=="North East (England)" ~ "2.128601",
                                              nuts118nm=="North West (England)" ~ "2.513925",
                                              nuts118nm=="Scotland" ~ "0",
                                              nuts118nm=="Northern Ireland" ~ "0",
                                              nuts118nm=="East Midlands (England)" ~ "2.436963",
                                              nuts118nm=="West Midlands (England)" ~ "2.609478",
                                              nuts118nm=="East of England" ~ "2.490435",
                                              nuts118nm=="South East (England)" ~ "2.462370",
                                              nuts118nm=="London" ~ "2.462370",
                                              nuts118nm=="South West (England)" ~ "2.592493",
                                              nuts118nm=="Wales" ~ "0",
                                              nuts118nm=="Yorkshire and The Humber" ~ "2.640980"))
#Set Species H as numeric
sapply(df_join,class)
df_join$Species_H<-as.numeric(df_join$Species_H)
sapply(df_join,class)
#Set Scotland, Wales and N Ireland as NA
df_join[,17][df_join[,17]==0]<-NA
sapply(df_join,class)
#Shade regions according to Species H 
map<-ggplot()+geom_polygon(data=df_join,aes(x=long.x,y=lat.x,group=group,fill=Species_H),colour="black")+
  theme_void()
map

########################################################################
##AGE DIVERSITY

#NW England	1.641346
#NE England	1.481078
#Yorkshire and Humber	1.474877
#E Midlands	1.526605
#E England	1.469524
#SE and London	1.592720
#SW England	1.675848
#W Midlands	1.601985

#Create Age_H column
df_join$Age_H<-NA
#Fill Age_H column with values according to regions
df_join<-df_join%>%mutate(Age_H=case_when(nuts118nm=="North East (England)" ~ "1.481078",
                                              nuts118nm=="North West (England)" ~ "1.641346",
                                              nuts118nm=="Scotland" ~ "0",
                                              nuts118nm=="Northern Ireland" ~ "0",
                                              nuts118nm=="East Midlands (England)" ~ "1.526605",
                                              nuts118nm=="West Midlands (England)" ~ "1.601985",
                                              nuts118nm=="East of England" ~ "1.469524",
                                              nuts118nm=="South East (England)" ~ "1.592720",
                                              nuts118nm=="London" ~ "1.592720",
                                              nuts118nm=="South West (England)" ~ "1.675848",
                                              nuts118nm=="Wales" ~ "0",
                                              nuts118nm=="Yorkshire and The Humber" ~ "1.474877"))
#Set Age H as numeric
sapply(df_join,class)
df_join$Age_H<-as.numeric(df_join$Age_H)
sapply(df_join,class)
#Set Scotland, Wales and N Ireland as NA
df_join[,18][df_join[,18]==0]<-NA
#Shade regions according to Age H 
map<-ggplot()+geom_polygon(data=df_join,aes(x=long.x,y=lat.x,group=group,fill=Age_H),colour="black")+
  theme_void()
map


######################################################################
##SIZE DIVERSITY

#NW England	1.775272
#NE England	1.644406
#Yorkshire and Humber	1.572085
#E Midlands	1.562921
#E England	1.601189
#SE and London	1.576065
#SW England	1.578978
#W Midlands	1.585522

#Create Size_H column
df_join$Size_H<-NA
#Fill Size_H column with values according to regions
df_join<-df_join%>%mutate(Size_H=case_when(nuts118nm=="North East (England)" ~ "1.644406",
                                          nuts118nm=="North West (England)" ~ "1.775272",
                                          nuts118nm=="Scotland" ~ "0",
                                          nuts118nm=="Northern Ireland" ~ "0",
                                          nuts118nm=="East Midlands (England)" ~ "1.562921",
                                          nuts118nm=="West Midlands (England)" ~ "1.585522",
                                          nuts118nm=="East of England" ~ "1.601189",
                                          nuts118nm=="South East (England)" ~ "1.576065",
                                          nuts118nm=="London" ~ "1.576065",
                                          nuts118nm=="South West (England)" ~ "1.578978",
                                          nuts118nm=="Wales" ~ "0",
                                          nuts118nm=="Yorkshire and The Humber" ~ "1.572085"))
#Set Size H as numeric
sapply(df_join,class)
df_join$Size_H<-as.numeric(df_join$Size_H)
sapply(df_join,class)
#Set Scotland, Wales and N Ireland as NA
df_join[,19][df_join[,19]==0]<-NA
#Shade regions according to Size H 
map<-ggplot()+geom_polygon(data=df_join,aes(x=long.x,y=lat.x,group=group,fill=Size_H),colour="black")+
  theme_void()
map

##############################################################################

#Can do just do the editing on the polygons??? Edit polygon attribute table??
shp_df<-as.data.frame(shp@data)
shp_df$Species_H<-NA

##Write out shapefile
df_join_sf<-st_as_sf(df_join,coords=c("long.y","lat.y"),crs=4326)
df_join_sf
df_join_sp<-as(df_join_sf,"Spatial")
class(df_join_sp)
df_join_sp
st_write(df_join_sf,"div_shapefile.shp",driver="ESRI Shapefile")
##This gives point geometry and does not show up in QGIS
