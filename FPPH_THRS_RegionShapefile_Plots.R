##First attempt at shading regions according to diversity


#Load packages
require(rgdal)
require(ggplot2)
require(dplyr)
require(sf)

#Read in NUTS level 1 shapefile
shp<-readOGR("J:/GISprojects/Ecosystems Analysis/FPPH/THRS indicators/Data/NUTS/Download 10.01.2020/NUTS_Level_1_January_2018_Full_Clipped_Boundaries_in_the_United_Kingdom/NUTS_Level_1_January_2018_Full_Clipped_Boundaries_in_the_United_Kingdom.shp")
summary(shp@data)

#Draw map without attributes
map<-ggplot()+geom_polygon(data=shp,aes(x=long,y=lat,group=group),colour="black",fill=NA)
map+theme_void()

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

#Shade regions according to region name in "nuts118nm"
map<-ggplot()+geom_polygon(data=df_join,aes(x=long.x,y=lat.x,group=group,fill=nuts118nm),colour="black")+theme_void()
map

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
#Shade regions according to Species H 
map<-ggplot()+geom_polygon(data=df_join,aes(x=long.x,y=lat.x,group=group,fill=Species_H),colour="black")+theme_void()
map



