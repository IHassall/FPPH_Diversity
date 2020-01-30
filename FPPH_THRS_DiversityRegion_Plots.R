library(sf)
library(rgdal)
library(dplyr)
library(ggplot2)
#Read in shapefile
uk<-st_read("J:/GISprojects/Ecosystems Analysis/FPPH/THRS indicators/Data/NUTS/Download 10.01.2020/NUTS_Level_1_January_2018_Full_Clipped_Boundaries_in_the_United_Kingdom/NUTS_Level_1_January_2018_Full_Clipped_Boundaries_in_the_United_Kingdom.shp")

#Find class of file
class(uk)
#Find column with geometry data
attr(uk,"sf_column")
#Print first three features
print(uk[1:10],n=3)

#Add species diversity column to data frame
uk$Species_H<-NA
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
uk<-uk%>%mutate(Species_H=case_when(nuts118nm=="North East (England)" ~ "2.128601",
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
uk<-uk%>%mutate(Species_H=na_if(Species_H,"0"))

#Print first 3 to check
print(uk[1:11],n=12)

#Set Species H as numeric
sapply(uk,class)
uk$Species_H<-as.numeric(uk$Species_H)
sapply(uk,class)

#Plot sf object using ggplot and scale fill according to Species H
ggplot(data=uk)+
  geom_sf(aes(fill=Species_H))+
  scale_fill_viridis_c(option="plasma")+
  theme_bw()

#Retrieve current CRS and save as crs
st_crs(uk)
crs<-"+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs"

#Write out shapefile
st_write(uk,"Species_H.shp")

##IF NEEDED - Convert to spatial
uk_sp<-as(uk,"Spatial")
st_crs(uk_sp)

