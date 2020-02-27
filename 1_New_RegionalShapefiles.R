##Script Name: ENS Regional Diversity Plots
##Author: Izzy Hassall, Ecosystems Analysis
##Contact (if different from above):
##Date Created: 2020-02-27
##Date Modified:
##Licence:
##Abstract: New regional diversity plots with ENS instead of Shannon Diversity Index


##R version 3.6.1 (2019-07-05)
##Dependencies:
library(sf)
library(rgdal)
library(dplyr)
library(ggplot2)

#Read in shapefile
uk<-st_read("J:/GISprojects/Ecosystems Analysis/FPPH/THRS indicators/Data/NUTS/Download_10.01.2020/NUTS_Level_1_January_2018_Full_Clipped_Boundaries_in_the_United_Kingdom/NUTS_Level_1_January_2018_Full_Clipped_Boundaries_in_the_United_Kingdom.shp")

#Find class of file
class(uk)
#Find column with geometry data
attr(uk,"sf_column")
#Print first three features
print(uk[1:10],n=3)

#######################################################################
#SPECIES DIVERSITY
#Add species diversity column to data frame
uk$Species_ENS<-NA
#Species diversity data for regions obtained from FPPH_THRS_NFI_RegionsDiv_ALL.R script
##Regions
##Shannon / ENS
##NW England	2.513925 / 12.35332
##NE England	2.128601 / 8.403103
##Yorkshire and Humber	2.640980 / 14.02694
##E Midlands	2.436963 / 11.43825
##E England	2.490435 / 12.06652
##SE and London	2.462370 / 11.73258
##SW England	2.592493 / 13.36304
##W Midlands	2.609478 / 13.59195

#Fill Species_H column with values according to regions
uk<-uk%>%mutate(Species_ENS=case_when(nuts118nm=="North East (England)" ~ "8.403103",
                                    nuts118nm=="North West (England)" ~ "12.35332",
                                    nuts118nm=="Scotland" ~ "0",
                                    nuts118nm=="Northern Ireland" ~ "0",
                                    nuts118nm=="East Midlands (England)" ~ "11.43825",
                                    nuts118nm=="West Midlands (England)" ~ "13.59195",
                                    nuts118nm=="East of England" ~ "12.06652",
                                    nuts118nm=="South East (England)" ~ "11.73258",
                                    nuts118nm=="London" ~ "11.73258",
                                    nuts118nm=="South West (England)" ~ "13.36304",
                                    nuts118nm=="Wales" ~ "0",
                                    nuts118nm=="Yorkshire and The Humber" ~ "14.02694"))
uk<-uk%>%mutate(Species_ENS=na_if(Species_ENS,"0"))

#Print to check
print(uk[1:11],n=12)

#Set Species H as numeric
sapply(uk,class)
uk$Species_ENS<-as.numeric(uk$Species_ENS)
sapply(uk,class)

#Plot sf object using ggplot and scale fill according to Species H
ggplot(data=uk)+
  geom_sf(aes(fill=Species_ENS))+
  scale_fill_viridis_c(option="plasma")+
  theme_bw()

################################################################
#Add Age Class Diversity to shapefile and plot

#NW England	1.641346 / 5.162113
#NE England	1.481078 / 4.397684
#Yorkshire and Humber	1.474877 / 4.370498
#E Midlands	1.526605 / 4.602525
#E England	1.469524 / 4.347165
#SE and London	1.592720 / 4.917105
#SW England	1.675848 / 5.343324
#W Midlands	1.601985 / 4.962874

#Create Age_H column
uk$Age_ENS<-NA
#Fill Age_H column with values according to regions
uk<-uk%>%mutate(Age_ENS=case_when(nuts118nm=="North East (England)" ~ "4.397684",
                                nuts118nm=="North West (England)" ~ "5.162113",
                                nuts118nm=="Scotland" ~ "0",
                                nuts118nm=="Northern Ireland" ~ "0",
                                nuts118nm=="East Midlands (England)" ~ "4.602525",
                                nuts118nm=="West Midlands (England)" ~ "4.962874",
                                nuts118nm=="East of England" ~ "4.347165",
                                nuts118nm=="South East (England)" ~ "4.917105",
                                nuts118nm=="London" ~ "4.917105",
                                nuts118nm=="South West (England)" ~ "5.343324",
                                nuts118nm=="Wales" ~ "0",
                                nuts118nm=="Yorkshire and The Humber" ~ "4.370498"))
uk<-uk%>%mutate(Age_ENS=na_if(Age_ENS,"0"))

#Set Age H as numeric
sapply(uk,class)
uk$Age_ENS<-as.numeric(uk$Age_ENS)
sapply(uk,class)

#Print to check
print(uk[1:12],n=13)

#Plot sf object using ggplot and scale fill according to Age H
ggplot(data=uk)+
  geom_sf(aes(fill=Age_ENS))+
  scale_fill_viridis_c(option="plasma")+
  theme_bw()

###############################################################
##Add DBH diversity to shapefile 

#NW England 1.775272 / 5.901886
#NE England 1.644406 / 5.177933
#Yorkshire and Humber 1.572085 / 4.816681
#E Midlands 1.562921 / 4.772742
#E England 1.601189 / 4.958925
#SE and London 1.576065 / 4.835889
#SW England 1.578978 / 4.849997
#W Midlands 1.585522 / 4.881839

#Create DBH_H column
uk$DBH_ENS<-NA
#Fill Age_H column with values according to regions
uk<-uk%>%mutate(DBH_ENS=case_when(nuts118nm=="North East (England)" ~ "5.177933",
                                nuts118nm=="North West (England)" ~ "5.901886",
                                nuts118nm=="Scotland" ~ "0",
                                nuts118nm=="Northern Ireland" ~ "0",
                                nuts118nm=="East Midlands (England)" ~ "4.772742",
                                nuts118nm=="West Midlands (England)" ~ "4.881839",
                                nuts118nm=="East of England" ~ "4.958925",
                                nuts118nm=="South East (England)" ~ "4.835889",
                                nuts118nm=="London" ~ "4.835889",
                                nuts118nm=="South West (England)" ~ "4.849997",
                                nuts118nm=="Wales" ~ "0",
                                nuts118nm=="Yorkshire and The Humber" ~ "4.816681"))
uk<-uk%>%mutate(DBH_ENS=na_if(DBH_ENS,"0"))

#Set DBH H as numeric
sapply(uk,class)
uk$DBH_H<-as.numeric(uk$DBH_ENS)
sapply(uk,class)

#Print to check
print(uk[1:13],n=14)

#Plot sf object using ggplot and scale fill according to DBH H
ggplot(data=uk)+
  geom_sf(aes(fill=DBH_H))+
  scale_fill_viridis_c(option="plasma")+
  theme_bw()

###############################################################
#Retrieve current CRS and save as crs
st_crs(uk)
crs<-"+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs"

#Write out shapefile
getwd()
st_write(uk,"Species_Age_DBH_ENS.shp")

##IF NEEDED - Convert to spatial
uk_sp<-as(uk,"Spatial")
st_crs(uk_sp)