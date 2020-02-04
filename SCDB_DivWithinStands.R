##Script Name:
##Author: Izzy Hassall, Ecosystems Analysis
##Contact (if different from above):
##Date Created: 2020-02-04
##Date Modified:
##Licence:
##Abstract:


##R version 3.6.1 (2019-07-05)
##Dependencies:
library(sf)
library(rgdal)
library(dplyr)
library(ggplot2)
library(reshape2)

scdb<-st_read("J:/GISprojects/Ecosystems Analysis/FPPH/THRS indicators/Data/SCDB/National_Forest_Estate_Subcompartments_England_2019/National_Forest_Estate_Subcompartments_England_2019.shp")
reg<-st_read("J:/GISprojects/Ecosystems Analysis/FPPH/THRS indicators/Data/NUTS/Download_10.01.2020/NUTS_Level_1_January_2018_Full_Clipped_Boundaries_in_the_United_Kingdom/NUTS_Level_1_January_2018_Full_Clipped_Boundaries_in_the_United_Kingdom.shp")
class(scdb)
class(reg)
attr(scdb,"sf_column")
attr(reg,"sf_column")
#Print first three features
print(scdb[1:40],n=3)
print(reg[1:10],n=3)

##############################################################
#Use BLOCK attribute to generate diversity per forest block/stand
#Create smaller dataset with fewer variables
colnames(scdb)
scdb_2<-scdb%>%select("BLOCK","PRISPECIES","Shape__Are")
sapply(scdb_2,class)
class(scdb_2)
#Remove geometry data so just df
st_geometry(scdb_2)<-NULL
class(scdb_2)
sapply(scdb_2,class)
scdb_2$BLOCK<-as.factor(scdb_2$BLOCK)
melt<-melt(scdb_2,id=c("BLOCK","PRISPECIES"))
new<-dcast(melt,BLOCK+PRISPECIES~variable,sum)


#scdb_agg<-aggregate(scdb_2,by=list(scdb_2$BLOCK),FUN=sum)
#Aggregate did not work as species is factor...Maybe not right for this time so used melt() instead
##############################################################
##Divide the SCDB dataset into regions using the NUTS dataset
#Create spatial subset of scdb compartments within each NUTS region
nw<-reg%>%filter(nuts118nm=="North West (England)")
sc_nw<-scdb[nw, ,op=st_intersects]
#Check by plotting region
ggplot(data=sc_nw)+
  geom_sf()

#Subset for all other NUTS region
ne<-reg%>%filter(nuts118nm=="North East (England)")
sc_ne<-scdb[ne, ,op=st_intersects]

yh<-reg%>%filter(nuts118nm=="Yorkshire and The Humber")
sc_yh<-scdb[yh, ,op=st_intersects]

em<-reg%>%filter(nuts118nm=="East Midlands (England)")
sc_em<-scdb[em, ,op=st_intersects]

ee<-reg%>%filter(nuts118nm=="East of England")
sc_ee<-scdb[ee, ,op=st_intersects]

sel<-reg%>%filter(nuts118nm%in%c("South East (England)","London"))
sc_sel<-scdb[sel, ,op=st_intersects]

sw<-reg%>%filter(nuts118nm=="South West (England)")
sc_sw<-scdb[sw, ,op=st_intersects]

wm<-reg%>%filter(nuts118nm=="West Midlands (England)")
sc_wm<-scdb[wm, ,op=st_intersects]
