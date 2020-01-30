##Script Name: LiDAR Data 
##Author: Izzy Hassall, Ecosystems Analysis
##Contact (if different from above):
##Date Created: 2020-01-30
##Date Modified: 
##Licence:
##Abstract: Show the capability of LiDAR data in constructing canopy structural diversity metrics using canopy height models


##R version 3.6.1 (2019-07-05)
##Dependencies:
library(readxl)
library(ggplot2)
library(dplyr)
library(reshape2)

###############################################
##LiDAR DATA FOR SD38NW_3085 
##Obtained from DEFRA - https://environment.data.gov.uk/DefraDataDownload/?Mode=survey

#Created profiles in QGIS 
#Profile 1
#Load in profile excel sheet 
prof1<-read_excel("C:/Users/Izzy.Hassall/Documents/Tree Health Resilience/Lidar/SD38NW_SD3085_Profile1.xlsx")
#Plot line graph as profile
ggplot(data=prof1,aes(x=DIST,y=Z))+
  geom_line()+
  xlab("Horizontal Distance (m)")+
  ylab("Canopy Height (m)")+
  scale_x_continuous(limits=c(0,450),expand=c(0,0))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

#Profile 2
prof2<-read_excel("C:/Users/Izzy.Hassall/Documents/Tree Health Resilience/Lidar/SD38NW_SD3085_Profile2.xlsx")
#Plot line graph as profile
ggplot(data=prof2,aes(x=DIST,y=Z))+
  geom_line()+
  xlab("Horizontal Distance (m)")+
  ylab("Canopy Height (m)")+
  scale_x_continuous(limits=c(0,325),expand=c(0,0))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

#Create mock profile 2 data to show difference over time
prof2<-prof2%>%select(DIST,Z)
prof2_mock<-prof2_mock%>%rename(Distance=DIST,Height2020=Z)      
prof2_mock$Height2030<-c(10.45,6.78,2.89,10.77,5.78,5.67,0.005,2.56,12.45,5.89,9.75,7.56,14.75,8.23,13.9,4.45,13.62,14.77,7.34,10.66,3.89,1.56,9.03,8.45,3.16,4.76,0.67,0.027,2.01,6.67,4.45,0.01,5.31,1.45,3.42,2.09,9.45,5.67,9.32,0.012,0.03,3.67,7.83,8.34,6.7,1.49,2.82,8.35,11.71,0.12,8.34,9.56,0.13,4.34,14.3,3.01,4.56,6.23,1.67,4.31,3.32,11.56,5.34,12,10.63,3.24,8.34,0.001,0.02,8.34,6.2,0.001,9.45,11.98,4.56,3.23,12.06,18.001,3.14,2.03,8.91,7.45,8.23,6.97,4.05,13.14,9.31,0.1,7.34,5.34,6.78,12.45,12.89,5.31,10.67,2.18,0.129,0.24,1.65,2.98,3.23,1.45,9.341,3.56,5.67,1.678,0.45,8.25,0.001,6.721,5.834,5.02,4.21,5.43,11.09,11.56,6.24,10.005,4.525,11.78,8.024,3.267,7.095,7.51,4.234,6.985,2.784,8.451,4.81,4.231,0.034,1.54,0.23,3.54,4.72,10.87,6.34,2.875,4.67,0.41,0.56,0,0.897,4.576,9.234,8.234,6.13,4.35,2.56,3.112,1.45,0.6,1.75,2.0541,4.556,9.034,4.07)       
prof2_melt<-melt(prof2_mock,id="Distance")
prof2_melt<-prof2_melt%>%rename(Year=variable,Height=value)
ggplot(prof2_melt,aes(x=Distance,y=Height,colour=Year))+
  geom_line()+
  xlab("Horizontal Distance (m)")+
  ylab("Canopy Height (m)")+
  scale_x_continuous(limits=c(0,100),expand=c(0,0))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

#####################################################################
##Compare zonal statistics for areas of woodland
##Using sample tile SD38ne
##Zonal statistics obtained using the Canopy Height Model layer and NFI 2018 shapefile

#Read in excel file saved from the shapefile
chm<-read_excel("J:/GISprojects/Ecosystems Analysis/FPPH/THRS indicators/Data/EA LiDAR_NWArea/CHM/NFI_Clipped_SD38ne_CHMExtent.xlsx")
#Remove non woodland subregions by selecting only woodland rows
chm<-chm%>%filter(CATEGORY%in%c("Woodland"))
ggplot(chm,aes(x=))