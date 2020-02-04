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
prof1<-read_excel("C:/Users/Izzy.Hassall/Documents/Tree Health Resilience/Lidar/SD38NW_SD3085_Practice/SD38NW_SD3085_Profile1.xlsx")
#Plot line graph as profile
ggplot(data=prof1,aes(x=DIST,y=Z))+
  geom_line()+
  xlab("Horizontal Distance (m)")+
  ylab("Canopy Height (m)")+
  scale_x_continuous(limits=c(0,450),expand=c(0,0))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

#Profile 2
prof2<-read_excel("C:/Users/Izzy.Hassall/Documents/Tree Health Resilience/Lidar/SD38NW_SD3085_Practice/SD38NW_SD3085_Profile2.xlsx")
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
prof2_mock<-prof2%>%rename(Distance=DIST,Height2020=Z)      
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

#First square SD38ne
#Read in excel file saved from the shapefile
chm_38ne<-read_excel("J:/GISprojects/Ecosystems Analysis/FPPH/THRS indicators/Data/EA LiDAR_NWArea/CHM/NFI_Clipped_SD38ne_CHMExtent.xlsx")
#Remove non woodland subregions by selecting only woodland rows
chm_38ne<-chm_38ne%>%filter(CATEGORY%in%c("Woodland"))

#Create subset of just the broadleaves and conifers stands
chm_38ne_subs<-chm_38ne%>%filter(IFT_IOA%in%c("Broadleaved","Conifer"))
colnames(chm_38ne_subs)
colnames(chm_38ne_subs)<-sub("_","",colnames(chm_38ne_subs))
#New column with object number
chm_38ne_subs$Object<-1:nrow(chm_38ne_subs)
ggplot(chm_38ne_subs,aes(x=Object,y=mean))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mean-stdev,ymax=mean+stdev),width=0.2)+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

#Plot line with error ribbon instead of bar chart
ggplot(chm_38ne_subs,aes(x=Object,y=mean))+
  geom_ribbon(aes(ymin=mean-stdev,ymax=mean+stdev),alpha=0.2)+
  geom_line()+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Forest Compartment")+
  ylab("Mean Height (m)")

#Table detailing mean and SD of canopy height
colnames(chm_38ne_subs)
chm_38ne<-chm_38ne_subs%>%select(IFTIOA,mean,median,stdev,max)

#Generate mean and SD of the square
mean(chm_38ne$mean)
sd(chm_38ne$mean)
#Generate coefficient of variation
cov<-(sd(chm_38ne$mean)/mean(chm_38ne$mean))*100
cov

################################################################
#Create mock data of mean and sd over time
years<-c(seq(2020,2050,1))
mock_38ne<-matrix(c(2.371,2.403,2.407,2.435,2.652,2.782,2.783,2.784,2.895,2.987,2.896,3.002,3.233,3.247,3.287,3.298,	3.312,3.314,3.354,3.445,3.502,3.525,3.562,3.451,3.451,3.489,3.561,3.572,3.689,3.672,3.680,
                    2.732,2.742,2.634,2.623,2.312,2.132,1.967,2.165,1.897,1.975,1.834,1.623,1.875,1.976,2.045,2.001,2.014,2.251,2.34,2.462,2.354,2.132,2.563,2.679,2.451,2.562,2.586,2.566,2.671,2.581,2.643),
                    nrow=31,
                    dimnames=list(c(years),
                                  c("Mean","SD")))
#Add year as column
mock_38ne<-as.data.frame(mock_38ne)
mock_38ne$Year<-years
#Calculate Coefficient of Variation
sapply(mock_38ne,class)
mock_38ne$cov<-(mock_38ne$Mean/mock_38ne$SD)*100

#Plot this over time as line with shaded SD region - mock data
ggplot(data=mock_38ne,aes(x=Year,y=Mean))+
  geom_ribbon(aes(ymin=Mean-SD,ymax=Mean+SD),alpha=0.2)+
  geom_line()+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

#Edit mock data to show more variation in SD
mock_38ne_2<-matrix(c(2.371,2.403,2.407,2.435,2.652,2.782,2.783,2.784,2.895,2.987,2.896,3.002,3.233,3.247,3.287,3.298,	3.312,3.314,3.354,3.445,3.502,3.525,3.562,3.451,3.451,3.489,3.561,3.572,3.689,3.672,3.680,
                    2.732,2.720,2.611,2.561,2.312,2.132,1.967,1.823,1.897,1.652,1.512,1.324,1.234,1.432,1.678,1.783,1.892,2.001,2.34,2.462,2.354,2.132,2.563,2.679,2.451,2.562,2.586,2.566,2.671,2.581,2.643),
                  nrow=31,
                  dimnames=list(c(years),
                                c("Mean","SD")))
#Add year as column
mock_38ne_2<-as.data.frame(mock_38ne_2)
mock_38ne_2$Year<-years
#Calculate Coefficient of Variation
sapply(mock_38ne_2,class)
mock_38ne_2$cov<-(mock_38ne_2$Mean/mock_38ne_2$SD)*100
#Plot this edited data over time as line with shaded SD region - mock data
ggplot(data=mock_38ne_2,aes(x=Year,y=Mean))+
  geom_ribbon(aes(ymin=Mean-SD,ymax=Mean+SD),alpha=0.2)+
  geom_line()+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

#########################################################################
#Plot the mean with coefficient of variation shaded ribbon
ggplot(data=mock_38ne,aes(x=Year,y=Mean))+
  geom_ribbon(aes(ymin=Mean-cov,ymax=Mean+cov),alpha=0.2)+
  geom_line()+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())
##Too much variation to visualise like this##

#########################################################################
#Second square SD39se 
chm_39se<-read_excel("J:/GISprojects/Ecosystems Analysis/FPPH/THRS indicators/Data/EA LiDAR_NWArea/CHM/NFI_Clipped_SD39se_CHMExtent.xlsx")
#Remove non woodland subregions by selecting only woodland rows
chm_39se<-chm_39se%>%filter(CATEGORY%in%c("Woodland"))

#Create subset removing Felled and Windblow areas
chm_39se_subs<-chm_39se%>%filter(IFT_IOA%in%c("Broadleaved","Conifer","Mixed mainly broadleaved","Mixed mainly conifer","Young trees"))
colnames(chm_39se_subs)
colnames(chm_39se_subs)<-sub("_","",colnames(chm_39se_subs))

#New column with object number
chm_39se_subs$Object<-1:nrow(chm_39se_subs)
ggplot(chm_39se_subs,aes(x=Object,y=mean))+
  geom_bar(stat="identity")+
  geom_errorbar(aes(ymin=mean-stdev,ymax=mean+stdev),width=0.2)+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())

#Plot line with error ribbon instead of bar chart
ggplot(chm_39se_subs,aes(x=Object,y=mean))+
  geom_ribbon(aes(ymin=mean-stdev,ymax=mean+stdev),alpha=0.2)+
  geom_line()+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("Forest Compartment")+
  ylab("Mean Height (m)")
  
#Table detailing mean and SD of canopy height
colnames(chm_39se_subs)
chm_39se_subs<-chm_39se_subs%>%select(IFTIOA,mean,median,stdev,max)

#Generate mean and SD of the entire square
mean(chm_39se_subs$mean)
sd(chm_39se_subs$mean)
#Generate coefficient of variation
cov<-(sd(chm_39se_subs$mean)/mean(chm_39se_subs$mean))*100
cov

############################################################################
#Generate mock data showing regional canopy height MEAN, SD, COV plotted as bar chart
#Could plot on map??
ch_reg<-matrix(c(3.363,2.146,4.521,2.461,3.784,3.489,2.576,3.169,
                 2.934,1.811,2.121,1.343,1.455,1.034,1.992,2.156,
                 114.622,92.860,213.154,183.246,154.134,171.534,129.317,146.985),
               nrow=8,
               dimnames=list(c("NW England","NE England","Yorkshire and Humber","E Midlands","E England","SE and London","SW England","W Midlands"),
                             c("Mean","SD","CoV")))
regions<-c("NW England","NE England","Yorkshire and Humber","E Midlands","E England","SE and London","SW England","W Midlands")
ch_reg<-as.data.frame(ch_reg)
ch_reg$Region<-c(regions)
sapply(ch_reg,class)
ch_reg$Region<-as.factor(ch_reg$Region)
sapply(ch_reg,class)
ch_reg$Region<-factor(ch_reg$Region,levels=c("NW England","NE England","Yorkshire and Humber","E Midlands","E England","SE and London","SW England","W Midlands"))

#Plot as barplot
ggplot(ch_reg,aes(x=Region,y=Mean))+
  geom_bar(stat="identity",fill="grey")+
  geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),width=0.2)+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  ylab("Mean Height (m)")+
  xlab("")+
  theme(axis.text.x=element_text(angle=45,hjust=1))

#Trying to change plot parameters
par()
opar<-par()
#$pin
#[1] 7.791250 3.868333
par(pin=c(5,4))
par(mai=c(2,2,2,2))
