##Script Name: Woodland regeneration and new planting
##Author: Izzy Hassall, Ecosystems Analysis
##Contact (if different from above):
##Date Created: 2020-01-20
##Date Modified:
##Licence:
##Abstract: Using NFI Forestry Statistics data to assess the restocking and new planting of woodland areas over time


##R version 3.6.1 (2019-07-05)
##Dependencies:
library(readxl)
library(reshape)
library(ggplot2)

###############################################################################
##Data Used: Ch1_Woodland_FS2019 

##REGENERATION/RESTOCKING

#Looking at restocking (planting in felled areas) for conifers/broadleaves
#Read in data
restock<-read_excel("J:/GISprojects/Ecosystems Analysis/FPPH/THRS indicators/Data/National Forest Inventory England/Inventory reports/Ch1_Woodland_FS2019.xlsx",sheet="Table 1.13",range="F6:H12")
restock<-restock[-1,]
#Add column
restock$Year<-c("2014-2015","2015-2016","2016-2017","2017-2018","2018-2019")
sapply(restock,class)
restock$Year<-as.factor(restock$Year)
#Ensure set as data frame for use in melt function
restock<-as.data.frame(restock)
#Restructure using melt()
restock<-melt(restock)
#Change column names
names(restock)[names(restock)=="value"]<-"Area"
names(restock)[names(restock)=="variable"]<-"Woodland"

#Plot with bars for each woodland type
ggplot(data=restock,aes(x=Year,y=Area,fill=Woodland))+
        labs(y="Area (thousand ha)")+
        geom_bar(stat="identity",position="dodge")+
        theme_bw()+
        theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
        theme(axis.text.x=element_text(angle=60,hjust=1,colour="black"))+
        theme(axis.title.x=element_blank())+
        theme(axis.text.y=element_text(colour="black"))  

##Restocking over longer timescale for all trees
restock_all<-read_excel("J:/GISprojects/Ecosystems Analysis/FPPH/THRS indicators/Data/National Forest Inventory England/Inventory reports/Ch1_Woodland_FS2019.xlsx",sheet="data for figure 1.7",range="A5:B49")
colnames(restock_all)
names(restock_all)[names(restock_all)=="Year                             (ending 31/3)"]<-"Year"
names(restock_all)[names(restock_all)=="England"]<-"Area"
sapply(restock_all,class)
ggplot(data=restock_all,aes(x=Year,y=Area,group=1))+
  geom_path()+
  geom_point()+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=60,hjust=1,colour="black"))+
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(colour="black"))+
  labs(x="",y="Area (thousand ha)")+
  scale_x_continuous(breaks=c(1976,1980,1984,1988,1992,1996,2000,2004,2008,2012,2016,2020))

#######################################################################
##NEW PLANTING
#Read in data
plant<-read_excel("J:/GISprojects/Ecosystems Analysis/FPPH/THRS indicators/Data/National Forest Inventory England/Inventory reports/Ch1_Woodland_FS2019.xlsx",sheet="Table 1.13",range="B6:E12")
plant<-plant[-1,]       
names(plant)[names(plant)=="...1"]<-"Year"
#Ensure set as data frame for use in melt() function
plant<-as.data.frame(plant)
#Restructure
plant<-melt(plant)
#Change column names
names(plant)[names(plant)=="value"]<-"Area"
names(plant)[names(plant)=="variable"]<-"Woodland"
sapply(plant,class)

#Plot with bars for each woodland type
ggplot(data=plant,aes(x=Year,y=Area,fill=Woodland))+
  geom_bar(stat="identity",position="dodge")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=60,hjust=1,colour="black"))+
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(colour="black"))+
  labs(x="",y="Area (thousand ha)")

##Plot changes in new planting over longer time scale for all trees
#Read in data
plant_all<-read_excel("J:/GISprojects/Ecosystems Analysis/FPPH/THRS indicators/Data/National Forest Inventory England/Inventory reports/Ch1_Woodland_FS2019.xlsx",sheet="data for figure 1.6",range="A5:B49")
colnames(plant_all)
names(plant_all)[names(plant_all)=="Year                             (ending 31/3)"]<-"Year"
names(plant_all)[names(plant_all)=="England"]<-"Area"
sapply(plant_all,class)
ggplot(data=plant_all,aes(x=Year,y=Area,group=1))+
  geom_path()+
  geom_point()+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=60,hjust=1,colour="black"))+
  theme(axis.title.x=element_blank())+
  theme(axis.text.y=element_text(colour="black"))+
  labs(x="",y="Area (thousand ha)")+
  coord_cartesian(ylim=c(0,7))+
  scale_x_continuous(breaks=c(1976,1980,1984,1988,1992,1996,2000,2004,2008,2012,2016,2020))+
  scale_y_continuous(breaks=c(0:7))
