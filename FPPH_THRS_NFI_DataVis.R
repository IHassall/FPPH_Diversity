##Preliminary R Exploration of NFI Data##
##Species Diversity##
##Date: 2020-01-09##
##Data Visualisation##
##Needs to follow on from FPPH_THRS_NFI_DataPrep script##

library(readxl)
library(tidyr)
library(ggplot2)
library(plyr)
library(dplyr)

##BROADLEAVES Species for NFI regions##
#Bar Charts for each region of numbers of trees
NWeng_bp<-ggplot(data=NWeng,aes(x=Species,y=Total))+
  geom_bar(stat="identity",position="dodge",width=1,colour="black",fill="grey87")+
  scale_y_continuous(breaks=seq(0,12,len=7))+
  labs(y="Number of Trees (millions)")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=45,hjust=1))
NWeng_bp

#Create % column for total number of trees 
#Need this for divided bar charts and pie charts
NWeng=mutate(NWeng,Proportion_of_Total=Total/sum(Total))
NEeng=mutate(NEeng,Proportion_of_Total=Total/sum(Total))
YorkHumb=mutate(YorkHumb,Proportion_of_Total=Total/sum(Total))
EMid=mutate(EMid,Proportion_of_Total=Total/sum(Total))
Eeng=mutate(Eeng,Proportion_of_Total=Total/sum(Total))
SELon=mutate(SELon,Proportion_of_Total=Total/sum(Total))
SWeng=mutate(SWeng,Proportion_of_Total=Total/sum(Total))
WMid=mutate(WMid,Proportion_of_Total=Total/sum(Total))

#Pie charts
ggplot(NWeng,aes(x="",y=Proportion_of_Total,fill=Species))+
  geom_bar(width=1,stat="identity")+
  coord_polar("y",start=0)+
  scale_fill_discrete()+
  theme_void()
#Can adjust colour scale as needed

###Divided bar charts to compare region composition###
#Stacked bar chart
#Remove All Broadleaves from main dataset
eng_bl_species<-eng_bl[!(eng_bl$Species=="All broadleaves"),]
#Specify order of species to correspond with NFI
eng_bl_species$Species<-factor(eng_bl_species$Species,levels=c("Oak","Beech","Sycamore","Ash","Birch","Sweet chestnut","Hazel","Hawthorn","Alder","Willow","Other broadleaves"))
ggplot(data=eng_bl_species,aes(x=Region,y=Total,fill=Species))+
  labs(y="Number of Trees (millions)")+
  geom_bar(stat="identity")+
  scale_fill_brewer(palette="Set3")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=60,hjust=1))+
  theme(axis.title.x=element_blank())
#Could flip axis with "+cord_flip()" but think it looks OK as is

##CONIFERS DATA##
#Divided bar charts to compare region composition
#Remove "All broadleaves" rows 
eng_con_species<-eng_con[!(eng_con$Species=="All conifers"),]
#Specify order of species to correspond with NFI
eng_con_species$Species<-factor(eng_con_species$Species,levels=c("Sitka spruce","Scots pine","Corsican pine","Norway spruce","Larches","Douglas fir","Lodgepole pine","Other conifers"))
ggplot(data=eng_con_species,aes(x=Region,y=Total,fill=Species))+
  labs(y="Number of Trees (millions)")+
  geom_bar(stat="identity")+
  scale_fill_brewer(palette="Set3")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=60,hjust=1))+
  theme(axis.title.x=element_blank())
