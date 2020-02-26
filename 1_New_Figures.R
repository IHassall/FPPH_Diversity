##Script Name: New Diversity Graphs

##Author: Izzy Hassall, Ecosystems Analysis
##Contact (if different from above):

##Date Created: 2020-02-26

##Date Modified:

##Licence:

##Abstract:
#Species/Age/DBH diversity - species/age/dbh class composition by area over time, current snapshot (table), ENS 

##R version 3.6.1 (2019-07-05)
##Dependencies:
library(readxl)
library(tidyr)
library(ggplot2)
library(plyr)
library(dplyr)
library(vegan)
library(reshape)

#NFI Annual Stats Release
#Interim estimates at March 2012 - data I was already using in other scripts 

###Change existing graphs so that mock data shows a general trend and more distinct changes###

##READ IN AND ARRANGE COUNTRIES BL AND CON DATASETS##
#BROADLEAVES
eng_bl<-read_excel("J:/GISprojects/Ecosystems Analysis/FPPH/THRS indicators/Data/National Forest Inventory England/Inventory reports/NFI_Prelim_BL_Ash_Tables.xls",sheet="Table 10",range="B5:F19")
#Remove irrelevant columns and rows
eng_bl<-eng_bl[-c(1,2),][,-4]
#Change Column names
names(eng_bl)[names(eng_bl)=="Principal species"]<-"Species"
names(eng_bl)[names(eng_bl)=="Private sector"]<-"Private"
colnames(eng_bl)
#Specify column type
sapply(eng_bl,class)
eng_bl$Total<-as.numeric(eng_bl$Total)
eng_bl$Private<-as.numeric(eng_bl$Private)
eng_bl$FC<-as.numeric(eng_bl$FC)
eng_bl$Species<-as.factor(eng_bl$Species)
sapply(eng_bl,class)
#Remove "All broadleaves" row
eng_bl_species<-eng_bl[-1,]

#CONIFERS
eng_con<-read_excel("J:/GISprojects/Ecosystems Analysis/FPPH/THRS indicators/Data/National Forest Inventory England/Inventory reports/FR_NFI_NumberConiferTreesInGB.xlsx",sheet="Table 1",range="B5:F16")
#Remove irrelevant columns and rows
eng_con<-eng_con[-c(1,2),][,-4]
#Change Column names
names(eng_con)[names(eng_con)=="Principal species"]<-"Species"
names(eng_con)[names(eng_con)=="Private sector"]<-"Private"
colnames(eng_bl)
#Specify column type
sapply(eng_con,class)
eng_con$Total<-as.numeric(eng_con$Total)
eng_con$Private<-as.numeric(eng_con$Private)
eng_con$FC<-as.numeric(eng_con$FC)
eng_con$Species<-as.factor(eng_con$Species)
sapply(eng_con,class)
#Remove "All broadleaves" row
eng_con_species<-eng_con[-1,]

##ALL SPECIES
#Create single dataset with all species
eng_all<-rbind(eng_bl_species,eng_con_species)
sapply(eng_all,class)

#List species
species<-c("Oak","Beech","Sycamore","Ash","Birch","Sweet chestnut","Hazel","Hawthorn","Alder","Willow","Other broadleaves","Sitka spruce","Scots pine","Corsican pine","Norway spruce","Larches","Douglas fir","Lodgepole pine","Other conifers")
#Remove irrelevant columns
eng_all_div<-eng_all[,-c(1,2,3)]
#Set as data matrix
eng_all_div<-data.matrix(eng_all_div)

#Calculate Shannon H
eng_all_H<-diversity(eng_all_div)
eng_all_H

########################################
#Generate mock dataset to show changes in SPECIES diversity over time
#Based on area in thousand ha
#FOR ALL SPECIES WITH OVER 40,000 HA
species<-c("Oak","Beech","Sycamore","Ash","Birch","Hazel","Hawthorn","Willow","Other broadleaves","Sitka spruce","Scots pine","Corsican pine","Larches","Other conifers")
all_mock<-matrix(c(151,155,155,160,165,170,
                   59,64,62,67,68,72,
                   74,79,78,83,87,93,
                   120,124,118,115,110,100,
                   90,100,104,108,112,118,
                   64,66,62,68,70,70,
                   57,60,55,60,65,67,
                   41,38,37,40,45,50,
                   191,195,190,186,190,196,
                   80,87,90,94,95,100,
                   61,54,56,60,63,60,
                   40,44,46,50,56,54,
                   40,40,48,52,48,53,
                   84,86,88,90,93,90),
                 nrow=6)
colnames(all_mock)=species
blocks<-c("2009-2014","2015-2020","2020-2025","2025-2030","2030-2035","2035-2040")
rownames(all_mock)=blocks

#Restructure using melt()
all_mock<-melt(all_mock)
#Change column names
colnames(all_mock)<-c("Block","Species","Total")

#Divided bar charts to show change over time
#Specify order of species to correspond with NFI
ggplot(data=all_mock,aes(x=Block,y=Total,fill=Species))+
  labs(y="Number of Trees (millions)")+
  geom_bar(stat="identity")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=60,hjust=1))+
  theme(axis.title.x=element_blank())
#Still too many divisions

##################################################
#MAKE TEN CATEGORIES 
species_cut<-c("Oak","Beech","Sycamore","Ash","Birch","Hazel","Other broadleaves","Sitka spruce","Scots pine","Other conifers")
all_mock_cut<-matrix(c(151,160,160,190,234,260,
                   59,70,65,78,80,89,
                   74,90,120,85,90,134,
                   120,129,110,120,134,140,
                   90,112,85,110,75,60,
                   64,69,75,68,85,90,
                   289,300,320,300,305,305,
                   80,93,95,99,108,120,
                   61,54,50,68,73,70,
                   164,155,170,166,190,204),
                 nrow=6)
colnames(all_mock_cut)=species_cut
blocks<-c("2009-2014","2015-2020","2020-2025","2025-2030","2030-2035","2035-2040")
rownames(all_mock_cut)=blocks

#Restructure using melt()
all_mock_cut<-melt(all_mock_cut)
#Change column names
colnames(all_mock_cut)<-c("Block","Species","Total")

#Divided bar charts to show change over time
#Specify order of species to correspond with NFI
all_mock_cut$Species<-factor(all_mock_cut$Species,levels=c("Oak","Beech","Sycamore","Ash","Birch","Hazel","Other broadleaves","Sitka spruce","Scots pine","Other conifers"))
plot<-ggplot(data=all_mock_cut,aes(x=Block,y=Total,fill=Species))+
  labs(y="Area (1000 ha)")+
  geom_bar(stat="identity")+
  scale_fill_brewer(palette="Set3")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  theme(axis.title.x=element_blank())
plot+theme(aspect.ratio=1)

################################################################
#Proportional stacked bar chart
#Add proportion column to df
df0914<-filter(all_mock_cut,Block=="2009-2014")
df1520<-filter(all_mock_cut,Block=="2015-2020")
df2025<-filter(all_mock_cut,Block=="2020-2025")
df2530<-filter(all_mock_cut,Block=="2025-2030")
df3035<-filter(all_mock_cut,Block=="2030-2035")
df3540<-filter(all_mock_cut,Block=="2035-2040")
df0914=mutate(df0914,Proportion=Total/sum(Total))
df1520=mutate(df1520,Proportion=Total/sum(Total))
df2025=mutate(df2025,Proportion=Total/sum(Total))
df2530=mutate(df2530,Proportion=Total/sum(Total))
df3035=mutate(df3035,Proportion=Total/sum(Total))
df3540=mutate(df3540,Proportion=Total/sum(Total))
#Df of proportions
all_prop<-rbind(df0914,df1520,df2025,df2530,df3035,df3540)
sapply(all_prop,class)
all_prop$Species<-factor(all_prop$Species,levels=c("Oak","Beech","Sycamore","Ash","Birch","Hazel","Other broadleaves","Sitka spruce","Scots pine","Other conifers"))
#Plot proportional stacked bar chart
plot<-ggplot(data=all_prop,aes(x=Block,y=Proportion,fill=Species))+
  labs(y="Proportion of Total Woodland Area")+
  geom_bar(stat="identity")+
  theme_bw()+
  scale_fill_brewer(palette="Set3")+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  theme(axis.title.x=element_blank())
plot+theme(aspect.ratio=1)

###########################################################
species_cut<-c("Oak","Beech","Sycamore","Ash","Birch","Hazel","Other broadleaves","Sitka spruce","Scots pine","Other conifers")
all_mock_cut<-matrix(c(151,160,160,190,234,260,
                       59,70,65,78,80,89,
                       74,90,120,85,90,134,
                       120,129,110,120,134,140,
                       90,112,85,110,75,60,
                       64,69,75,68,85,90,
                       289,300,320,300,305,305,
                       80,93,95,99,108,120,
                       61,54,50,68,73,70,
                       164,155,170,166,190,204),
                     nrow=6)
colnames(all_mock_cut)=species_cut
blocks<-c("2009-2014","2015-2020","2020-2025","2025-2030","2030-2035","2035-2040")
rownames(all_mock_cut)=blocks

#Shannon diversity index on mock data
all_mock_div<-diversity(all_mock_cut)
all_mock_div
#Create data frame
all_mock_div<-data.frame(blocks,all_mock_div)
names(all_mock_div)[names(all_mock_div)=="all_mock_div"]<-"Shannon"

#Calculate ENS for all species 
all_mock_div$ENS<-exp(all_mock_div$Shannon)
div_plot<-ggplot(data=all_mock_div,aes(x=blocks,y=ENS))+
  geom_bar(stat="identity",fill="grey87",colour="black")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=45,hjust=1,colour="black"))+
  theme(axis.text.y=element_text(colour="black"))+
  labs(x="",y="Effective Number of Species")+
div_plot+theme(aspect.ratio=0.9)

#Crop y axis
all_mock_div$ENS<-exp(all_mock_div$Shannon)
div_plot<-ggplot(data=all_mock_div,aes(x=blocks,y=ENS))+
  geom_bar(stat="identity",fill="grey87",colour="black")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=45,hjust=1,colour="black"))+
  theme(axis.text.y=element_text(colour="black"))+
  labs(x="",y="Effective Number of Species")+
  coord_cartesian(ylim=c(7,9))
div_plot+theme(aspect.ratio=0.9)


