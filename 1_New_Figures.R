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

##SPECIES##################################################
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
blocks<-c("2009-2014","2015-2020","2021-2025","2026-2030","2031-2035","2036-2040")
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
rownames(all_mock_cut)=blocks

#Restructure using melt()
all_mock_melt<-melt(all_mock_cut)
#Change column names
colnames(all_mock_melt)<-c("Block","Species","Total")

#Divided bar charts to show change over time
#Specify order of species to correspond with NFI
all_mock_melt$Species<-factor(all_mock_melt$Species,levels=c("Oak","Beech","Sycamore","Ash","Birch","Hazel","Other broadleaves","Sitka spruce","Scots pine","Other conifers"))
plot<-ggplot(data=all_mock_melt,aes(x=Block,y=Total,fill=Species))+
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
df0914<-filter(all_mock_melt,Block=="2009-2014")
df1520<-filter(all_mock_melt,Block=="2015-2020")
df2025<-filter(all_mock_melt,Block=="2021-2025")
df2530<-filter(all_mock_melt,Block=="2026-2030")
df3035<-filter(all_mock_melt,Block=="2031-2035")
df3540<-filter(all_mock_melt,Block=="2036-2040")
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
  labs(x="",y="Effective Number of Species")
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

#Make indicator species graphs over time
#Corsican Pine
cp<-filter(all_mock,Species=="Corsican pine")
cp_plot<-ggplot(data=cp,aes(x=Block,y=Total,group=1))+
  geom_point()+
  geom_line()+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=45,hjust=1,colour="black"))+
  theme(axis.text.y=element_text(colour="black"))+
  labs(x="",y="Area (1000 ha)")+
  scale_y_continuous(limits=c(0,60),breaks=seq(0,60,by=10))
cp_plot+theme(aspect.ratio=0.9)+
  ggtitle("Corsican Pine")

#Larch
lar<-filter(all_mock,Species=="Larches")
lar_plot<-ggplot(lar,aes(Block,Total,group=1))+
  geom_point()+
  geom_line()+
  theme_bw()+
  theme(panel.grid=element_blank())+
  theme(axis.text.x=element_text(angle=45,hjust=1,colour="black"))+
  labs(x="",y="Area (1000 ha)")+
  scale_y_continuous(limits=c(0,60),breaks=seq(0,60,by=10))
lar_plot+theme(aspect.ratio=0.9)+
  ggtitle("Larches")

#Calculate species evenness
#H/ln(SR)
all_mock_div=mutate(all_mock_div,Evenness=(Shannon/log(10)))
even_plot<-ggplot(data=all_mock_div,aes(x=blocks,y=Evenness))+
  geom_bar(stat="identity",fill="grey87",colour="black")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=45,hjust=1,colour="black"))+
  theme(axis.text.y=element_text(colour="black"))+
  labs(x="",y="Species Evenness")+
  coord_cartesian(ylim=c(0.8,1))
even_plot+theme(aspect.ratio=0.9)

###################################################################
##AGE##############################################################
###################################################################

##Stocked area data for both broadleaves and conifer species
#Create mock dataset with AreaTotal as starting point 
age_mock<-matrix(c(276,290,282,299,313,326,
                       324,312,298,338,358,369,
                       280,295,245,302,345,327,
                       161,140,174,147,178,189,
                       104,108,120,137,168,137,
                       64,64,65,64,64,65),
                     nrow=6,
                     dimnames=list(c("2009-2014","2015-2020","2021-2025","2026-2030","2031-2035","2036-2040"),
                                   c("0-20","21-40","41-60","61-80","81-100","100+")))
age_melt<-melt(age_mock)
colnames(age_melt)<-c("Block","AgeClass","TotalArea")
age_melt<-as.data.frame(age_melt)
sapply(age_melt,class)
age_melt$AgeClass<-factor(age_melt$AgeClass,levels=c("0-20","21-40","41-60","61-80","81-100","100+"))
plot<-ggplot(data=age_melt,aes(x=Block,y=TotalArea,fill=AgeClass))+
  labs(y="Area (1000 ha)")+
  geom_bar(stat="identity")+
  scale_fill_brewer(palette="Set3")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  theme(axis.title.x=element_blank())
plot+theme(aspect.ratio=1)

#Calculate Shannon and ENS
age_all_div<-diversity(age_mock)
years<-c("2009-2014","2015-2020","2021-2025","2026-2030","2031-2035","2036-2040")
age_all_div<-data.frame(years,age_all_div)
names(age_all_div)[names(age_all_div)=="age_all_div"]<-"Shannon"
sapply(age_all_div,class)
#Calculate ENS for all species 
age_all_div$ENS<-exp(age_all_div$Shannon)
age_plot<-ggplot(data=age_all_div,aes(x=years,y=ENS))+
  geom_bar(stat="identity",fill="grey87",colour="black")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=45,hjust=1,colour="black"))+
  theme(axis.text.y=element_text(colour="black"))+
  labs(x="",y="Effective Number of Age Classes")
age_plot+theme(aspect.ratio=0.9)
#Calculate ENS for all species with croppped axis
age_plot<-ggplot(data=age_all_div,aes(x=years,y=ENS))+
  geom_bar(stat="identity",fill="grey87",colour="black")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=45,hjust=1,colour="black"))+
  theme(axis.text.y=element_text(colour="black"))+
  labs(x="",y="Effective Number of Age Classes")+
  coord_cartesian(ylim=c(4,5.5))
age_plot+theme(aspect.ratio=0.9)

#Calculate age class evenness
#H/ln(SR)
age_all_div=mutate(age_all_div,Evenness=(Shannon/log(6)))
even_plot<-ggplot(data=age_all_div,aes(x=years,y=Evenness))+
  geom_bar(stat="identity",fill="grey87",colour="black")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=45,hjust=1,colour="black"))+
  theme(axis.text.y=element_text(colour="black"))+
  labs(x="",y="Age Class Evenness")+
  coord_cartesian(ylim=c(0.8,1))
even_plot+theme(aspect.ratio=0.9)

##############################################################
##SIZE########################################################
#############################################################
#Stacked bar plots over time
bl_mock<-matrix(c(70,79,89,93,100,105,
                  127,114,132,157,165,181,
                  201,165,198,234,253,267,
                  172,189,143,158,189,212,
                  157,157,176,162,166,172,
                  84,89,93,93,98,100,
                  73,73,74,76,80,76),
                nrow=6,
                dimnames=list(c("2009-2014","2015-2020","2021-2025","2025-2030","2031-2035","2036-2040"),
                              c("0-10","11-20","21-40","41-60","61-80","81-100","100+")))


bl_mock_melt<-melt(bl_mock)
colnames(bl_mock_melt)<-c("Block","DBHClass","Total")
bl_mock_melt<-as.data.frame(bl_mock_melt)
sapply(bl_mock_melt,class)
bl_mock_melt$DBHClass<-factor(bl_mock_melt$DBHClass,levels=c("0-10","11-20","21-40","41-60","61-80","81-100","100+"))
size_plot<-ggplot(data=bl_mock_melt,aes(x=Block,y=Total,fill=DBHClass))+
  labs(y="Area (1000 ha)")+
  geom_bar(stat="identity")+
  scale_fill_brewer(palette="Set3")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  theme(axis.title.x=element_blank())
size_plot+theme(aspect.ratio=0.9)

#Calculate Shannon diversity
blocks<-c("2009-2014","2015-2020","2021-2025","2026-2030","2031-2035","2036-2040")
size_div<-diversity(bl_mock)
size_div
size_div<-data.frame(blocks,size_div)
names(size_div)[names(size_div)=="size_div"]<-"Shannon"
#Plot Shannon over time 
ggplot(data=size_div,aes(x=blocks,y=Shannon))+
  geom_bar(stat="identity",width=0.75,colour="black",fill="grey87")+
  labs(x="",y="Shannon H")+
  coord_cartesian(ylim=c(1.6,1.62))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=50,hjust=1))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y=element_text(colour="black"))

#Calculate ENS of DBH Broadleaves diversity
size_div$ENS<-exp(size_div$Shannon)
size_plot<-ggplot(data=size_div,aes(x=blocks,y=ENS))+
  geom_bar(stat="identity",colour="black",fill="grey87")+
  labs(x="",y="Effective Number of DBH Classes")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y=element_text(colour="black"))
size_plot+theme(aspect.ratio=0.9)
#Cropped y axis
size_plot<-ggplot(data=size_div,aes(x=blocks,y=ENS))+
  geom_bar(stat="identity",colour="black",fill="grey87")+
  labs(x="",y="Effective Number of DBH Classes")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=45,hjust=1))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y=element_text(colour="black"))+
  coord_cartesian(ylim=c(5,7))
size_plot+theme(aspect.ratio=0.9)

#Generate size class evenness
#H/ln(SR)
size_div=mutate(size_div,Evenness=(Shannon/log(7)))
even_plot<-ggplot(data=size_div,aes(x=blocks,y=Evenness))+
  geom_bar(stat="identity",fill="grey87",colour="black")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=45,hjust=1,colour="black"))+
  theme(axis.text.y=element_text(colour="black"))+
  labs(x="",y="DBH Class Evenness")+
  coord_cartesian(ylim=c(0.8,1))
even_plot+theme(aspect.ratio=0.9)
