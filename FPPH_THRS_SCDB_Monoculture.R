##Script Name: Area of monoculture
##Author: Izzy Hassall, Ecosystems Analysis
##Contact (if different from above):
##Date Created: 2020-01-17
##Date Modified:
##Licence:
##Abstract:


##R version 3.6.1 (2019-07-05)
##Dependencies:
library(tidyr)
library(ggplot2)
library(plyr)
library(dplyr)

#Dara used: SCDB Dataset "J:\GISprojects\Ecosystems Analysis\FPPH\THRS indicators\Data\SCDB\National_Forest_Estate_Subcompartments_England_2019.csv"
##Area of monoculture using the SCDB dataset
##Using the % cover of principal species to set threshold for monoculture
scdb<-read.csv("J:/GISprojects/Ecosystems Analysis/FPPH/THRS indicators/Data/SCDB/National_Forest_Estate_Subcompartments_England_2019.csv")
scdb_spec<-scdb[,c(9,14,38)]
scdb_spec<-scdb_spec[!(scdb_spec$PRISPECIES==" "),]
#Select all entries where % cover>80 (areas of monoculture)
scdb_mono<-scdb_spec[(scdb_spec$PRIPCTAREA>80),]
#Sum area of monoculture for each species
scdb_mono<-as.data.frame(scdb_mono)
sapply(scdb_mono,class)
spec_mono<-aggregate(scdb_mono$Shape__Area,by=list(PRISPECIES=scdb_mono$PRISPECIES),FUN=sum)
names(spec_mono)[names(spec_mono)=="x"]<-"Area"
names(spec_mono)[names(spec_mono)=="PRISPECIES"]<-"PrimarySpecies"
#Remove Mixed broadleaves,"other" species to leave only single species
spec_mono<-spec_mono[-c(46,47,56:66),]
#Data currently in m2 - convert to km2
spec_mono$Area<-spec_mono$Area*0.000001
sapply(spec_mono,class)
spec_mono$Area<-as.numeric(spec_mono$Area)

#Add areas to give total area of monoculture
sum(spec_mono$Area)
#Create mock dataset using this as starting point for 2019
mono_mock<-matrix(c(1006.162,1007.001,1007.894,1008.023,1007.342,1007.211,1007.459),
                  nrow=7,
                  dimnames=list(c("2019","2020","2021","2022","2023","2024","2025"),
                                c("Monoculture Area")))
sapply(mono_mock,class)
mono_mock<-as.character(mono_mock)
mono_mock
year<-c("2019","2020","2021","2022","2023","2024","2025")
mono_mock<-as.data.frame(mono_mock)
mono_mock$Year<-year
names(mono_mock)[names(mono_mock)=="mono_mock"]<-"MonoArea"
sapply(mono_mock,class)
mono_mock$MonoArea<-as.numeric(as.character(mono_mock$MonoArea))
ggplot(data=mono_mock,aes(x=year,y=MonoArea))+
  geom_bar(stat="identity",width=0.75,colour="black",fill="grey87")+
  labs(x="",y="Area (Km2)")+
  coord_cartesian(ylim=c(1005,1010))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=50,hjust=1))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y=element_text(colour="black"))

#Plot all species bar chart to visually assess species with most monoculture
ggplot(data=spec_mono,aes(x=PrimarySpecies,y=Area))+
  geom_bar(stat="identity",width=0.75,colour="black",fill="grey87")+
  labs(x="",y="Area (ha)")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=50,hjust=1))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y=element_text(colour="black"))
#Species with high area of monoculture (and worth plotting) are: Beech, Birch, Corsican pine, Douglas fir, Hybrid larch,Japanese larch,Lodgepole pine, Norway spruce, Oak, Scots pine, Sitka spruce
#Create subset of principal monoculture species
species<-c("Beech","Birch (downy/silver)","Corsican pine","Douglas fir","Hybrid larch","Japanese larch","Lodgepole pine","Norway spruce","Oak (robur/petraea)","Scots pine","Sitka spruce")
high_mono<-filter(spec_mono,PrimarySpecies%in%species)
ggplot(data=high_mono,aes(x=PrimarySpecies,y=Area))+
  geom_bar(stat="identity",width=0.75,colour="black",fill="grey87")+
  labs(x="",y="Area (Km2)")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=50,hjust=1))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y=element_text(colour="black"))
