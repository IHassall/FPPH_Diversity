##FPPH THRS Indicators Diversity##
##FC SCDB Dataset to examine species and structural diversity##
##Date 2020-01-13##

library(vegan)
library(tidyr)
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape)
library(tibble)

#Read in SCDB data
scdb<-read.csv("J:/GISprojects/Ecosystems Analysis/FPPH/THRS indicators/Data/SCDB/National_Forest_Estate_Subcompartments_England_2019.csv",header=T,na.strings=c(""))
scdb<-scdb[,c(9,18,20,26,38)]
#Rename columns
colnames(scdb)
names(scdb)[names(scdb)=="PRISPECIES"]<-"PrimarySpecies"
names(scdb)[names(scdb)=="CULTIVATN"]<-"Cultivation"
names(scdb)[names(scdb)=="PRILANDUSE"]<-"PrimaryLandUse"
names(scdb)[names(scdb)=="PRIHABITAT"]<-"PrimaryHabitat"
names(scdb)[names(scdb)=="Shape__Area"]<-"Area"

#Create datasets for each variable and remove blanks
#Species
scdb_species<-scdb[,c(1,5)]
scdb_species<-scdb_species[!(scdb_species$PrimarySpecies==" "),]
levels(scdb_species$PrimarySpecies)
scdb_species<-as.data.frame(scdb_species)
#Cultivation
scdb_cult<-scdb[,c(2,5)]
scdb_cult<-scdb_cult[!(scdb_cult$Cultivation==" "),]
levels(scdb_cult$Cultivation)
#Land Use
scdb_land<-scdb[,c(3,5)]
scdb_land<-scdb_land[!(scdb_land$PrimaryLandUse==" "),]
#Habitat
scdb_hab<-scdb[,c(4,5)]
scdb_hab<-scdb_hab[!(scdb_hab$PrimaryHabitat==" "),]

#Sum area of each level for each dataset
species_area<-aggregate(scdb_species$Area,by=list(PrimarySpecies=scdb_species$PrimarySpecies),FUN=sum)
names(species_area)[names(species_area)=="x"]<-"Area"
cult_area<-aggregate(scdb_cult$Area,by=list(Cultivation=scdb_cult$Cultivation),FUN=sum)
names(cult_area)[names(cult_area)=="x"]<-"Area"
land_area<-aggregate(scdb_land$Area,by=list(PrimaryLandUse=scdb_land$PrimaryLandUse),FUN=sum)
names(land_area)[names(land_area)=="x"]<-"Area"
hab_area<-aggregate(scdb_hab$Area,by=list(PrimaryHabitat=scdb_hab$PrimaryHabitat),FUN=sum)
names(hab_area)[names(hab_area)=="x"]<-"Area"
#Convert from m2 to km2
species_area$Area<-species_area$Area*0.000001
cult_area$Area<-cult_area$Area*0.000001
land_area$Area<-land_area$Area*0.000001
hab_area$Area<-hab_area$Area*0.000001

#Shannon Diversity
#SPECIES 
#Transpose species_area
species_area_t<-t(species_area)
species_area_t<-as.matrix(species_area_t)
colnames(species_area_t)<-as.character(unlist(species_area_t[1,]))
species_area_t=species_area_t[-1,]
species_area_t<-sapply(species_area_t,as.numeric)
diversity(species_area_t)

#CULTIVATION
cult_area_t<-t(cult_area)
cult_area_t<-as.matrix(cult_area_t)
colnames(cult_area_t)<-as.character(unlist(cult_area_t[1,]))
cult_area_t=cult_area_t[-1,]
cult_area_t<-sapply(cult_area_t,as.numeric)
diversity(cult_area_t)

#LAND USE
land_area_t<-t(land_area)
land_area_t<-as.matrix(land_area_t)
colnames(land_area_t)<-as.character(unlist(land_area_t[1,]))
land_area_t=land_area_t[-1,]
land_area_t<-sapply(land_area_t,as.numeric)
diversity(land_area_t)

#HABITAT
hab_area_t<-t(hab_area)
hab_area_t<-as.matrix(hab_area_t)
colnames(hab_area_t)<-as.character(unlist(hab_area_t[1,]))
hab_area_t=hab_area_t[-1,]
hab_area_t<-sapply(hab_area_t,as.numeric)
diversity(hab_area_t)

##Selected attributes of Land Use and Cultivation
#Planting aspects of cultivation 
plant<-cult_area[c(2,5,15,16),]
#Land use creating open areas
open<-land_area[c(5,12,18,29),]
#Obtain annual values from SCDB dataset to create time series of open and planted areas
