##FPPH THRS##
##Species Diversity##
##Date: 2020-01-13##
##Data Used: NFI_Prelim_BL_Ash_Tables.xls and FR_NFI_NumberConiferTreesInGB.xlsx obtained from NFI website
##Creating mock data to show change in Shannon diversity over time

library(readxl)
library(tidyr)
library(ggplot2)
library(plyr)
library(dplyr)
library(vegan)

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
#Shannon diversity over time
#Create mock data of number of trees (millions) in 5 year blocks
Blocks<-c("2009-2014","2015-2020","2020-2025","2025-2030","2030-2035","2035-2040","2040-2045","2045-2050")
eng_all_blocks<-matrix(c(92.42284,49.31392,80.10662,98.67758,118.00641,43.95191,182.03947,80.382,29.81457,40.07122,161.57519,104.54371,38.99646,34.77352,26.24529,30.76485,20.88422,12.78085,17.97287,
                         91.87456,50.24783,79.38945,97.13421,118.67555,44.1515,182.36744,79.2145,29.12032,40.9327,162.67281,105.68772,39.2912,33.0125,25.7845,31.3578,21.5672,11.34921,18.56211,
                         91.85591,50.01022,78.8911,96.9921,118.90992,44.26589,182.78195,79.02917,28.65729,40.78253,162.78234,105.23416,39.56414,33.5612,25.43918,32.76941,22.78455,10.9526,18.27873,
                         90.87945,50.67212,78.25235,96.5672,118.560933,44.78241,182.9384,78.92819,29.24359,40.550622,162.12782,105.555782,39.98712,33.2451,25.56912,31.5092,23.02011,11.02391,18.01922,
                         91.23411,49.8921,79.65223,97.32414,118.52142,44.21341,182.76221,79.34152,29.12412,40.678231,162.49225,105.21022,39.34267,33.55672,25.76237,31.252231,22.67544,11.23415,17.98028,
                         91.78201,49.21354,79.01204,96.98723,118.90242,43.67981,182.31562,79.60792,29.65281,40.817211,161.98623,105.4512,39.76281,33.6798,25.67283,31.26712,22.98725,11.56102,17.827312,
                         91.98271,49.44592,79.28751,97.02934,118.90562,43.687641,182.42843,79.641726,29.68736,40.87947,162.03948,105.57649,39.78951,33.68795,25.50942,31.19847,22.40958,11.45989,17.687648,
                         92.3123,49.56721,79.45612,97.45129,118.75624,43.87941,182.87614,79.56498,29.51428,40.67467,161.91492,104.98492,39.567192,33.89752,25.20395,30.90384,22.2934,11.3492,17.65018),
                       nrow=19,
                       dimnames=list(c("Oak","Beech","Sycamore","Ash","Birch","Sweet chestnut","Hazel","Hawthorn","Alder","Willow","Other broadleaves","Sitka spruce","Scots pine","Corsican pine","Norway spruce","Larches","Douglas fir","Lodgepole pine","Other conifers"),
                                     c("2009-2014","2015-2020","2020-2025","2025-2030","2030-2035","2035-2040","2040-2045","2045-2050")))

#Transpose mock dataset
eng_blocks<-t(eng_all_blocks)
#Shannon diversity index on mock data
eng_blocks_div<-diversity(eng_blocks)
eng_blocks_div
#Create data frame
eng_div<-data.frame(Blocks,eng_blocks_div)
names(eng_div)[names(eng_div)=="eng_blocks_div"]<-"Shannon"
#Visualise as barplot with rescaled y axis to show small differences
ggplot(data=eng_div,aes(x=Blocks,y=Shannon))+
  geom_bar(stat="identity",width=0.75,colour="black",fill="grey87")+
  labs(x="",y="Shannon H")+
  coord_cartesian(ylim=c(2.7,2.71))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=50,hjust=1))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y=element_text(colour="black"))

  

