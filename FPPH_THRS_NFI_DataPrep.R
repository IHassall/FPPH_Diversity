##Preliminary R Exploration of NFI Data##
##Species Diversity##
##Date: 2020-01-08##
##Data Used: NFI_Prelim_BL_Ash_Tables.xls and FR_NFI_NumberConiferTreesInGB.xlsx obtained from NFI website

library(readxl)
library(tidyr)
library(ggplot2)
library(plyr)
library(dplyr)

#########################################
##READ IN AND ARRANGE REGIONAL DATASET##
##BROADLEAVES##
#Read in excel doc and select England area
eng_bl<-read_excel("J:/GISprojects/Ecosystems Analysis/FPPH/THRS indicators/Data/National Forest Inventory England/Inventory reports/NFI_Prelim_BL_Ash_Tables.xls",sheet="Table A.7",range="B5:F110")
#Add column for region information
eng_bl$Region<-NA
eng_bl<-eng_bl[c(6,1,2,3,4,5)]
#Change Column names
names(eng_bl)[names(eng_bl)=="Principal species"]<-"Species"
names(eng_bl)[names(eng_bl)=="Private sector"]<-"Private"
colnames(eng_bl)

#Input region data into column
a<-rep("NW England",times=14)
b<-rep("NE England",times=13)
c<-rep("Yorkshire and Humber",times=13)
d<-rep("E Midlands",times=13)
e<-rep("E England",times=13)
f<-rep("SE and London",times=13)
g<-rep("SW England",times=13)
h<-rep("W Midlands",times=13)
regions<-c(a,b,c,d,e,f,g,h)
regions
eng_bl$Region<-regions
#Remove irrelevant columns and rows
eng_bl<-eng_bl[-c(1,2,15,28,41,54,67,80,93),][,-5]

#Specify columns as numeric
sapply(eng_bl,class)
eng_bl$Total<-as.numeric(eng_bl$Total)
eng_bl$Private<-as.numeric(eng_bl$Private)
eng_bl$FC<-as.numeric(eng_bl$FC)
eng_bl$Species<-as.factor(eng_bl$Species)
sapply(eng_bl,class)

#Convert values from thousands to millions
eng_bl$Total<-eng_bl$Total*0.001
eng_bl$Private<-eng_bl$Private*0.001
eng_bl$FC<-eng_bl$FC*0.001

##CONIFERS
#Load conifer data

#Read in excel doc and select England area
eng_con<-read_excel("J:/GISprojects/Ecosystems Analysis/FPPH/THRS indicators/Data/National Forest Inventory England/Inventory reports/FR_NFI_NumberConiferTreesInGB.xlsx",sheet="Table 2",range="B5:F86")
#Add column for region information
eng_con$Region<-NA
eng_con<-eng_con[c(6,1,2,3,4,5)]
#Change Column names
names(eng_con)[names(eng_con)=="Principal species"]<-"Species"
names(eng_con)[names(eng_con)=="Private sector"]<-"Private"
colnames(eng_con)

#Input region data into column
a<-rep("NW England",times=11)
b<-rep("NE England",times=10)
c<-rep("Yorkshire and Humber",times=10)
d<-rep("E Midlands",times=10)
e<-rep("E England",times=10)
f<-rep("SE and London",times=10)
g<-rep("SW England",times=10)
h<-rep("W Midlands",times=10)
regions<-c(a,b,c,d,e,f,g,h)
regions
eng_con$Region<-regions
#Remove irrelevant columns and rows
eng_con<-eng_con[-c(1,2,12,22,32,42,52,62,72),][,-5]

#Specify columns as numeric
sapply(eng_con,class)
eng_con$Total<-as.numeric(eng_con$Total)
eng_con$Private<-as.numeric(eng_con$Private)
eng_con$FC<-as.numeric(eng_con$FC)
eng_con$Species<-as.factor(eng_con$Species)
sapply(eng_con,class)

#Convert values from thousands to millions
eng_con$Total<-eng_con$Total*0.001
eng_con$Private<-eng_con$Private*0.001
eng_con$FC<-eng_con$FC*0.001

##COMBINE INTO ONE DATASET
##ALL SPECIES
eng_all<-rbind(eng_bl,eng_con)
sapply(eng_all,class)
##This dataset shows all species for all England regions
#Includes "All" rows
#In millions of trees

#Make dataset with only "All broadleaves/conifers" rows for each region
eng_all_total<-filter(eng_all,Species %in% c("All broadleaves","All conifers"))
#Dataset with "All broadleaves/conifers" removed
eng_all_species<-eng_all[!(eng_all$Species=="All broadleaves"),]
eng_all_species<-eng_all_species[!(eng_all_species$Species=="All conifers"),]


