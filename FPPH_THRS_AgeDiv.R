##Script Name:FPPH_THRS_AgeDiv
##Author: Izzy Hassall, Ecosystems Analysis
##Contact (if different from above):
##Date Created: 2020-01-15
##Date Modified:
##Licence:
##Abstract: Age diversity calculations based on National Forest Inventory data and mock datasets


##R version 3.6.1 (2019-07-05)
##Dependencies:
library(readxl)
library(tidyr)
library(ggplot2)
library(plyr)
library(dplyr)
library(vegan)
library(reshape)

##Datasets used:"NFI_Prelim_BL_Ash_Tables" from NFI and "Ch1_Woodland_FS2019"

#########################################
##BROADLEAVES##
##Using "NFI_Prelim_BL_Ash_Tables" count data for broadleaves species
##Values are number of trees (millions)
#Read in excel doc and select England area
eng_bl_age<-read_excel("J:/GISprojects/Ecosystems Analysis/FPPH/THRS indicators/Data/National Forest Inventory England/Inventory reports/NFI_Prelim_BL_Ash_Tables.xls",sheet="Table 11",range="B5:F15")
#Remove irrelevant rows and columns
eng_bl_age<-eng_bl_age[-c(1,2,10),][,-4]
#Change Column names
names(eng_bl_age)[names(eng_bl_age)=="Age class (years)"]<-"AgeClass"
names(eng_bl_age)[names(eng_bl_age)=="Private sector"]<-"Private"
#Specify columns as numeric
sapply(eng_bl_age,class)
eng_bl_age$Total<-as.numeric(eng_bl_age$Total)
eng_bl_age$Private<-as.numeric(eng_bl_age$Private)
eng_bl_age$FC<-as.numeric(eng_bl_age$FC)
eng_bl_age$AgeClass<-as.factor(eng_bl_age$AgeClass)
sapply(eng_bl_age,class)
#Create dataframe of just total data
bl_age_total<-eng_bl_age[,c(1,4)]
bl_age_total<-t(bl_age_total)
#Set first column as column names
colnames(bl_age_total)<-as.character(unlist(bl_age_total[1,]))
bl_age_total=bl_age_total[-1,]
bl_age_total<-sapply(bl_age_total,as.numeric)
bl_age_total<-as.data.frame(bl_age_total)
names(bl_age_total)[names(bl_age_total)=="bl_age_total"]<-"TotalArea"
#Calculate Shannon diversity on age data
bl_age_div<-diversity(bl_age_total)
bl_age_div

#Pie chart for broadleaves England
#Recreate dataframe for total area of woodland
bl_total<-eng_bl_age[,c(1,4)]
sapply(bl_total,class)
bl_total$AgeClass<-as.factor(bl_total$AgeClass)
bl_total$AgeClass<-factor(bl_total$AgeClass,levels=c("0-10","11-20","21-40","41-60","61-80","81-100","100+"))
ggplot(bl_total,aes(x="",y=Total,fill=AgeClass))+
  geom_bar(width=1,stat="identity")+
  coord_polar("y",start=0)+
  scale_fill_brewer(palette="GnBu")+
  theme_void()

#Create mock dataset to illustrate change in age diversity over time
#In number of trees (millions)
#Starting point the Total column of Table 11 in "NFI_Prelim_BL_Ash_Tables" 
age_bl_mock<-matrix(c(21.38185214,22.03419,22.4561,22.1245,22.98512,23.1431,22.8791,23.4512,
                  219.1835626,220.0045,220.5623,220.90852,221.451,221.00532,221.3412,220.875,
                  353.4146369,352.9899,352.7541,352.6514,352.8975,352.2514,351.99765,351.2351,
                  190.1221531,190.0561,189.8796,189.7786,189.4235,189.7869,189.5412,188.9021,
                  117.4032743,117.0092,116.8742,116.1242,115.9874,116.2542,116.564,115.99875,
                  45.3744257,44.87975,44.89874,45.12312,45.00657,44.9872,44.4562,44.9352,
                  32.24089133,32.11223,32.5423,32.09854,31.55463,31.34523,31.2251,31.00212),
                 nrow=8,
                 dimnames=list(c("2009-2014","2015-2020","2021-2025","2026-2030","2031-2035","2036-2040","2041-2045","2046-2050"),
                               c("0-10","11-20","21-40","41-60","61-80","81-100","100+")))
#Calculate Shannon diversity
blocks<-c("2009-2014","2015-2020","2021-2025","2026-2030","2031-2035","2036-2040","2041-2045","2046-2050")
age_div<-diversity(age_bl_mock)
age_div
age_div<-data.frame(blocks,age_div)
names(age_div)[names(age_div)=="age_div"]<-"Shannon"
#Plot Shannon over time 
ggplot(data=age_div,aes(x=blocks,y=Shannon))+
  geom_bar(stat="identity",width=0.75,colour="black",fill="grey87")+
  labs(x="",y="Shannon H")+
  coord_cartesian(ylim=c(1.6,1.63))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=50,hjust=1))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y=element_text(colour="black"))

##SHANNON DIVERSITY FOR REGIONS
eng_age_reg<-read_excel("J:/GISprojects/Ecosystems Analysis/FPPH/THRS indicators/Data/National Forest Inventory England/Inventory reports/NFI_Prelim_BL_Ash_Tables.xls",sheet="Table A.8",range="B5:F78")

#Change Column names
names(eng_age_reg)[names(eng_age_reg)=="Principal species"]<-"Species"
names(eng_age_reg)[names(eng_age_reg)=="Private sector"]<-"Private"
names(eng_age_reg)[names(eng_age_reg)=="Age class (years)"]<-"AgeClass"
colnames(eng_age_reg)

#Create region column
a<-rep("NW England",times=10)
b<-rep("NE England",times=9)
c<-rep("Yorkshire and Humber",times=9)
d<-rep("E Midlands",times=9)
e<-rep("E England",times=9)
f<-rep("SE and London",times=9)
g<-rep("SW England",times=9)
h<-rep("W Midlands",times=9)
regions<-c(a,b,c,d,e,f,g,h)
regions
eng_age_reg$Region<-regions
#Remove irrelevant columns and rows
eng_age_reg<-eng_age_reg[-c(1,2,10,11,19,20,28,29,37,38,46,47,55,56,64,65,73),][,-4]
eng_age_reg<-eng_age_reg[c(5,1,2,3,4)]
#Specify columns as numeric
sapply(eng_age_reg,class)
eng_age_reg$Total<-as.numeric(eng_age_reg$Total)
eng_age_reg$Private<-as.numeric(eng_age_reg$Private)
eng_age_reg$FC<-as.numeric(eng_age_reg$FC)
eng_age_reg$AgeClass<-as.factor(eng_age_reg$AgeClass)
eng_age_reg$AgeClass<-factor(eng_age_reg$AgeClass,levels=c("0-10","11-20","21-40","41-60","61-80","81-100","100+"))
sapply(eng_age_reg,class)
#Calculate Shannon diversity on total broadleaves
#Create dataframe with only total trees
age_reg_total<-eng_age_reg[,-c(3,4)]
sapply(age_reg_total,class)
#Restructure data using the Reshape Package
melt<-melt(age_reg_total,id=(c("Region","AgeClass")))
age_reg_total<-cast(melt,Region~variable+AgeClass)
#Rename columns to remove "Total"
colnames(age_reg_total)<-sub("Total_","",colnames(age_reg_total))
colnames(age_reg_total)
#Reorder rows in NFI data order
age_reg_total<-age_reg_total[c(4,3,8,2,1,5,6,7),]
#Remove the Region column
age_reg_total<-age_reg_total[,-1]
#Set as data matrix
age_reg_total<-data.matrix(age_reg_total)
#Calculate Shannon H
age_reg_H<-diversity(age_reg_total)
age_reg_H
#Create table showing region and Shannon H
regions<-c("NW England","NE England","Yorkshire and Humber","E Midlands","E England","SE and London","SW England","W Midlands")
regions
age_reg_H<-data.frame(regions,age_reg_H)
#Rename columns
names(age_reg_H)[names(age_reg_H)=="age_reg_H"]<-"Shannon"
#Visualise as barplot with rescaled y axis to show small differences
ggplot(data=age_reg_H,aes(x=regions,y=Shannon))+
  geom_bar(stat="identity",width=0.75,colour="black",fill="grey87")+
  labs(y="Shannon H")+
  coord_cartesian(ylim=c(1.4,1.7))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=45,hjust=1))

########################################
##Stocked area data for both broadleaves and conifer species
##Using Ch1_Woodland_FS2019.xlsx dataset - Forestry Statistics 2019 from NFI
##Values are area in thousand hectares
##Conifers
age_area_con<-read_excel("J:/GISprojects/Ecosystems Analysis/FPPH/THRS indicators/Data/National Forest Inventory England/Inventory reports/Ch1_Woodland_FS2019.xlsx",sheet="Table 1.6",range="A5:C29")
#Tidy it up
age_area_con<-age_area_con[-c(1,8,9,16,17,24),][,-1]
names(age_area_con)[names(age_area_con)=="...2"]<-"AgeClass"
names(age_area_con)[names(age_area_con)=="England"]<-"Area"
age_area_con$Ownership<-NA
FC<-rep("FC",times=6)
private<-rep("Private",times=6)
total<-rep("Total",times=6)
age_area_con$Ownership<-c(FC,private,total)
age_area_con<-age_area_con[c(3,1,2)]
#Dataframe of total woodland area
age_area_con<-filter(age_area_con,Ownership %in% c("Total"))
age_area_con<-age_area_con[,-1]

#Pie chart for conifers England
#Ensure age classes are factor in correct order
sapply(age_area_con,class)
age_area_con$AgeClass<-as.factor(age_area_con$AgeClass)
age_area_con$AgeClass<-factor(age_area_con$AgeClass,levels=c("0-20","21-40","41-60","61-80","81-100","100+"))
ggplot(age_area_con,aes(x="",y=Area,fill=AgeClass))+
  geom_bar(width=1,stat="identity")+
  coord_polar("y",start=0)+
  scale_fill_brewer(palette="GnBu")+
  theme_void()

############################################
##Broadleaves
age_area_bl<-read_excel("J:/GISprojects/Ecosystems Analysis/FPPH/THRS indicators/Data/National Forest Inventory England/Inventory reports/Ch1_Woodland_FS2019.xlsx",sheet="Table 1.7",range="A5:C29")
#Tidy it up
age_area_bl<-age_area_bl[-c(1,8,9,16,17,24),][,-1]
names(age_area_bl)[names(age_area_bl)=="...2"]<-"AgeClass"
names(age_area_bl)[names(age_area_bl)=="England"]<-"Area"
age_area_bl$Ownership<-NA
FC<-rep("FC",times=6)
private<-rep("Private",times=6)
total<-rep("Total",times=6)
age_area_bl$Ownership<-c(FC,private,total)
age_area_bl<-age_area_bl[c(3,1,2)]
#Dataframe of total woodland area
age_area_bl<-filter(age_area_bl,Ownership %in% c("Total"))
age_area_bl<-age_area_bl[,-1]
#Pie chart for broadleaves England
sapply(age_area_bl,class)
age_area_bl$AgeClass<-as.factor(age_area_bl$AgeClass)
age_area_bl$AgeClass<-factor(age_area_bl$AgeClass,levels=c("0-20","21-40","41-60","61-80","81-100","100+"))
ggplot(age_area_bl,aes(x="",y=Area,fill=AgeClass))+
  geom_bar(width=1,stat="identity")+
  coord_polar("y",start=0)+
  scale_fill_brewer(palette="GnBu")+
  theme_void()

##########################################
#All trees
#Combine to one dataframe with conifers and broadleaves
age_all<-cbind(age_area_con,age_area_bl)
age_all<-age_all[,-3]
colnames(age_all)
names(age_all)[names(age_all)=="Area"]<-"AreaCon"
names(age_all)[names(age_all)=="Area.1"]<-"AreaBL"
sapply(age_all,class)
#Sum total area of trees of each age class
age_all$AreaTotal<-rowSums(age_all[,2:3])
#Create mock dataset with AreaTotal as starting point 
age_all_mock<-matrix(c(276,278,278,279,280,
                       324,324,323,323,322,
                       280,280,282,281,282,
                       161,160,160,159,159,
                       104,104,103,102,102,
                       64,64,65,64,64),
                     nrow=5,
                     dimnames=list(c("2019","2020","2021","2022","2023"),
                                   c("0-20","21-40","41-60","61-80","81-100","100+")))
age_all_div<-diversity(age_all_mock)
years<-c("2019","2020","2021","2022","2023")
age_all_div<-data.frame(years,age_all_div)
names(age_all_div)[names(age_all_div)=="age_all_div"]<-"Shannon"
sapply(age_all_div,class)
#Plot as bar chart
ggplot(data=age_all_div,aes(x=years,y=Shannon))+
  geom_bar(stat="identity",width=0.75,colour="black",fill="grey87")+
  labs(x="",y="Shannon H")+
  coord_cartesian(ylim=c(1.65,1.7))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=50,hjust=1))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y=element_text(colour="black"))

#Create barplot showing the total area for each age class woodland type
#Create dataframe with woodland information
age_woodland<-rbind(age_area_con,age_area_bl)
con<-rep("Conifers",times=6)
bl<-rep("Broadleaves",times=6)
age_woodland$Woodland<-c(con,bl)
ggplot(data=age_woodland,aes(x=AgeClass,y=Area,fill=Woodland))+
  labs(y="Total Area (thousand ha)")+
  geom_bar(stat="identity",position="dodge")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=60,hjust=1))+
  theme(axis.title.x=element_blank())

#########################################
##Calculate Shannon diversity for age class and broadleaves species for England
##Using "NFI_GB_Report_BL_StandingVolume_by_AgeClass" data for standing volume of broadleaved species (thousand m3)
age_spec<-read_excel("J:/GISprojects/Ecosystems Analysis/FPPH/THRS indicators/Data/National Forest Inventory England/Inventory reports/NFI_GB_Report_BL_StandingVolume_by_AgeClass.xls",sheet="Table 2 - BL by Country and GB",range="A3:N14")
##Neaten dataset
age_spec<-age_spec[-c(1,2,3,11),][,-c(2,3)]
names(age_spec)[names(age_spec)=="...1"]<-"AgeClass"
#Specify columns as numeric/factor
sapply(age_spec,class)
species<-c("Oak","Beech","Sycamore","Ash","Birches","Sweet chestnut","Hazel","Hawthorn","Alder","Willow","Other broadleaves")
age_spec[species]<-sapply(age_spec[species],as.numeric)
age_spec$AgeClass<-as.factor(age_spec$AgeClass)
age_spec$AgeClass<-gsub(" years","",age_spec$AgeClass)
age_spec$AgeClass<-factor(age_spec$AgeClass,levels=c("0-10","11-20","21-40","41-60","61-80","81-100","100+"))
sapply(age_spec,class)
#Remember age classes
age<-age_spec$AgeClass
#Transpose dataset without AgeClass column
age_spec_t<-as.data.frame(t(age_spec[,-1]))
#Set column names
colnames(age_spec_t)<-age
#Check column classes
sapply(age_spec_t,class)
#Calculate Shannon 
age_spec_H<-diversity(age_spec_t)
age_spec_H<-data.frame(species,age_spec_H)
names(age_spec_H)[names(age_spec_H)=="age_spec_H"]<-"Shannon"
#Visualise as barplot with rescaled y axis to show small differences
ggplot(data=age_spec_H,aes(x=species,y=Shannon))+
  geom_bar(stat="identity",width=0.75,colour="black",fill="grey87")+
  labs(x="",y="Shannon H")+
  coord_cartesian(ylim=c(1.3,1.7))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=45,hjust=1))

