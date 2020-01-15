###Age Diversity###
##Date created: 2020-01-15
##Datasets used:"NFI_Prelim_BL_Ash_Tables" from NFI

library(readxl)
library(tidyr)
library(ggplot2)
library(plyr)
library(dplyr)
library(vegan)

#########################################
##READ IN AND ARRANGE NATIONAL DATASET##
##BROADLEAVES##
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
bl_age_total
#Calculate Shannon diversity on age data
bl_age_div<-diversity(bl_age_total)
bl_age_div

#Create mock dataset to illustrate change in age diversity over time
#In number of trees (millions)
#Starting point the Total column of Table 11 in "NFI_Prelim_BL_Ash_Tables" 
age_mock<-matrix(c(21.38185214,22.03419,22.4561,22.1245,22.98512,23.1431,22.8791,23.4512,
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
age_div<-diversity(age_mock)
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
