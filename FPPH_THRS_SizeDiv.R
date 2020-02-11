##Script Name:
##Author: Izzy Hassall, Ecosystems Analysis
##Contact (if different from above):
##Date Created: 2020-01-17
##Date Modified:
##Licence:
##Abstract:


##R version 3.6.1 (2019-07-05)
##Dependencies:
library(readxl)
library(tidyr)
library(ggplot2)
library(plyr)
library(dplyr)
library(vegan)
library(reshape)

##Data Used:"NFI_Prelim_BL_Ash_Tables"

##Calculate structural diversity metrics based on count data of dbh classes in broadleaves
eng_bl<-read_excel("J:/GISprojects/Ecosystems Analysis/FPPH/THRS indicators/Data/National Forest Inventory England/Inventory reports/NFI_Prelim_BL_Ash_Tables.xls",sheet="Table 12",range="B5:F17")
#Data is in number of trees (millions) and DBH classesare in cm
#Neaten it up and remove irrelevant data
eng_bl<-eng_bl[-c(1,2,12),][,-4]
#Total number of bl trees data
bl_total<-eng_bl[,c(1,4)]
names(bl_total)[names(bl_total)=="Mean stand DBH (cm)"]<-"DBH"
sapply(bl_total,class)
bl_total$DBH<-as.factor(bl_total$DBH)
bl_total$Total<-as.numeric(bl_total$Total)

#Create mock dataset to illustrate change in size diversity over time
#In number of trees (millions)
#Starting point the Total column of Table 12 in "NFI_Prelim_BL_Ash_Tables" 
size_bl_mock<-matrix(c(48.59479958,395.3548691,256.7828136,102.9338022,101.3696618,40.3541177,24.32063591,5.40594848,1.9446781,
                       48.1262,396.00124,256.1241,102.24125,101.67845,40.8909,25.00516,5.38512,1.7584,
                       47.5691,396.2352,256.1578,101.9568,101.8951,41.52162,24.67414,5.35124,1.7492,
                       47.1245,396.6712,256.72352,101.8752,101.1242,41.7856,24.9074,5.1234,1.7411,
                       47.4568,396.8945,256.8925,101.2356,100.9856,41.9085,25.167,5.42566,1.75814,
                       47.7536,396.4125,256.5213,101.5623,100.8725,41.8056,24.8075,5.2914,1.74995,
                       47.3415,396.1125,256.3152,101.9671,101.3571,41.5142,24.5262,5.7864,1.73981,
                       47.6781,396.4512,256.8971,102.2452,101.8976,41.6125,24.7862,5.8971,1.7984),
                    nrow=9,
                    dimnames=list(c("0-7","7-10","10-15","15-20","20-30","30-40","40-60","60-80","80+"),
                                  c("2009-2014","2015-2020","2021-2025","2026-2030","2031-2035","2036-2040","2041-2045","2046-2050")))
#Calculate Shannon diversity
blocks<-c("2009-2014","2015-2020","2021-2025","2026-2030","2031-2035","2036-2040","2041-2045","2046-2050")
size_div<-t(size_bl_mock)
size_div<-diversity(size_div)
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
ggplot(data=size_div,aes(x=blocks,y=ENS))+
  geom_bar(stat="identity",width=0.75,colour="black",fill="grey87")+
  labs(x="",y="Effective Number of DBH Classes")+
  coord_cartesian(ylim=c(4.9,5.0))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=50,hjust=1))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y=element_text(colour="black"))


#########################################################################
#Stacked bar plots over time
#Create new mock dataset with bigger changes
bl_mock<-matrix(c(48.59,395.35,256.78,102.93,101.36,40.35,24.32,5.40,1.94,
                       48.12,398.00,259.12,107.24,101.67,40.89,20.00,5.38,1.75,
                       52.56,400.23,245.15,110.95,105.89,41.52,24.67,6.24,2.00,
                       55.12,410.67,256.72,101.87,101.12,35.78,24.90,5.12,1.74,
                       47.45,396.89,256.89,106.23,100.98,41.90,25.16,5.42,1.75,
                       42.75,390.41,245.52,101.56,104.87,41.80,24.80,5.29,1.74,
                       47.34,384.11,234.31,105.96,106.35,41.51,24.52,5.78,1.73,
                       50.61,390.45,240.89,102.24,101.89,41.61,24.78,5.89,1.79),
                     nrow=9,
                     dimnames=list(c("0-7","7-10","10-15","15-20","20-30","30-40","40-60","60-80","80+"),
                                   c("2009-2014","2015-2020","2021-2025","2026-2030","2031-2035","2036-2040","2041-2045","2046-2050")))
bl_mock_melt<-melt(bl_mock)
colnames(bl_mock_melt)<-c("DBHClass","Block","Total")
bl_mock_melt<-as.data.frame(bl_mock_melt)
sapply(bl_mock_melt,class)
bl_mock_melt$DBHClass<-factor(bl_mock_melt$DBHClass,levels=c("0-7","7-10","10-15","15-20","20-30","30-40","40-60","60-80","80+"))
ggplot(data=bl_mock_melt,aes(x=Block,y=Total,fill=DBHClass))+
  labs(y="Number of trees (millions)")+
  geom_bar(stat="identity")+
  scale_fill_brewer(palette="Greens")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=60,hjust=1))+
  theme(axis.title.x=element_blank())

##########################################################################
#Look at regional differences as snapshot 
##SHANNON DIVERSITY FOR REGIONS
eng_size_reg<-read_excel("J:/GISprojects/Ecosystems Analysis/FPPH/THRS indicators/Data/National Forest Inventory England/Inventory reports/NFI_Prelim_BL_Ash_Tables.xls",sheet="Table A.9",range="B5:F94")
#Data is in number of trees (thousands)
#Change Column names
names(eng_size_reg)[names(eng_size_reg)=="Mean stand DBH (cm)"]<-"DBH"
names(eng_size_reg)[names(eng_size_reg)=="Private sector"]<-"Private"

#Create region column
a<-rep("NW England",times=12)
b<-rep("NE England",times=11)
c<-rep("Yorkshire and Humber",times=11)
d<-rep("E Midlands",times=11)
e<-rep("E England",times=11)
f<-rep("SE and London",times=11)
g<-rep("SW England",times=11)
h<-rep("W Midlands",times=11)
regions<-c(a,b,c,d,e,f,g,h)
regions
eng_size_reg$Region<-regions
#Remove irrelevant columns and rows
eng_size_reg<-eng_size_reg[-c(1,2,12,13,23,24,34,35,45,46,56,57,67,68,78,79,89),][,-4]
eng_size_reg<-eng_size_reg[c(5,1,2,3,4)]
#Specify columns as numeric
sapply(eng_size_reg,class)
eng_size_reg$Total<-as.numeric(eng_size_reg$Total)
eng_size_reg$Private<-as.numeric(eng_size_reg$Private)
eng_size_reg$FC<-as.numeric(eng_size_reg$FC)
eng_size_reg$DBH<-as.factor(eng_size_reg$DBH)
eng_size_reg$DBH<-factor(eng_size_reg$DBH,levels=c("0-7","7-10","10-15","15-20","20-30","30-40","40-60","60-80","80+"))
sapply(eng_size_reg,class)
#Calculate Shannon diversity on total broadleaves
#Create dataframe with only total trees
size_reg_total<-eng_size_reg[,-c(3,4)]
sapply(size_reg_total,class)

#Restructure data using the Reshape Package
melt<-melt(size_reg_total,id=(c("Region","DBH")))
size_reg_total<-cast(melt,Region~variable+DBH)

#Rename columns to remove "Total"
colnames(size_reg_total)<-sub("Total_","",colnames(size_reg_total))
colnames(size_reg_total)

#Reorder rows in NFI data order
size_reg_total<-size_reg_total[c(4,3,8,2,1,5,6,7),]

#Remove the Region column
size_reg_total<-size_reg_total[,-1]

#Set as data matrix
size_reg_total<-data.matrix(size_reg_total)

#Calculate Shannon H
size_reg_H<-diversity(size_reg_total)
size_reg_H
#Create table showing region and Shannon H
regions<-c("NW England","NE England","Yorkshire and Humber","E Midlands","E England","SE and London","SW England","W Midlands")
regions
size_reg_H<-data.frame(regions,size_reg_H)
#Rename columns
names(size_reg_H)[names(size_reg_H)=="size_reg_H"]<-"Shannon"
#Visualise as barplot with rescaled y axis to show small differences
ggplot(data=size_reg_H,aes(x=regions,y=Shannon))+
  geom_bar(stat="identity",width=0.75,colour="black",fill="grey87")+
  labs(y="Shannon H")+
  coord_cartesian(ylim=c(1.5,1.8))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=45,hjust=1))

