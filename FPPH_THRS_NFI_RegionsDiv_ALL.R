##Shannon Diversity Index on ALL species data
##For each region
##Follow on from the FPPH_THRS_NFI_DataPrep script

library(tidyr)
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape)
library(tibble)
library(vegan)

#Use the eng_all_species dataset to calculate Shannon for each region
#Remove Private and FC columns to leave on the Total
eng_all_species<-eng_all_species[,-3,-4]
#Check class of variables
sapply(eng_all_species,class)

#Restructure data using the Reshape Package
melt<-melt(eng_all_species,id=(c("Region","Species")))
all_regions<-cast(melt,Region~variable+Species)

#Rename columns to remove "Total"
colnames(all_regions)<-sub("Total_","",colnames(all_regions))
colnames(all_regions)
#Reorder rows in NFI data order
all_regions<-all_regions[c(4,3,8,2,1,5,6,7),]
#List the regions in NFI data order
regions<-c("NW England","NE England","Yorkshire and Humber","E Midlands","E England","SE and London","SW England","W Midlands")
regions
# Calculate Shannon for England regions
#Remove the Region column
all_regions_div<-all_regions[,-1]
#Set as data matrix
all_regions_div<-data.matrix(all_regions_div)
#Calculate Shannon H
all_regions_H<-diversity(all_regions_div)
all_regions_H
#Create table showing region and Shannon H
div_all<-data.frame(regions,all_regions_H)
#Rename columns
names(div_all)[names(div_all)=="all_regions_H"]<-"Shannon"
names(div_all)[names(div_all)=="regions"]<-"Regions"

#Visualise as barplot with rescaled y axis to show small differences
ggplot(data=div_all,aes(x=Regions,y=Shannon))+
  geom_bar(stat="identity",width=0.75,colour="black",fill="grey87")+
  labs(y="Shannon H")+
  coord_cartesian(ylim=c(2,3))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=45,hjust=1))

