##Calculate Shannon diversity for broadleaves and conifers separately
##Following on from the DataPrep script
##Date created 2020-01-09

library(readxl)
library(tidyr)
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape)
library(tibble)
library(vegan)

##BROADLEAVES DATA
#Remove FC and Private columns as not used in the Total count analyses
eng_bl<-eng_bl[c(1,2,5)]
#Remove "All broadleaves" rows 
eng_bl<-eng_bl[!(eng_bl$Species=="All broadleaves"),]
#Change Column names
names(eng_bl)[names(eng_bl)=="Principal species"]<-"Species"
colnames(eng_bl)

#Restructure data using the Reshape Package
melt<-melt(eng_bl,id=(c("Region","Species")))
bl_regions<-cast(melt,Region~variable+Species)

#Reorder columns to match NFI data 
bl_regions<-as_tibble(bl_regions)
colnames(bl_regions)
bl_regions<-bl_regions[,c(1,8,4,11,3,5,10,7,6,2,12,9)]
#Reorder rows to match NFI data
bl_regions<-bl_regions[c(4,3,8,2,1,5,6,7),]
#Rename columns to remove "Total"
colnames(bl_regions)<-sub("Total_","",colnames(bl_regions))
colnames(bl_regions)

#List the regions
regions<-c("NW England","NE England","Yorkshire and Humber","E Midlands","E England","SE and London","SW England","W Midlands")
# Calculate Shannon for England regions
#Remove the Region column
bl_regions_div<-bl_regions[,-1]
bl_regions_div<-data.matrix(bl_regions_div)
bl_regions_H<-diversity(bl_regions_div)
bl_regions_H
#Create table showing region and Shannon H
div_bl<-data.frame(regions,bl_regions_H)
#Rename columns
names(div_bl)[names(div_bl)=="bl_regions_H"]<-"Shannon"
names(div_bl)[names(div_bl)=="regions"]<-"Regions"

##IF NEEDED TABLE THE OTHER WAY AROUND##
#Change orientation of table for visualisation purposes
#Transpose dataset
bl_regions_species<-t(bl_regions)
#Set first row (Region) as column names
colnames(bl_regions_species)=bl_regions_species[1,]
bl_regions_species<-bl_regions_species[-1,]
#Set row names as first column
bl_regions_species<-as.data.frame(bl_regions_species)
bl_regions_species<-bl_regions_species%>%rownames_to_column("Species")

##CONIFERS DATA##
#Remove FC and Private columns as not used in the Total count analyses
eng_con<-eng_con[c(1,2,5)]
#Remove "All conifers" rows 
eng_con<-eng_con[!(eng_con$Species=="All conifers"),]
#Change Column names
names(eng_con)[names(eng_con)=="Principal species"]<-"Species"
colnames(eng_bl)

#Restructure data using the Reshape Package
melt<-melt(eng_con,id=(c("Region","Species")))
con_regions<-cast(melt,Region~variable+Species)

#Reorder columns to match NFI data 
con_regions<-as_tibble(con_regions)
colnames(con_regions)
con_regions<-con_regions[,c(1,9,8,2,6,4,3,5,7)]
#Reorder rows to match NFI data
con_regions<-con_regions[c(4,3,8,2,1,5,6,7),]
#Rename columns to remove "Total"
colnames(con_regions)<-sub("Total_","",colnames(con_regions))
colnames(con_regions)

#List the regions
regions<-c("NW England","NE England","Yorkshire and Humber","E Midlands","E England","SE and London","SW England","W Midlands")
# Calculate Shannon for England regions
#Remove the Region column
con_regions_div<-con_regions[,-1]
con_regions_div<-data.matrix(con_regions_div)
con_regions_H<-diversity(con_regions_div)
con_regions_H
#Create table showing region and Shannon H
div_con<-data.frame(regions,con_regions_H)
#Rename columns
names(div_con)[names(div_con)=="con_regions_H"]<-"Shannon"
names(div_con)[names(div_con)=="regions"]<-"Regions"

##IF NEEDED TABLE THE OTHER WAY AROUND##
#Change orientation of table for visualisation purposes
#Transpose dataset
con_regions_species<-t(con_regions)
#Set first row (Region) as column names
colnames(con_regions_species)=con_regions_species[1,]
con_regions_species<-con_regions_species[-1,]
#Set row names as first column
con_regions_species<-as.data.frame(con_regions_species)
con_regions_species<-con_regions_species%>%rownames_to_column("Species")

##Plot the Shannon H for each region and each woodland type
#Add woodland type columns to the data frame
div_bl$Woodland<-"Broadleaves"
div_con$Woodland<-"Conifers"
#Combine to form one data frame
div_all<-rbind(div_bl,div_con)
#Create barplot showing the Shannon H for each region and woodland type
ggplot(data=div_all,aes(x=Regions,y=Shannon,fill=Woodland))+
  labs(y="Shannon H")+
  geom_bar(stat="identity",position="dodge")+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=60,hjust=1))+
  theme(axis.title.x=element_blank())
