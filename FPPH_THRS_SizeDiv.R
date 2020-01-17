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
#Neaten it up and remove irrelevant data
eng_bl<-eng_bl[-c(1,2,12),][,-4]
#Total number of bl trees data
bl_total<-eng_bl[,c(1,4)]
names(bl_total)[names(bl_total)=="Mean stand DBH (cm)"]<-"DBH"
sapply(bl_total,class)
bl_total$DBH<-as.factor(bl_total$DBH)
bl_total$Total<-as.numeric(bl_total$Total)
