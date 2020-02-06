##Script Name:
##Author: Izzy Hassall, Ecosystems Analysis
##Contact (if different from above):
##Date Created: 2020-02-04
##Date Modified:
##Licence:
##Abstract:


##R version 3.6.1 (2019-07-05)
##Dependencies:
library(sf)
library(rgdal)
library(dplyr)
library(ggplot2)
library(reshape2)

scdb<-st_read("J:/GISprojects/Ecosystems Analysis/FPPH/THRS indicators/Data/SCDB/National_Forest_Estate_Subcompartments_England_2019/National_Forest_Estate_Subcompartments_England_2019.shp")
reg<-st_read("J:/GISprojects/Ecosystems Analysis/FPPH/THRS indicators/Data/NUTS/Download_10.01.2020/NUTS_Level_1_January_2018_Full_Clipped_Boundaries_in_the_United_Kingdom/NUTS_Level_1_January_2018_Full_Clipped_Boundaries_in_the_United_Kingdom.shp")
class(scdb)
class(reg)
attr(scdb,"sf_column")
attr(reg,"sf_column")
#Print first three features
print(scdb[1:40],n=3)
print(reg[1:10],n=3)

##############################################################
#Use BLOCK attribute to generate diversity per forest block/stand
#Create smaller dataset with fewer variables
colnames(scdb)
scdb_2<-scdb%>%select("BLOCK","PRISPECIES","Shape__Are")
sapply(scdb_2,class)
class(scdb_2)
#Remove geometry data so just df
st_geometry(scdb_2)<-NULL
class(scdb_2)
sapply(scdb_2,class)
scdb_2$BLOCK<-as.factor(scdb_2$BLOCK)
#Remove NA data entries
scdb_spec<-na.omit(scdb_2)
#Restructure data frame
melt<-melt(scdb_spec,id=c("BLOCK","PRISPECIES"))
scdb_spec<-dcast(melt,BLOCK~PRISPECIES,sum)

#Summary for data table#
new<-dcast(melt,BLOCK+PRISPECIES~variable,sum)

#Generate Shannon Diversity on blocks
div<-diversity(scdb_spec[,2:103])
blocks<-scdb_spec$BLOCK
block_H<-data.frame(blocks,div)

#Mean and SD of block diversity for England
mean(block_H$div)
sd(block_H$div)

##############################################################
#Calculate Shannon H per compartment (9900)
#DF for compartments
colnames(scdb)
scdb_3<-scdb%>%select("COMPTMENT","BLOCK","PRISPECIES","Shape__Are")
sapply(scdb_3,class)
class(scdb_3)
#Remove geometry data so just df
st_geometry(scdb_3)<-NULL
class(scdb_3)
sapply(scdb_3,class)
scdb_3$COMPTMENT<-as.factor(scdb_3$COMPTMENT)
#Remove NA data entries
comp_spec<-na.omit(scdb_3)
#Restructure data frame
melt<-melt(comp_spec,id=c("COMPTMENT","PRISPECIES"))
comp_spec<-dcast(melt,COMPTMENT~PRISPECIES,sum)
#Generate Shannon Diversity on compartments
div<-diversity(comp_spec[,2:103])
comp<-comp_spec$COMPTMENT
comp_H<-data.frame(comp,div)

#Mean and SD of compartment diversity for England
mean(comp_H$div)
sd(comp_H$div)

##############################################################
##Divide the SCDB dataset into regions using the NUTS dataset
#Create spatial subset of scdb compartments within each NUTS region
nw<-reg%>%filter(nuts118nm=="North West (England)")
sc_nw<-scdb[nw, ,op=st_intersects]

ne<-reg%>%filter(nuts118nm=="North East (England)")
sc_ne<-scdb[ne, ,op=st_intersects]

yh<-reg%>%filter(nuts118nm=="Yorkshire and The Humber")
sc_yh<-scdb[yh, ,op=st_intersects]

em<-reg%>%filter(nuts118nm=="East Midlands (England)")
sc_em<-scdb[em, ,op=st_intersects]

ee<-reg%>%filter(nuts118nm=="East of England")
sc_ee<-scdb[ee, ,op=st_intersects]

sel<-reg%>%filter(nuts118nm%in%c("South East (England)","London"))
sc_sel<-scdb[sel, ,op=st_intersects]

sw<-reg%>%filter(nuts118nm=="South West (England)")
sc_sw<-scdb[sw, ,op=st_intersects]

wm<-reg%>%filter(nuts118nm=="West Midlands (England)")
sc_wm<-scdb[wm, ,op=st_intersects]

#Generate diversity of compartments per region
#NW
nw_comp<-sc_nw%>%select("COMPTMENT","BLOCK","PRISPECIES","Shape__Are")
sapply(nw_comp,class)
#Remove geometry data so just df
st_geometry(nw_comp)<-NULL
nw_comp$COMPTMENT<-as.factor(nw_comp$COMPTMENT)
#Remove NA data entries
nw_comp<-na.omit(nw_comp)
#Restructure data frame
melt<-melt(nw_comp,id=c("COMPTMENT","PRISPECIES"))
nw_comp<-dcast(melt,COMPTMENT~PRISPECIES,sum)
#Generate Shannon Diversity on compartments
div<-diversity(nw_comp[,2:60])
nw_comp<-nw_comp$COMPTMENT
nw_comp_H<-data.frame(nw_comp,div)
#Mean and SD of compartment diversity for NW
mean(nw_comp_H$div)
sd(nw_comp_H$div)

#NE
ne_comp<-sc_ne%>%select("COMPTMENT","BLOCK","PRISPECIES","Shape__Are")
sapply(ne_comp,class)
#Remove geometry data so just df
st_geometry(ne_comp)<-NULL
ne_comp$COMPTMENT<-as.factor(ne_comp$COMPTMENT)
#Remove NA data entries
ne_comp<-na.omit(ne_comp)
#Restructure data frame
melt<-melt(ne_comp,id=c("COMPTMENT","PRISPECIES"))
ne_comp<-dcast(melt,COMPTMENT~PRISPECIES,sum)
#Generate Shannon Diversity on compartments
div<-diversity(ne_comp[,2:47])
ne_comp<-ne_comp$COMPTMENT
ne_comp_H<-data.frame(ne_comp,div)
#Mean and SD of compartment diversity for NE
mean(ne_comp_H$div)
sd(ne_comp_H$div)

#YH
yh_comp<-sc_yh%>%select("COMPTMENT","BLOCK","PRISPECIES","Shape__Are")
sapply(yh_comp,class)
#Remove geometry data so just df
st_geometry(yh_comp)<-NULL
yh_comp$COMPTMENT<-as.factor(yh_comp$COMPTMENT)
#Remove NA data entries
yh_comp<-na.omit(yh_comp)
#Restructure data frame
melt<-melt(yh_comp,id=c("COMPTMENT","PRISPECIES"))
yh_comp<-dcast(melt,COMPTMENT~PRISPECIES,sum)
#Generate Shannon Diversity on compartments
div<-diversity(yh_comp[,2:57])
yh_comp<-yh_comp$COMPTMENT
yh_comp_H<-data.frame(yh_comp,div)
#Mean and SD of compartment diversity for YH
mean(yh_comp_H$div)
sd(yh_comp_H$div)

#EM
em_comp<-sc_nw%>%select("COMPTMENT","BLOCK","PRISPECIES","Shape__Are")
sapply(em_comp,class)
#Remove geometry data so just df
st_geometry(em_comp)<-NULL
em_comp$COMPTMENT<-as.factor(em_comp$COMPTMENT)
#Remove NA data entries
em_comp<-na.omit(em_comp)
#Restructure data frame
melt<-melt(em_comp,id=c("COMPTMENT","PRISPECIES"))
em_comp<-dcast(melt,COMPTMENT~PRISPECIES,sum)
#Generate Shannon Diversity on compartments
div<-diversity(em_comp[,2:60])
em_comp<-em_comp$COMPTMENT
em_comp_H<-data.frame(em_comp,div)
#Mean and SD of compartment diversity for EM
mean(em_comp_H$div)
sd(em_comp_H$div)

#EE
ee_comp<-sc_ee%>%select("COMPTMENT","BLOCK","PRISPECIES","Shape__Are")
sapply(ee_comp,class)
#Remove geometry data so just df
st_geometry(ee_comp)<-NULL
ee_comp$COMPTMENT<-as.factor(ee_comp$COMPTMENT)
#Remove NA data entries
ee_comp<-na.omit(ee_comp)
#Restructure data frame
melt<-melt(ee_comp,id=c("COMPTMENT","PRISPECIES"))
ee_comp<-dcast(melt,COMPTMENT~PRISPECIES,sum)
#Generate Shannon Diversity on compartments
div<-diversity(ee_comp[,2:75])
ee_comp<-ee_comp$COMPTMENT
ee_comp_H<-data.frame(ee_comp,div)
#Mean and SD of compartment diversity for EE
mean(ee_comp_H$div)
sd(ee_comp_H$div)

#SEL
sel_comp<-sc_sel%>%select("COMPTMENT","BLOCK","PRISPECIES","Shape__Are")
sapply(sel_comp,class)
#Remove geometry data so just df
st_geometry(sel_comp)<-NULL
sel_comp$COMPTMENT<-as.factor(sel_comp$COMPTMENT)
#Remove NA data entries
sel_comp<-na.omit(sel_comp)
#Restructure data frame
melt<-melt(sel_comp,id=c("COMPTMENT","PRISPECIES"))
sel_comp<-dcast(melt,COMPTMENT~PRISPECIES,sum)
#Generate Shannon Diversity on compartments
div<-diversity(sel_comp[,2:63])
sel_comp<-sel_comp$COMPTMENT
sel_comp_H<-data.frame(sel_comp,div)
#Mean and SD of compartment diversity for SEL
mean(sel_comp_H$div)
sd(sel_comp_H$div)

#SW
sw_comp<-sc_sw%>%select("COMPTMENT","BLOCK","PRISPECIES","Shape__Are")
sapply(sw_comp,class)
#Remove geometry data so just df
st_geometry(sw_comp)<-NULL
sw_comp$COMPTMENT<-as.factor(sw_comp$COMPTMENT)
#Remove NA data entries
sw_comp<-na.omit(sw_comp)
#Restructure data frame
melt<-melt(sw_comp,id=c("COMPTMENT","PRISPECIES"))
sw_comp<-dcast(melt,COMPTMENT~PRISPECIES,sum)
#Generate Shannon Diversity on compartments
div<-diversity(sw_comp[,2:75])
sw_comp<-sw_comp$COMPTMENT
sw_comp_H<-data.frame(sw_comp,div)
#Mean and SD of compartment diversity for SW
mean(sw_comp_H$div)
sd(sw_comp_H$div)

#WM
wm_comp<-sc_wm%>%select("COMPTMENT","BLOCK","PRISPECIES","Shape__Are")
sapply(wm_comp,class)
#Remove geometry data so just df
st_geometry(wm_comp)<-NULL
wm_comp$COMPTMENT<-as.factor(wm_comp$COMPTMENT)
#Remove NA data entries
wm_comp<-na.omit(wm_comp)
#Restructure data frame
melt<-melt(wm_comp,id=c("COMPTMENT","PRISPECIES"))
wm_comp<-dcast(melt,COMPTMENT~PRISPECIES,sum)
#Generate Shannon Diversity on compartments
div<-diversity(wm_comp[,2:56])
wm_comp<-wm_comp$COMPTMENT
wm_comp_H<-data.frame(wm_comp,div)
#Mean and SD of compartment diversity for WM
mean(wm_comp_H$div)
sd(wm_comp_H$div)

#Create DF of mean and SD of compartment diversity per region
reg<-c("NW","NE","YH","EM","EE","SEL","SW","WM")
nw<-data.frame("Mean"=mean(nw_comp_H$div),"SD"=sd(nw_comp_H$div))
ne<-data.frame("Mean"=mean(ne_comp_H$div),"SD"=sd(ne_comp_H$div))
yh<-data.frame("Mean"=mean(yh_comp_H$div),"SD"=sd(yh_comp_H$div))
em<-data.frame("Mean"=mean(em_comp_H$div),"SD"=sd(em_comp_H$div))
ee<-data.frame("Mean"=mean(ee_comp_H$div),"SD"=sd(ee_comp_H$div))
sel<-data.frame("Mean"=mean(sel_comp_H$div),"SD"=sd(sel_comp_H$div))
sw<-data.frame("Mean"=mean(sw_comp_H$div),"SD"=sd(sw_comp_H$div))
wm<-data.frame("Mean"=mean(wm_comp_H$div),"SD"=sd(wm_comp_H$div))
comp_reg<-rbind(nw,ne,yh,em,ee,sel,sw,wm)
comp_reg$Region<-factor(reg,levels=c("NW","NE","YH","EM","EE","SEL","SW","WM"))

ggplot(comp_reg,aes(x=Region,y=Mean))+
  geom_bar(stat="identity",fill="grey")+
  geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),width=0.2)+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("")+
  ylab("Mean Compartment Shannon Diversity")

#############################################################
#Generate diversity of blocks per region
#NW
nw_block<-sc_nw%>%select("BLOCK","PRISPECIES","Shape__Are")
sapply(nw_block,class)
#Remove geometry data so just df
st_geometry(nw_block)<-NULL
nw_block$BLOCK<-as.factor(nw_block$BLOCK)
#Remove NA data entries
nw_block<-na.omit(nw_block)
#Restructure data frame
melt<-melt(nw_block,id=c("BLOCK","PRISPECIES"))
nw_block<-dcast(melt,BLOCK~PRISPECIES,sum)
#Generate Shannon Diversity on blocks
div<-diversity(nw_block[,2:60])
nw_block_H<-data.frame(nw_block$BLOCK,div)
#Mean and SD of block diversity for NW
mean(nw_block_H$div)
sd(nw_block_H$div)

#NE
ne_block<-sc_ne%>%select("BLOCK","PRISPECIES","Shape__Are")
sapply(ne_block,class)
#Remove geometry data so just df
st_geometry(ne_block)<-NULL
ne_block$BLOCK<-as.factor(ne_block$BLOCK)
#Remove NA data entries
ne_block<-na.omit(ne_block)
#Restructure data frame
melt<-melt(ne_block,id=c("BLOCK","PRISPECIES"))
ne_block<-dcast(melt,BLOCK~PRISPECIES,sum)
#Generate Shannon Diversity on blocks
div<-diversity(ne_block[,2:47])
ne_block_H<-data.frame(ne_block$BLOCK,div)
#Mean and SD of block diversity for NE
mean(ne_block_H$div)
sd(ne_block_H$div)

#YH
yh_block<-sc_yh%>%select("BLOCK","PRISPECIES","Shape__Are")
sapply(yh_block,class)
#Remove geometry data so just df
st_geometry(yh_block)<-NULL
yh_block$BLOCK<-as.factor(yh_block$BLOCK)
#Remove NA data entries
yh_block<-na.omit(yh_block)
#Restructure data frame
melt<-melt(yh_block,id=c("BLOCK","PRISPECIES"))
yh_block<-dcast(melt,BLOCK~PRISPECIES,sum)
#Generate Shannon Diversity on blocks
div<-diversity(yh_block[,2:57])
yh_block_H<-data.frame(yh_block$BLOCK,div)
#Mean and SD of block diversity for YH
mean(yh_block_H$div)
sd(yh_block_H$div)

#EM
em_block<-sc_em%>%select("BLOCK","PRISPECIES","Shape__Are")
sapply(em_block,class)
#Remove geometry data so just df
st_geometry(em_block)<-NULL
em_block$BLOCK<-as.factor(em_block$BLOCK)
#Remove NA data entries
em_block<-na.omit(em_block)
#Restructure data frame
melt<-melt(em_block,id=c("BLOCK","PRISPECIES"))
em_block<-dcast(melt,BLOCK~PRISPECIES,sum)
#Generate Shannon Diversity on blocks
div<-diversity(em_block[,2:60])
em_block_H<-data.frame(em_block$BLOCK,div)
#Mean and SD of block diversity for EM
mean(em_block_H$div)
sd(em_block_H$div)

#EE
ee_block<-sc_ee%>%select("BLOCK","PRISPECIES","Shape__Are")
sapply(ee_block,class)
#Remove geometry data so just df
st_geometry(ee_block)<-NULL
ee_block$BLOCK<-as.factor(ee_block$BLOCK)
#Remove NA data entries
ee_block<-na.omit(ee_block)
#Restructure data frame
melt<-melt(ee_block,id=c("BLOCK","PRISPECIES"))
ee_block<-dcast(melt,BLOCK~PRISPECIES,sum)
#Generate Shannon Diversity on blocks
div<-diversity(ee_block[,2:75])
ee_block_H<-data.frame(ee_block$BLOCK,div)
#Mean and SD of block diversity for EE
mean(ee_block_H$div)
sd(ee_block_H$div)

#SEL
sel_block<-sc_sel%>%select("BLOCK","PRISPECIES","Shape__Are")
sapply(sel_block,class)
#Remove geometry data so just df
st_geometry(sel_block)<-NULL
sel_block$BLOCK<-as.factor(sel_block$BLOCK)
#Remove NA data entries
sel_block<-na.omit(sel_block)
#Restructure data frame
melt<-melt(sel_block,id=c("BLOCK","PRISPECIES"))
sel_block<-dcast(melt,BLOCK~PRISPECIES,sum)
#Generate Shannon Diversity on blocks
div<-diversity(sel_block[,2:63])
sel_block_H<-data.frame(sel_block$BLOCK,div)
#Mean and SD of block diversity for SEL
mean(sel_block_H$div)
sd(sel_block_H$div)

#SW
sw_block<-sc_sw%>%select("BLOCK","PRISPECIES","Shape__Are")
sapply(sw_block,class)
#Remove geometry data so just df
st_geometry(sw_block)<-NULL
sw_block$BLOCK<-as.factor(sw_block$BLOCK)
#Remove NA data entries
sw_block<-na.omit(sw_block)
#Restructure data frame
melt<-melt(sw_block,id=c("BLOCK","PRISPECIES"))
sw_block<-dcast(melt,BLOCK~PRISPECIES,sum)
#Generate Shannon Diversity on blocks
div<-diversity(sw_block[,2:75])
sw_block_H<-data.frame(sw_block$BLOCK,div)
#Mean and SD of block diversity for SW
mean(sw_block_H$div)
sd(sw_block_H$div)

#WM
wm_block<-sc_wm%>%select("BLOCK","PRISPECIES","Shape__Are")
sapply(wm_block,class)
#Remove geometry data so just df
st_geometry(wm_block)<-NULL
wm_block$BLOCK<-as.factor(wm_block$BLOCK)
#Remove NA data entries
wm_block<-na.omit(wm_block)
#Restructure data frame
melt<-melt(wm_block,id=c("BLOCK","PRISPECIES"))
wm_block<-dcast(melt,BLOCK~PRISPECIES,sum)
#Generate Shannon Diversity on blocks
div<-diversity(wm_block[,2:56])
wm_block_H<-data.frame(wm_block$BLOCK,div)
#Mean and SD of block diversity for WM
mean(wm_block_H$div)
sd(wm_block_H$div)

#Create DF of mean and SD of block diversity per region
reg<-c("NW","NE","YH","EM","EE","SEL","SW","WM")
nw<-data.frame("Mean"=mean(nw_block_H$div),"SD"=sd(nw_block_H$div))
ne<-data.frame("Mean"=mean(ne_block_H$div),"SD"=sd(ne_block_H$div))
yh<-data.frame("Mean"=mean(yh_block_H$div),"SD"=sd(yh_block_H$div))
em<-data.frame("Mean"=mean(em_block_H$div),"SD"=sd(em_block_H$div))
ee<-data.frame("Mean"=mean(ee_block_H$div),"SD"=sd(ee_block_H$div))
sel<-data.frame("Mean"=mean(sel_block_H$div),"SD"=sd(sel_block_H$div))
sw<-data.frame("Mean"=mean(sw_block_H$div),"SD"=sd(sw_block_H$div))
wm<-data.frame("Mean"=mean(wm_block_H$div),"SD"=sd(wm_block_H$div))
block_reg<-rbind(nw,ne,yh,em,ee,sel,sw,wm)
block_reg$Region<-factor(reg,levels=c("NW","NE","YH","EM","EE","SEL","SW","WM"))

ggplot(block_reg,aes(x=Region,y=Mean))+
  geom_bar(stat="identity",fill="grey")+
  geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),width=0.2)+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("")+
  ylab("Mean Block Shannon Diversity")

###################################################################
#Could use the SCDB to generate diversity within stands
#Eg compartment diversity within blocks??