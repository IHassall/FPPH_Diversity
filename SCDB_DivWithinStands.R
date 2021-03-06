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
library(tibble)
library(ggplot2)
library(reshape2)
library(vegan)

scdb<-st_read("J:/GISprojects/Ecosystems Analysis/FPPH/THRS indicators/Data/SCDB/National_Forest_Estate_Subcompartments_England_2019/National_Forest_Estate_Subcompartments_England_2019.shp")
reg<-st_read("J:/GISprojects/Ecosystems Analysis/FPPH/THRS indicators/Data/NUTS/Download_10.01.2020/NUTS_Level_1_January_2018_Full_Clipped_Boundaries_in_the_United_Kingdom/NUTS_Level_1_January_2018_Full_Clipped_Boundaries_in_the_United_Kingdom.shp")
class(scdb)
class(reg)
attr(scdb,"sf_column")
attr(reg,"sf_column")
#Print first three features
print(scdb[1:40],n=3)
print(reg[1:10],n=3)
#Remove any entries not in woodland
levels(scdb$PRILANDUSE)
scdb<-scdb%>%filter(PRILANDUSE%in%c("High Forest","Non-plantation research","Partially Intruded Broadleaf","Research Plantation","Seed Orchard","Seed Stand","Worked Coppice"))
levels(scdb$PRILANDUSE)

print(scdb[1:40],n=3)
#Columns needed: COMPTMENT, BLOCK, PRISPECIES, SECSPECIES, TERSPECIES, PRIPCTAREA,SECPCTAREA, TERPCTAREA, Shape__Are,geometry
scdb<-scdb%>%select("COMPTMENT","BLOCK","PRISPECIES","SECSPECIES","TERSPECIES","PRIPCTAREA","SECPCTAREA","TERPCTAREA","Shape__Are","geometry")
#Could use this dataset to calculate the area per species per compartment (pri, sec and ter)
#Create columns of area of each primary, secondary and tertiary species
scdb<-scdb%>%mutate(Pri=(Shape__Are/100)*PRIPCTAREA)
scdb<-scdb%>%mutate(Sec=(Shape__Are/100)*SECPCTAREA)
scdb<-scdb%>%mutate(Ter=(Shape__Are/100)*TERPCTAREA)
print(scdb[1:13],n=3)

#Remove columns not needed
scdb<-scdb%>%select("COMPTMENT","BLOCK","PRISPECIES","SECSPECIES","TERSPECIES","Pri","Sec","Ter","Shape__Are")
#Remove geometry data so just df
st_geometry(scdb)<-NULL
class(scdb)
sapply(scdb,class)
#Remove NA data entries
scdb<-na.omit(scdb)

##START FROM HERE!!!####
##############################################################
#Use BLOCK attribute to generate diversity per forest block/stand
colnames(scdb)
scdb_spec<-scdb%>%select("BLOCK","PRISPECIES","Shape__Are")
sapply(scdb_spec,class)
#Remove geometry column
st_geometry(scdb_spec)<-NULL
#Restructure data frame
melt<-melt(scdb_spec,id=c("BLOCK","PRISPECIES"))
scdb_spec<-dcast(melt,BLOCK~PRISPECIES,sum)

#Summary for data table#
new<-dcast(melt,BLOCK+PRISPECIES~variable,sum)

#Generate Shannon Diversity on blocks
div<-diversity(scdb_spec[,2:103])
blocks<-scdb_spec$BLOCK
block_H<-data.frame(blocks,div)
#Remove last row as div is 0
block_H<-block_H[-99,]
#Mean and SD of block diversity for England
mean(block_H$div)
sd(block_H$div)

#Plot mean and SD block diversity over time
block_mock<-matrix(c(2.040465,2.022412,2.15723,2.32111,2.56178,2.34167,2.48934,2.73498,2.84529,2.34756,2.12166,2.00465,
                     0.5768735,0.60342,0.62341,0.63676,0.68452,0.667834,0.531211,0.493412,0.365412,0.321231,0.57252,0.57984),
                   nrow=12,
                   dimnames=list(c(2019:2030),
                                 c("Mean","SD")))
block_mock<-as.data.frame(block_mock)
block_mock<-block_mock%>%rownames_to_column("Year")
ggplot(data=block_mock,aes(x=Year,y=Mean))+
  geom_bar(stat="identity",fill="grey")+
  geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),width=0.2)+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("")+
  ylab("Mean Block Shannon Diversity")

#Generate ENS on blocks
block_H$ens<-exp(block_H$div)
mean(block_H$ens)
sd(block_H$ens)

#Convert mock data to ens
ens_mock<-matrix(c(8.762,7.556,8.647,10.186,12.958,10.398,12.053,15.409,17.206,15.460,12.335,14.081,
                     3.627,3.412,2.897,2.789,3.562,4.873,5.341,5.004,5.121,5.623,5.462,5.712),
                   nrow=12,
                   dimnames=list(c(2019:2030),
                                 c("Mean","SD")))
ens_mock<-as.data.frame(ens_mock)
ens_mock<-ens_mock%>%rownames_to_column("Year")
sapply(ens_mock,class)
ens_plot<-ggplot(data=ens_mock,aes(x=Year,y=Mean,group=1))+
  geom_ribbon(aes(ymin=Mean-SD,ymax=Mean+SD),fill="grey87")+
  geom_line(aes(y=Mean))+
  theme_bw()+
  theme(panel.grid.major=element_blank())+
  theme(panel.grid.minor=element_blank())+
  scale_x_discrete(expand=c(0,0))+
  labs(x="",y="Mean ENS")
ens_plot+theme(aspect.ratio=1)

ens_plot2<-ggplot()+
  geom_smooth(data=ens_mock,x=Year,y=Mean,ymin=Mean-SD,ymax=Mean+SD,stat="identity")+
  theme_bw()+
  theme(panel.grid.major=element_blank())+
  theme(panel.grid.minor=element_blank())
ens_plot+theme(aspect.ratio=1)
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

#Plot as shaded map
#Add species diversity column to data frame
reg$Comp_H<-NA
#Fill Block_H column with values according to regions
reg<-reg%>%mutate(Comp_H=case_when(nuts118nm=="North East (England)" ~ "0.5001757",
                                    nuts118nm=="North West (England)" ~ "0.6238855",
                                    nuts118nm=="Scotland" ~ "0",
                                    nuts118nm=="Northern Ireland" ~ "0",
                                    nuts118nm=="East Midlands (England)" ~ "0.6238855",
                                    nuts118nm=="West Midlands (England)" ~ "0.8488028",
                                    nuts118nm=="East of England" ~ "0.5786850",
                                    nuts118nm=="South East (England)" ~ "0.7896011",
                                    nuts118nm=="London" ~ "0.7896011",
                                    nuts118nm=="South West (England)" ~ "0.8402446",
                                    nuts118nm=="Wales" ~ "0",
                                    nuts118nm=="Yorkshire and The Humber" ~ "0.7342868"))
reg<-reg%>%mutate(Comp_H=na_if(Comp_H,"0"))

#Print to check
print(reg[1:12],n=12)

#Set Comp H as numeric
sapply(reg,class)
reg$Comp_H<-as.numeric(reg$Comp_H)
#Plot sf object using ggplot and scale fill according to Species H
ggplot(data=reg)+
  geom_sf(aes(fill=Comp_H))+
  scale_fill_viridis_c(option="plasma")+
  theme_bw()

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
nw_block_H$ENS<-exp(nw_block_H$div)
#Mean and SD of block diversity for NW
mean(nw_block_H$div)
sd(nw_block_H$div)
#Mean and SD of block ENS
mean(nw_block_H$ENS)
sd(nw_block_H$ENS)

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
ne_block_H$ENS<-exp(ne_block_H$div)
#Mean and SD of block diversity for NE
mean(ne_block_H$div)
sd(ne_block_H$div)
#Mean and SD of block ENS
mean(ne_block_H$ENS)
sd(ne_block_H$ENS)

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
yh_block_H$ENS<-exp(yh_block_H$div)
#Mean and SD of block diversity for YH
mean(yh_block_H$div)
sd(yh_block_H$div)
#Mean and SD of block ENS
mean(yh_block_H$ENS)
sd(yh_block_H$ENS)

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
em_block_H$ENS<-exp(em_block_H$div)
#Mean and SD of block diversity for EM
mean(em_block_H$div)
sd(em_block_H$div)
#Mean and SD of block ENS
mean(em_block_H$ENS)
sd(em_block_H$ENS)


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
ee_block_H$ENS<-exp(ee_block_H$div)
#Mean and SD of block diversity for EE
mean(ee_block_H$div)
sd(ee_block_H$div)
#Mean and SD of block ENS
mean(ee_block_H$ENS)
sd(ee_block_H$ENS)

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
sel_block_H$ENS<-exp(sel_block_H$div)
#Mean and SD of block diversity for SEL
mean(sel_block_H$div)
sd(sel_block_H$div)
#Mean and SD of block ENS
mean(sel_block_H$ENS)
sd(sel_block_H$ENS)

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
sw_block_H$ENS<-exp(sw_block_H$div)
#Mean and SD of block diversity for SW
mean(sw_block_H$div)
sd(sw_block_H$div)
#Mean and SD of block ENS
mean(sw_block_H$ENS)
sd(sw_block_H$ENS)

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
wm_block_H$ENS<-exp(wm_block_H$div)
#Mean and SD of block diversity for WM
mean(wm_block_H$div)
sd(wm_block_H$div)
#Mean and SD of block ENS
mean(wm_block_H$ENS)
sd(wm_block_H$ENS)

#Create DF of mean and SD of block diversity per region
reg<-c("NW","NE","YH","EM","EE","SEL","SW","WM")
nw<-data.frame("Mean"=mean(nw_block_H$ENS),"SD"=sd(nw_block_H$ENS))
ne<-data.frame("Mean"=mean(ne_block_H$ENS),"SD"=sd(ne_block_H$ENS))
yh<-data.frame("Mean"=mean(yh_block_H$ENS),"SD"=sd(yh_block_H$ENS))
em<-data.frame("Mean"=mean(em_block_H$ENS),"SD"=sd(em_block_H$ENS))
ee<-data.frame("Mean"=mean(ee_block_H$ENS),"SD"=sd(ee_block_H$ENS))
sel<-data.frame("Mean"=mean(sel_block_H$ENS),"SD"=sd(sel_block_H$ENS))
sw<-data.frame("Mean"=mean(sw_block_H$ENS),"SD"=sd(sw_block_H$ENS))
wm<-data.frame("Mean"=mean(wm_block_H$ENS),"SD"=sd(wm_block_H$ENS))
block_reg<-rbind(nw,ne,yh,em,ee,sel,sw,wm)
block_reg$Region<-factor(reg,levels=c("NW","NE","YH","EM","EE","SEL","SW","WM"))

ggplot(block_reg,aes(x=Region,y=Mean))+
  geom_bar(stat="identity",fill="grey")+
  geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),width=0.2)+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  xlab("")+
  ylab("Mean Block ENS")+
  theme(aspect.ratio=1)

#Generate ENS dataset and plot for block diversity of regions
reg_ens<-matrix(c(1.362861,1.207486,1.689696,1.574219,1.425375,1.722581,1.773293,1.901147),
                nrow=8)
reg_ens<-as.data.frame(reg_ens)
reg_ens$Region<-c("NW","NE","YH","EM","EE","SEL","SW","WM")
reg_ens$Region<-factor(reg_ens$Region,levels=c("NW","NE","YH","EM","EE","SEL","SW","WM"))
reg_ens$ENS<-exp(reg_ens$V1)
reg_ens<-reg_ens[,2:3]



#Plot as shaded map
#Add species diversity column to data frame
reg$Block_H<-NA
#Fill Block_H column with values according to regions
reg<-reg%>%mutate(Block_H=case_when(nuts118nm=="North East (England)" ~ "1.207486",
                                    nuts118nm=="North West (England)" ~ "1.362861",
                                    nuts118nm=="Scotland" ~ "0",
                                    nuts118nm=="Northern Ireland" ~ "0",
                                    nuts118nm=="East Midlands (England)" ~ "1.574219",
                                    nuts118nm=="West Midlands (England)" ~ "1.901147",
                                    nuts118nm=="East of England" ~ "1.425475",
                                    nuts118nm=="South East (England)" ~ "1.722581",
                                    nuts118nm=="London" ~ "1.722581",
                                    nuts118nm=="South West (England)" ~ "1.773293",
                                    nuts118nm=="Wales" ~ "0",
                                    nuts118nm=="Yorkshire and The Humber" ~ "1.689696"))
reg<-reg%>%mutate(Block_H=na_if(Block_H,"0"))

#Print to check
print(reg[1:11],n=12)

#Set Block H as numeric
sapply(reg,class)
reg$Block_H<-as.numeric(reg$Block_H)
#Plot sf object using ggplot and scale fill according to Species H
ggplot(data=reg)+
  geom_sf(aes(fill=Block_H))+
  scale_fill_viridis_c(option="plasma")+
  theme_bw()

###################################################################
#Combining the SE and London polygons
reg$group<-c("1","2","3","4","5","6","7","7","8","9","10","11")
reg%>%split(.$group)%>%
  lapply(st_union)%>%
  do.call(c, .)
ggplot(data=reg)+
  geom_sf()
###NOT WORKED - NEED TO ASK WHY NOT

###################################################################
#Retrieve current CRS and save as crs
st_crs(reg)
crs<-"+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +datum=OSGB36 +units=m +no_defs"

#Write out shapefile
st_write(reg,"Block_Comp_SCDB_H.shp")

###################################################################
#Could use the SCDB to generate diversity within stands
#Eg compartment diversity within blocks??

