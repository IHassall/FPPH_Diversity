library(sf)
library(rgdal)
library(dplyr)
library(tibble)
library(ggplot2)
library(reshape2)
library(vegan)
library(betapart)
library(asbio)

#Load in dataset
scdb<-st_read("J:/GISprojects/Ecosystems Analysis/FPPH/THRS indicators/Data/SCDB/National_Forest_Estate_Subcompartments_England_2019/National_Forest_Estate_Subcompartments_England_2019.shp")
colnames(scdb)
scdb_spec<-scdb%>%select("BLOCK","PRISPECIES","Shape__Are")
sapply(scdb_spec,class)
#Remove geometry column
st_geometry(scdb_spec)<-NULL
#Print some of df 
head(scdb_spec)

#Restructure data frame to give site by abundance matrix
melt<-melt(scdb_spec,id=c("BLOCK","PRISPECIES"))
scdb_spec<-dcast(melt,BLOCK~PRISPECIES,sum)

#Create binary presence/absence matrix too
scdb_pa<-ifelse(scdb_spec>0,1,0)
#Remove Block column to just give species matrix without site names
scdb_pa<-scdb_pa[,-1]

#Generate Sorensen index of dissimilarity
beta<-vegdist(scdb_pa,binary=TRUE)
print(beta[1:50])
mean(beta)

#Another method to calculate Sorensen index
dist_sor<-beta.pair(scdb_pa,index.family="sorensen")

#betapart.core
beta1<-betapart.core(scdb_pa)
#beta.multi
beta2<-beta.multi(scdb_pa,index.family="sorensen")
#beta.pair
beta3<-beta.pair(scdb_pa,index.family="sorensen")

#Use beta (from vegdist()) for clustering
#Transform beta diversity matrix into set of coordinates
mds<-metaMDS(beta)
#Convert to form used for plotting
mds_data<-as.data.frame(mds$points)
mds_data$Block<-rownames(mds_data)
ggplot(mds_data,aes(x=MDS1,y=MDS2))+
  geom_point()+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(aspect.ratio=0.9)

#Create data frame with regions (MOCK)
mds_data$Block<-as.character(mds_data$Block)
sapply(mds_data,class)
mds_reg<-mds_data%>%mutate(Region=case_when(Block==c(1:10) ~ "NE",
                                            Block==c(11:20) ~ "NW",
                                            Block==c(21:30) ~ "EM",
                                            Block==c(31:40) ~ "WM",
                                            Block==c(41:50) ~ "EE",
                                            Block==c(51:60) ~ "SE",
                                            Block==c(61:80) ~ "YH",
                                            Block==c(81:100) ~ "SW"))
ggplot(mds_reg,aes(x=MDS1,y=MDS2,colour=Region))+
  geom_point()+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(aspect.ratio=0.9)



