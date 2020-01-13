##Subset Broadleaves Data
#For use in bar plots etc
#Subset data for regions and remove "All Broadleaves" rows
#Follow on from the DataPrep script
NWeng<-subset(eng_bl,Region=="NW England")
NWeng<-NWeng[-1,]
NEeng<-subset(eng_bl,Region=="NE England")
NEeng<-NEeng[-1,]
YorkHumb<-subset(eng_bl,Region=="Yorkshire and Humber")
YorkHumb<-YorkHumb[-1,]
EMid<-subset(eng_bl,Region=="E Midlands")
EMid<-EMid[-1,]
Eeng<-subset(eng_bl,Region=="E England")
Eeng<-Eeng[-1,]
SELon<-subset(eng_bl,Region=="SE and London")
SELon<-SELon[-1,]
SWeng<-subset(eng_bl,Region=="SW England")
SWeng<-SWeng[-1,]
WMid<-subset(eng_bl,Region=="W Midlands")
WMid<-WMid[-1,]

#Specify Species as ordered factor to ensure always in same order
NWeng$Species<-factor(NWeng$Species,levels=NWeng$Species)
NWeng$Species
NEeng$Species<-factor(NEeng$Species,levels=NEeng$Species)
NEeng$Species
YorkHumb$Species<-factor(YorkHumb$Species,levels=YorkHumb$Species)
YorkHumb$Species
EMid$Species<-factor(EMid$Species,levels=EMid$Species)
EMid$Species
Eeng$Species<-factor(Eeng$Species,levels=Eeng$Species)
Eeng$Species
SELon$Species<-factor(SELon$Species,levels=SELon$Species)
SELon$Species
SWeng$Species<-factor(SWeng$Species,levels=SWeng$Species)
SWeng$Species
WMid$Species<-factor(WMid$Species,levels=WMid$Species)
WMid$Species