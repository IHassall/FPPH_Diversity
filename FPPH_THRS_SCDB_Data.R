##Script Name:
##Author: Izzy Hassall, Ecosystems Analysis
##Contact (if different from above):
##Date Created: 2020-01-13
##Date Modified:
##Licence:
##Abstract:


##R version 3.6.1 (2019-07-05)
##Dependencies:

library(vegan)
library(tidyr)
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape)
library(tibble)

##FPPH THRS Indicators Diversity##
##FC SCDB Dataset to examine species and structural diversity##

#Read in SCDB data
scdb<-read.csv("J:/GISprojects/Ecosystems Analysis/FPPH/THRS indicators/Data/SCDB/National_Forest_Estate_Subcompartments_England_2019.csv",header=T,na.strings=c(""))
scdb<-scdb[,c(9,18,20,26,38)]
#Rename columns
colnames(scdb)
names(scdb)[names(scdb)=="PRISPECIES"]<-"PrimarySpecies"
names(scdb)[names(scdb)=="CULTIVATN"]<-"Cultivation"
names(scdb)[names(scdb)=="PRILANDUSE"]<-"PrimaryLandUse"
names(scdb)[names(scdb)=="PRIHABITAT"]<-"PrimaryHabitat"
names(scdb)[names(scdb)=="Shape__Area"]<-"Area"

#Create datasets for each variable and remove blanks
#Species
scdb_species<-scdb[,c(1,5)]
scdb_species<-scdb_species[!(scdb_species$PrimarySpecies==" "),]
levels(scdb_species$PrimarySpecies)
scdb_species<-as.data.frame(scdb_species)
#Cultivation
scdb_cult<-scdb[,c(2,5)]
scdb_cult<-scdb_cult[!(scdb_cult$Cultivation==" "),]
levels(scdb_cult$Cultivation)
#Land Use
scdb_land<-scdb[,c(3,5)]
scdb_land<-scdb_land[!(scdb_land$PrimaryLandUse==" "),]
#Habitat
scdb_hab<-scdb[,c(4,5)]
scdb_hab<-scdb_hab[!(scdb_hab$PrimaryHabitat==" "),]

#Sum area of each level for each dataset
species_area<-aggregate(scdb_species$Area,by=list(PrimarySpecies=scdb_species$PrimarySpecies),FUN=sum)
names(species_area)[names(species_area)=="x"]<-"Area"
species_area$Area<-format(species_area$Area,scientific=FALSE)
species_area$Area<-as.numeric(species_area$Area)
cult_area<-aggregate(scdb_cult$Area,by=list(Cultivation=scdb_cult$Cultivation),FUN=sum)
names(cult_area)[names(cult_area)=="x"]<-"Area"
cult_area$Area<-format(cult_area$Area,scientific=FALSE)
cult_area$Area<-as.numeric(cult_area$Area)
land_area<-aggregate(scdb_land$Area,by=list(PrimaryLandUse=scdb_land$PrimaryLandUse),FUN=sum)
names(land_area)[names(land_area)=="x"]<-"Area"
land_area$Area<-format(land_area$Area,scientific=FALSE)
land_area$Area<-as.numeric(land_area$Area)
hab_area<-aggregate(scdb_hab$Area,by=list(PrimaryHabitat=scdb_hab$PrimaryHabitat),FUN=sum)
names(hab_area)[names(hab_area)=="x"]<-"Area"
hab_area$Area<-format(hab_area$Area,scientific=FALSE)
hab_area$Area<-as.numeric(hab_area$Area)
cult_area<-aggregate(scdb_cult$Area,by=list(Cultivation=scdb_cult$Cultivation),FUN=sum)
names(cult_area)[names(cult_area)=="x"]<-"Area"
cult_area$Area<-format(cult_area$Area,scientific=FALSE)
land_area<-aggregate(scdb_land$Area,by=list(PrimaryLandUse=scdb_land$PrimaryLandUse),FUN=sum)
names(land_area)[names(land_area)=="x"]<-"Area"
land_area$Area<-format(land_area$Area,scientific=FALSE)
hab_area<-aggregate(scdb_hab$Area,by=list(PrimaryHabitat=scdb_hab$PrimaryHabitat),FUN=sum)
names(hab_area)[names(hab_area)=="x"]<-"Area"
hab_area$Area<-format(hab_area$Area,scientific=FALSE)

#Convert from m2 to km2
species_area$Area<-species_area$Area*0.000001
cult_area$Area<-cult_area$Area*0.000001
land_area$Area<-land_area$Area*0.000001
hab_area$Area<-hab_area$Area*0.000001

#Shannon Diversity
#SPECIES 
#Transpose species_area
species_area_t<-t(species_area)
species_area_t<-as.matrix(species_area_t)
colnames(species_area_t)<-as.character(unlist(species_area_t[1,]))
species_area_t=species_area_t[-1,]
species_area_t<-sapply(species_area_t,as.numeric)
diversity(species_area_t)
#Create mock dataset to show time series for FC land
#Starting point Primary Species Area of National_Forest_Estate_Subcompartments_England_2019
#Data in Km2 (as converted further up this script)
levels(species_area$PrimarySpecies)
species_mock<-matrix(c(3.380631483,35.655031414,0.343013390,0.154138501,0.003469147,129.9844715,87.943191842,0.009107129,0.713803763,0.035176459,0.013663371,0.017778363,0.033731395,0.706650635,2.825621326,0.158013542,0.037311321,258.468516,0.150330528,111.3603824,1.395565643,0.109395580,0.125103455,0.007492709,12.348602331,0.339698320,0.276509087,2.586257634,4.972488642,0.089460604,0.078707012,0.219732616,0.450173277,6.579284940,2.762742393,0.007974362,1.854134486,0.021872289,0.021675137,27.635111760,4.968051811,0.137161617,0.707211375,49.149723988,0.005553494,2.753178127,0.451376965,0.269810698,30.114791599,0.516438780,0.778487727,68.395834500,8.995940508,1.053834489,0.021170627,0.585573250,0.056690734,0.572278583,69.304100491,170.7411793,0.419965184,2.522676507,0.066184460,0.008289468,0.691406580,0.265959105,0.150561793,0.007293732,0.061070032,0.057918842,0.068517465,0.553682978,2.432644163,0.014960661,0.200817314,0.020784595,3.285892381,0.101783727,0.667376660,179.1820978,1.813222263,3.521576900,0.012662188,0.036053637,2.163994375,490.2894832,3.305462229,17.495177366,12.778216578,0.005823108,0.030925470,0.195269984,17.552814532,9.825564079,0.330476405,0.060445715,0.553103853,0.141953815,0.609162737,0.013998372,0.101647204,1.278837503,
                    3.352463,34.898,0.360192,0.178975,0.00298047,130.9844715,89.12786,0.00095891,0.738974,0.045612,0.014195,0.013849,0.03389812,0.8000381,2.98041,0.167821,0.045984,257.2143,0.1508941,112.34091,1.402948,0.89751,0.2237805,0.0059481,13.0349812,0.23981,0.28971,2.587896,4.90741,0.091997,0.099471,0.199383,0.459834,6.90816,2.56194,0.00797412,1.85948,0.0243488,0.02108523,27.41085,4.98471,0.137892,0.707397,49.129085,0.0059857,2.130855,0.42431,0.313098,30.3241,0.524983,0.778371,69.4124,8.094,1.023985,0.0199948,0.69084,0.0689783,0.55874,70.012975,171.5678,0.418671,2.529847,0.67836,0.009873,0.693798,0.24312,0.154123,0.00694812,0.0698341,0.049837,0.079128,0.498471,2.398971,0.019873,0.200893,0.028938,3.498271,0.1029837,0.668974,180.57894,1.81322,3.529817,0.012661,0.036761,2.20948,491.009481,3.305948,17.541985,11.78983,0.0059841,0.030983,0.1950984,17.09381,9.82739,0.33098,0.0604491,0.549871,0.14098,0.609129,0.0149839,0.12984,1.289408,
                    3.3442341,34.123,0.369078,0.162837,0.0029874,130.3123,88.90584,0.00908481,0.72873,0.039875,0.0132312,0.0159348,0.0335984,0.784612,2.987484,0.158974,0.045235,258.02391,0.16763,112.25434,1.31209,0.918274,0.129847,0.006874,12.98471,0.312309,0.27839,2.589741,4.90847,0.089741,0.087974,0.209737,0.456093,6.65462,2.6984,0.00812341,1.85123,0.02413,0.02786,27.54123,4.98731,0.13873,0.7093,49.14094,0.00559844,2.150938,0.425094,0.314985,30.50948,0.51249,0.7772879,69.00912,8.12598,1.0609841,0.02001859,0.610925,0.070172,0.5124,69.0941,172.8908,0.41985,2.542309,0.689804,0.009847,0.72085,0.25094,0.169804,0.0069804,0.04124,0.060984,0.080934,0.56709,2.412093,0.018598,0.2009128,0.028904,3.3252,0.1029031,0.718239,182.50948,1.81093,3.54109,0.01352,0.036783,2.34391,492.03901,3.32123,17.500412,11.908312,0.0050941,0.030918,0.195674,17.04789,9.87931,0.3305409,0.06984,0.54981,0.140923,0.60895,0.0138924,0.1020938,1.298307),
                nrow=102,
                dimnames=list(c("Alder","Ash","Aspen","Atlas cedar","Austrian pine","Beech","Birch (downy/silver)","Bird cherry","Bishop pine","Black poplar", "Black walnut",            
                                "Blackthorn","Cider gum","Coast redwood","Common alder",            
                                "Common lime","Common walnut","Corsican pine","Crack willow",            
                                "Douglas fir","Downy birch","Downy oak","Elm",                     
                                "English elm","European larch","European silver fir","Field maple",            
                                "Goat willow","Grand Fir","Grey alder","Grey poplar",             
                                "Grey willow","Hawthorn species","Hazel","Holly species",           
                                "Holm oak","Hornbeam","Horse chestnut","Hungarian oak",           
                                "Hybrid larch","Hybrid poplar","Italian alder","Japanese cedar",          
                                "Japanese larch","Large-leaved lime","Lawsons cypress","Leyland cypress",         
                                "Lime","Lodgepole pine","Macedonian pine","Maritime pine",           
                                "Mixed broadleaves","Mixed conifers","Monterey pine","Mountain pine",           
                                "Noble fir","Nordmann fir","Norway maple","Norway spruce",           
                                "Oak (robur/petraea)","Oriental spruce","other broadleaves","other Cedar",             
                                "other cherry spp","other conifers","other Eucalyptus","other firs (Abies)",     
                                "other Nothofagus","other pines","other poplar spp","other spruces",           
                                "other willows","Pedunculate/common oak","Ponderosa pine","Raoul/rauli",             
                                "Red (pacific silver) fir","Red oak","Roble","Rowan",                   
                                "Scots pine","Serbian spruce","Sessile oak","Shagbark hickory",        
                                "Shining gum","Silver birch","Sitka spruce","Small-leaved lime",       
                                "Sweet chestnut","Sycamore","Tulip tree","Turkey oak",              
                                "Wellingtonia","Western hemlock","Western red cedar","Weymouth pine",           
                                "White poplar","White willow","Whitebeam","Wild cherry/gean",        
                                "Wild service tree","Wych elm","Yew"),
                              c("2019","2020","2021")))
#Transpose mock data and calculate Shannon diversity
species_mock_t<-t(species_mock)
diversity(species_mock_t)
years<-c("2019","2020","2021")
species_div<-diversity(species_mock_t)
species_div<-data.frame(years,species_div)
names(species_div)[names(species_div)=="species_div"]<-"Shannon"
names(species_div)[names(species_div)=="years"]<-"Year"
#Plot Shannon over time 
ggplot(data=species_div,aes(x=Year,y=Shannon))+
  geom_bar(stat="identity",width=0.75,colour="black",fill="grey87")+
  labs(x="",y="Shannon H")+
  coord_cartesian(ylim=c(2.5,2.6))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=50,hjust=1))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y=element_text(colour="black"))


#CULTIVATION
cult_area_t<-t(cult_area)
cult_area_t<-as.matrix(cult_area_t)
colnames(cult_area_t)<-as.character(unlist(cult_area_t[1,]))
cult_area_t=cult_area_t[-1,]
cult_area_t<-sapply(cult_area_t,as.numeric)
diversity(cult_area_t)

#LAND USE
land_area_t<-t(land_area)
land_area_t<-as.matrix(land_area_t)
colnames(land_area_t)<-as.character(unlist(land_area_t[1,]))
land_area_t=land_area_t[-1,]
land_area_t<-sapply(land_area_t,as.numeric)
diversity(land_area_t)

#HABITAT
#Split into fine and broad habitat categories
hab_fine<-hab_area[c(1:38),]
hab_broad<-hab_area[-c(1:38),]
#Fine habitat diversity
hab_fine_t<-t(hab_fine)
hab_fine_t<-as.matrix(hab_fine_t)
colnames(hab_fine_t)<-as.character(unlist(hab_fine_t[1,]))
hab_fine_t=hab_fine_t[-1,]
hab_fine_t<-sapply(hab_fine_t,as.numeric)
diversity(hab_fine_t)
#Create fine scale habitat mock dataset
hab_fine_mock<-matrix(c(0.0061538194,0.0412456106,81.9336972656,0.011754573,0.6703760326,0.0007146183,0.2121085497,0.0133437908,8.228857994,2.4120975129,29.9165275268,1.1739111741,116.9416961998,1.1816143649,195.8191388902,7.8077608308,0.0581115331,0.2177308694,0.0331564463,0.0092167595,0.0272282360,47.3414541756,0.0289241096,0.2014548293,0.3116383635,0.1393523976,0.0236835422,1.5482222334,0.0109429170,2.2348946002,1.8303437706,0.0543981986,0.3689275657,52.9502322465,2.9352714397,7.0458041845,5.3823866054,1.3353282273,
                        0.00616345,0.044631,82.23432,0.0091234,0.729874,0.00072334,0.2121089,0.014672,8.45891,2.56711,30.56189,1.19524,117.32541,1.00892,194.90841,7.807983,0.058111,0.218974,0.033156,0.010894,0.031085,45.34129,0.0308951,0.201354,0.31661,0.136786,0.02368,1.47098,0.011356,2.2562,1.897412,0.049878,0.368927,52.95084,2.87619,7.033592,5.23581,1.5234,
                        0.0061897,0.04487,82.2561,0.009084,0.69841,0.0007687,0.212109,0.0145142,8.78312,2.76825,30.97612,1.20984,116.78541,0.994812,194.4125,7.931254,0.058111,0.21894,0.033156,0.01241,0.030981,44.24125,0.030945,0.201354,0.31781,0.13531,0.02368,1.470312,0.011451,2.26731,1.91852,0.047821,0.3689,52.89124,2.756411,7.02364,5.125167,1.51467),
                      nrow=38,
                      dimnames=list(c("Aquifer fed naturally fluctuating water",          
                                      "Arable Field margins","Blanket bog",                                    
                                      "Coastal saltmarsh","Eutrophic standing waters",                        
                                      "Hedgerows","Inland Rock Outcrop and Scree Habitats",           
                                      "Limestone pavements","Lowland beech/yew woodland",                       
                                      "Lowland calcareous grassland","Lowland dry acid grassland",                       
                                      "Lowland Fens","Lowland heathland",                                
                                      "Lowland meadows","Lowland Mixed Deciduous Woodland",
                                      "Lowland raised bog","Maritime cliff/slopes",                            
                                      "Mesotrophic lakes","Mountain Heaths and Willow Scrubs",                
                                      "Native pine woodlands","Non HAP native pinewood",                          
                                      "Not Surveyed","Open Mosaic Habitats on Previously Developed Land",
                                      "Ponds","Purple moor grass/rush pastures",
                                      "Reedbeds","Rivers",                                           
                                      "Surveyed; Unknown Habitat","Traditional Orchard",                              
                                      "Upland birchwoods","Upland calcareous grassland",                      
                                      "Upland Flushes, Fens & Swamps","Upland hay meadows",                               
                                      "Upland heathland","Upland mixed ashwoods",                            
                                      "Upland oakwood","Wet woodland",                                     
                                      "Wood Pasture & Parkland"),
                                    c("2019","2020","2021")))

#Calculate Shannon on mock time series data for fine habitat
#Transpose mock dataset
hab_fine_mock<-t(hab_fine_mock)
#Shannon diversity index on mock data
hab_fine_div<-diversity(hab_fine_mock)
hab_fine_div
#Create data frame
hab_fine_div<-data.frame(years,hab_fine_div)
names(hab_fine_div)[names(hab_fine_div)=="hab_fine_div"]<-"Shannon"
#Visualise as barplot with rescaled y axis to show small differences
ggplot(data=hab_fine_div,aes(x=years,y=Shannon))+
  geom_bar(stat="identity",width=0.75,colour="black",fill="grey87")+
  labs(x="",y="Shannon H")+
  coord_cartesian(ylim=c(1.94,1.96))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=50,hjust=1))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y=element_text(colour="black"))

#Broad habitat diversity
hab_broad_t<-t(hab_broad)
hab_broad_t<-as.matrix(hab_broad_t)
colnames(hab_broad_t)<-as.character(unlist(hab_broad_t[1,]))
hab_broad_t=hab_broad_t[-1,]
hab_broad_t<-sapply(hab_broad_t,as.numeric)
diversity(hab_broad_t)
#Create broad scale habitat mock data
hab_broad_mock<-matrix(c(87.6090365969,5.3093990810,5.0539321839,2.1090936816,14.7632106268,431.6019897205,6.9773839686,0.9635381525,1372.820003,0.2740643477,0.3518739900,13.3030872265,3.2224364318,6.7735505459,0.4738680780,1.0702265646,1.2303646876,0.0093113479,
                           88.24512,5.301242,5.01249,2.10909,16.78129,433.14256,7.56822,0.87512,1389.9812,0.28124,0.351985,13.304712,3.2224,6.89712,0.47386,1.07022,1.21904,0.015691,
                           87.56891,5.40571,5.023512,2.10909,16.99812,433.56123,7.56236,0.84578,1401.765,0.28454,0.35871,13.30672,3.2226,6.9236,0.47386,1.07022,1.21785,0.015763),
                       nrow=18,
                       dimnames=list(c("ACID GRASSLAND","ARABLE/HORTICULTURE","BOGS",                                               
                                        "BOUNDARY & LINEAR FEATURES","BRACKEN",                                            
                                        "BROADLEAVED; MIXED/YEW WOODLANDS","BUILT UP AREAS & GARDENS",                           
                                        "CALCAREOUS GRASSLAND","CONIFEROUS WOODLANDS",                               
                                        "DWARF SHRUB HEATH","FEN; MARSH/SWAMP",                                   
                                        "IMPROVED GRASSLAND","INLAND ROCK",                                        
                                        "NEUTRAL GRASSLAND","RIVERS & STREAMS",                                   
                                        "STANDING OPEN WATER/CANALS","UNKNOWN","URBAN"),
                                     c("2019","2020","2021")))

#Calculate Shannon on mock time series data for broad habitat
#Transpose mock dataset
hab_broad_mock<-t(hab_broad_mock)
#Shannon diversity index on mock data
hab_broad_div<-diversity(hab_broad_mock)
hab_broad_div
#Create data frame
hab_broad_div<-data.frame(years,hab_broad_div)
names(hab_broad_div)[names(hab_broad_div)=="hab_broad_div"]<-"Shannon"
#Visualise as barplot with rescaled y axis to show small differences
ggplot(data=hab_broad_div,aes(x=years,y=Shannon))+
  geom_bar(stat="identity",width=0.75,colour="black",fill="grey87")+
  labs(x="",y="Shannon H")+
  coord_cartesian(ylim=c(0.89,0.90))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=50,hjust=1))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y=element_text(colour="black"))

#Broad habitat diversity
hab_broad_t<-t(hab_broad)
hab_broad_t<-as.matrix(hab_broad_t)
colnames(hab_broad_t)<-as.character(unlist(hab_broad_t[1,]))
hab_broad_t=hab_broad_t[-1,]
hab_broad_t<-sapply(hab_broad_t,as.numeric)
diversity(hab_broad_t)
#Create broad scale habitat mock data
hab_broad_mock<-matrix(c(87.6090365969,5.3093990810,5.0539321839,2.1090936816,14.7632106268,431.6019897205,6.9773839686,0.9635381525,1372.820003,0.2740643477,0.3518739900,13.3030872265,3.2224364318,6.7735505459,0.4738680780,1.0702265646,1.2303646876,0.0093113479,
                           88.24512,5.301242,5.01249,2.10909,16.78129,433.14256,7.56822,0.87512,1389.9812,0.28124,0.351985,13.304712,3.2224,6.89712,0.47386,1.07022,1.21904,0.015691,
                           87.56891,5.40571,5.023512,2.10909,16.99812,433.56123,7.56236,0.84578,1401.765,0.28454,0.35871,13.30672,3.2226,6.9236,0.47386,1.07022,1.21785,0.015763),
                       nrow=18,
                       dimnames=list(c("ACID GRASSLAND","ARABLE/HORTICULTURE","BOGS",                                               
                                        "BOUNDARY & LINEAR FEATURES","BRACKEN",                                            
                                        "BROADLEAVED; MIXED/YEW WOODLANDS","BUILT UP AREAS & GARDENS",                           
                                        "CALCAREOUS GRASSLAND","CONIFEROUS WOODLANDS",                               
                                        "DWARF SHRUB HEATH","FEN; MARSH/SWAMP",                                   
                                        "IMPROVED GRASSLAND","INLAND ROCK",                                        
                                        "NEUTRAL GRASSLAND","RIVERS & STREAMS",                                   
                                        "STANDING OPEN WATER/CANALS","UNKNOWN","URBAN"),
                                     c("2019","2020","2021")))

#Calculate Shannon on mock time series data for broad habitat
#Transpose mock dataset
hab_broad_mock<-t(hab_broad_mock)
#Shannon diversity index on mock data
hab_broad_div<-diversity(hab_broad_mock)
hab_broad_div
#Create data frame
hab_broad_div<-data.frame(years,hab_broad_div)
names(hab_broad_div)[names(hab_broad_div)=="hab_broad_div"]<-"Shannon"
#Visualise as barplot with rescaled y axis to show small differences
ggplot(data=hab_broad_div,aes(x=years,y=Shannon))+
  geom_bar(stat="identity",width=0.75,colour="black",fill="grey87")+
  labs(x="",y="Shannon H")+
  coord_cartesian(ylim=c(0.89,0.90))+
  theme_bw()+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=50,hjust=1))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y=element_text(colour="black"))


##Land Use 
##Selected attributes of Land Use and Cultivation
#Obtain annual values from SCDB dataset to create time series of open and planted areas
#PLANTING aspects of cultivation 
plant<-cult_area[c(2,5,15,16),]
plant_mock<-matrix(c(12.4264784,1.1630937,0.3172046,0.2544703,
                     13.3789,1.28741,0.308973,0.25488,
                     13.69871,1.322099,0.3089,0.24511,
                     14.00012,1.3299897,0.3099812,0.23141,
                     14.55611,1.35488,0.309124,0.23156,
                     14.24812,1.36728,0.30495,0.23154,
                     14.89744,1.38041,0.30123,0.24366),
                   nrow=4,
                   dimnames=list(c("Clear brash and direct plant","Direct plant with screef","Plant at stump","Plant at stump on existing ploughing"),
                                 c("2019","2020","2021","2022","2023","2024","2025")))
#Create sum of open areas for each year
plant_mock_total<-colSums(plant_mock)
year<-c("2019","2020","2021","2022","2023","2024","2025")
plant_mock_total<-data.frame(year,plant_mock_total)
names(plant_mock_total)[names(plant_mock_total)=="plant_mock_total"]<-"Total_Area"
ggplot(data=plant_mock_total,aes(x=year,y=Total_Area))+
  geom_bar(stat="identity",width=0.75,colour="black",fill="grey87")+
  labs(x="",y="Planted Area (Km2)")+
  theme_bw()+
  coord_cartesian(ylim=c(14,17))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=50,hjust=1))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y=element_text(colour="black"))                  


#Land use creating OPEN areas
open<-land_area[c(5,12,18,29),]
open_mock<-matrix(c(0.2187725,86.9912569,467.9734293,23.6763863,
                    0.215984,86.8972,468.9084,23.7612,
                    0.214094,87.3452,470.452,23.4123,
                    0.216798,87.412,469.0908,23.5709,
                    0.21777,85.9084,468.512,23.56781,
                    0.25178,86.059,468.98,23.890785,
                    0.23124,85.6712,467.9084,23.4612),
                  nrow=4,
                  dimnames=list(c("Burnt","Felled","Open","Unplantable or bare"),
                                c("2019","2020","2021","2022","2023","2024","2025")))
#Create sum of open areas for each year
open_mock_total<-colSums(open_mock)
year<-c("2019","2020","2021","2022","2023","2024","2025")
open_mock_total<-data.frame(year,open_mock_total)
names(open_mock_total)[names(open_mock_total)=="open_mock_total"]<-"Total_Area"
ggplot(data=open_mock_total,aes(x=year,y=Total_Area))+
  geom_bar(stat="identity",width=0.75,colour="black",fill="grey87")+
  labs(x="",y="Open Area (Km2)")+
  theme_bw()+
  coord_cartesian(ylim=c(550,600))+
  theme(panel.grid.major=element_blank(),panel.grid.minor=element_blank())+
  theme(axis.text.x=element_text(angle=50,hjust=1))+
  theme(axis.text.x=element_text(colour="black"))+
  theme(axis.text.y=element_text(colour="black"))


