##Data manipulation of Broadleaves/Conifers/All datasets to give dataset with "All" totals
##For data tables etc
##Date created 2020-01-20

##BROADLEAVES DATA

#Restructure data using the Reshape Package
melt<-melt(eng_bl,id=(c("Region","Species")))
bl_regions<-cast(melt,Region~variable+Species)

#Reorder columns to match NFI data 
bl_regions<-as_tibble(bl_regions)
colnames(bl_regions)
bl_regions<-bl_regions[,c(1,3,9,5,12,4,6,11,8,7,2,13,10)]
#Reorder rows to match NFI data
bl_regions<-bl_regions[c(4,3,8,2,1,5,6,7),]
#Rename columns to remove "Total"
colnames(bl_regions)<-sub("Total_","",colnames(bl_regions))
colnames(bl_regions)

#Create dataset with regions and total all broadleaves
bl_total_regions<-bl_regions[,c(1,2)]