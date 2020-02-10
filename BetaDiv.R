#BETAPART PACKAGE
install.packages("betapart")
library(betapart)
data(bbsData)
rownames(bbs1980)
head(bbs1980)
#Binary presence/absence data matrix with rows as sites and columns as species ID
#1=presence, 0=absence

#Multiple site dissimilarities
beta.multi(bbs1980,index.family="sorensen")
#$beta.SIM
#[1] 0.8855248
##This value is the turnover component - Simpson dissimilarity

#$beta.SNE
#[1] 0.03252266
##value of the nestedness component, measured as nestedness-resultant fraction of Sorensen dissimilarity

#$beta.SOR
#[1] 0.9180474
##value of the overall beta diversity, measured as Sorensen dissimilarity

###########################################################
#Abundance-based multiple-site dissimilarities
#(accounting for balanced variation and abundance gradient components of dissimilarity and sum of both)
#Data frame where rows are sites and columns are species
#DF could be betapart.abund object derived from the betapart.core.abund function
beta.multi.abund(bbs1980,index.family="bray")
#$beta.BRAY.BAL
#[1] 0.8855248
##Value of balanced variation component of Bray-Curtis multiple site dissimilarity

#$beta.BRAY.GRA
#[1] 0.03252266
##value of abundance-gradient component of Bray-Curtis multiple site dissimilarity

#$beta.BRAY
#[1] 0.9180474
##value of overall dissimilarity measured as Bray-Curtis multiple site dissimilarity

############################################################
#Incidence based pair wise dissimilarities
#3 distance matrices accounting for the turnover, nestedness and total dissimilarity
beta.pair(bbs1980,index.family="sorensen")
#Pair-wise comparisons between site pairs - very large data matrix produced
#beta.sim is dissimiilarity matrix accounting for spatial turnover 
#beta.snw is diss matrix accounting for nestedness-resultant dissimilarity
#beta.sor is diss matrix accounting for total dissimilarity

#beta.pair.abund is an option for abundance data

############################################################
############################################################
#VEGAN PACKAGE
library(vegan)
data(BCI)
#Data frame of 50 sites (rows) and 225 species (columns) with values of abundance (stem counts of trees on 1 ha plots)

#Calculate classic value of beta diversity
#B=no. species / (mean richness per one site -1)
?specnumber
#specnumber() looks to give the number of species per row (site) - rowSums(BCI>0) would be the same
ncol(BCI)/mean(specnumber(BCI))-1
#[1] 1.478519
#This gives a single number for beta diversity

#Pairwise comparison of sites
#a is number of shared species in two sites
#b and c are the numbers of species unique to each site
#Then (mean richness per one site)=((2a+b+c)/(2a+b+c)/2)-1
#This is sorensen index of dissimilarity - found using vegdist()
beta<-vegdist(BCI,binary=TRUE)
mean(beta)
#[1] 0.3399075

betadiver(help=TRUE)
