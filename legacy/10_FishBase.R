################################################
# Retrieving life history traits from FISHBASE #
################################################

#Date start: May 05/2016 by jotero
#Last update: May 30/2016 by jotero
#
# Project: Convenio Pesca Artesanal CSIC-XUNTA
#
# Authors: Jaime Otero (jotero@iim.csic.es) and Alex Alonso (alex@iim.csic.es)

# References:

#Boettiger et al. (2012) rfishbase: exploring, manipulating and visualizing FishBase data from R. Journal of Fish Biology 81: 2030-2039
#Carl Boettiger, Scott Chamberlain, Duncan Temple Lang, Peter Wainwright (2017). R Interface to 'FishBase'.
#     R package version 2.1.2. https://CRAN.R-project.org/package=rfishbase

#https://cran.r-project.org/web/packages/rfishbase/rfishbase.pdf
#https://ropensci.org/tutorials/rfishbase_tutorial/
#https://github.com/ropensci/rfishbase

#libraries

source(file="C:\\iim-csic\\cursos\\stats\\R\\r_files\\Code\\HighStatLib.r")
source(file="C:\\iim-csic\\cursos\\stats\\R\\r_files\\Code\\multiple_ggplot.r")
source(file="C:\\iim-csic\\cursos\\stats\\R\\r_files\\Code\\CI_mean.r")
source(file="C:\\iim-csic\\cursos\\stats\\R\\r_files\\Code\\functions.r")

library(ggplot2)
library(dplyr)

#fishbase
#we need some libraries to deal with url
library(XML)
library(rfishbase)

setwd("C:\\iim-csic\\proyectos\\ARTESANAIS-convenio_xunta_utpb\\analisys\\obj#2")


###############
#1# LOAD DATA #
###############

#1.1# Taxonomic information from catch data of UTP monitoring

taxon<-read.csv2(file="C:\\iim-csic\\proyectos\\ARTESANAIS-convenio_xunta_utpb\\analisys\\obj#5\\data\\Especies_UTPB.csv",
                 header=T,dec=",",sep=";")
names(taxon)
head(taxon)
str(taxon)


####################################
#2# Fullfill taxonomic information #
####################################

#2.1# Common names in english and spanish

#all common names
common_names("Trisopterus luscus")
#filter by language (select the first option)
unique(data.frame(common_names("Trisopterus luscus") %>% filter(Language=="English")))[,1]
unique(data.frame(common_names("Trisopterus luscus") %>% filter(Language=="Spanish")))[,1]

#extraemos la primera opción por simplicidad
taxon$ENname<-rep(NA,dim(taxon)[1])
taxon$ESname<-rep(NA,dim(taxon)[1])

#loop to add common names to the original data frame
for (i in 1:dim(taxon)[1]){
  
  taxon$ENname[i]<-ifelse(class(try(data.frame(common_names(
    as.character(taxon$SP[i])) %>% filter(Language=="English"))[1,1]))=="try-error",NA,
    data.frame(common_names(as.character(taxon$SP[i])) %>% filter(Language=="English"))[1,1])
  
  taxon$ESname[i]<-ifelse(class(try(data.frame(common_names(
    as.character(taxon$SP[i])) %>% filter(Language=="Spanish"))[1,1]))=="try-error",NA,
    data.frame(common_names(as.character(taxon$SP[i])) %>% filter(Language=="Spanish"))[1,1])

  }

#replace where NA
taxon$ENname<-ifelse(is.na(taxon$common),taxon$ENname,as.character(taxon$common))
taxon$ESname<-ifelse(is.na(taxon$comun),taxon$ESname,as.character(taxon$comun))
taxon<-taxon[,-c(5,6)]

#lets identify fish and invertebrates and solve data frames NAs
taxon$fish<-ifelse(taxon$Filo=="Chordata","Fish","Invertebrate")
taxon$fish<-ifelse(taxon$Subfilo=="Tunicata","Invertebrate",taxon$fish)
taxon$fish<-ifelse(taxon$Reino=="Plantae","Algae",taxon$fish)
unique(taxon[which(is.na(taxon$fish)),]$Filo)
taxon[which(is.na(taxon$fish)),]$fish<-"Invertebrate"
table(taxon$fish)

















### Examples of library workflow ###

library(rfishbase)

myfish<-validate_names(c("Trisopterus luscus","Pollachius pollachius","Labrus bergylta")) # validate scientific names

species(myfish) # example of retrieving data using the function 'species'

morphometrics(myfish) # function morphometrics

ecology(myfish) # function ecology

maturity(myfish,fields=c("tm","Lm")) # function maturity selecting specific 'fields'

gg<-as.data.frame(popgrowth(myfish, # function popgrowth
	fields=c("K","Loo","M","Locality","Temperature")))

plot(K~Loo,data=gg) # plotting data


########################
########################

quartz.options(dpi=75)
library(rfishbase)
library(dplyr)
library(ggplot2)
library(nlme)
library(effects)
library(psych)


# ---------------
#1# Load Grand Banks data

GB.sp<-read.csv2(file="/Users/jaimeoterovillar/Documents/Proyectos/ICES/WGCOMEDA/Species traits/georges_bank_sp.csv",header=T,dec=",",sep=",")

GB.sp


# ---------------
#2# Validate scientific names and add species unique code

#2.1# Validate our scientific name vs name on FISHBASE which will be the valid name
# for further data extraction

GB.sp$val.sp<-rep(NA,dim(GB.sp)[1])

for (i in 1:dim(GB.sp)[1]) {GB.sp$val.sp[i]<-validate_names(
		as.character(GB.sp$spName[i]))}

GB.sp

#2.2# Add species unique code from FISHBASE because we will need it for merging
 
GB.sp$spCode<-rep(NA,dim(GB.sp)[1])

for (i in 1:dim(GB.sp)[1]) {GB.sp$spCode[i]<-as.data.frame(species(
		GB.sp$val.sp[i],fields=c("SpecCode")))[[2]]}

GB.sp


# ---------------
#3# Get categorical information for each species
# Run the loops when there is data for all species. However, there are species
# without data. When this happens (e.g. Body shape) do not run the loop, extract data 
# directly and merge datasets

#3.1# Habitat type

GB.sp$habitat<-rep(NA,dim(GB.sp)[1])

for (i in 1:dim(GB.sp)[1]) {GB.sp$habitat[i]<-species(
		GB.sp$val.sp[i],fields=c("DemersPelag"))[[2]]}

#3.2# Feeding type

GB.sp$diet<-rep(NA,dim(GB.sp)[1])

for (i in 1:dim(GB.sp)[1]) {GB.sp$diet[i]<-ecology(
		GB.sp$val.sp[i],fields=c("FeedingType"))[[3]]}

#3.3# Body shape

Bshape<-as.data.frame(morphology(
	GB.sp$val.sp,fields=c("BodyShapeI","Speccode")))

GB.sp<-merge(GB.sp,Bshape,all=T,by.x="spCode",by.y="Speccode")


# ---------------
#4# Get continuous information for each species
# Run loops for data that do not have more than one entry.
# For those variables that have more than one entry per species extract all
# available information, compute means, and merge datasets

#4.1# Trophic level

GB.sp$dietTroph<-rep(NA,dim(GB.sp)[1])

for (i in 1:dim(GB.sp)[1]) {GB.sp$dietTroph[i]<-as.data.frame(ecology(
		GB.sp$val.sp[i],fields=c("DietTroph")))[[3]]}

GB.sp$foodTroph<-rep(NA,dim(GB.sp)[1])

for (i in 1:dim(GB.sp)[1]) {GB.sp$foodTroph[i]<-as.data.frame(ecology(
		GB.sp$val.sp[i],fields=c("FoodTroph")))[[3]]}

#4.2# Max length (SOME SPs DO NOT HAVE DATA, THEREFORE NAs)

Lmax<-as.data.frame(length_weight(GB.sp$val.sp,
	fields=c("LengthMax","Locality","Sex","SpecCode"))) 

#4.3# Age & Length at maturity (SOME SPs DO NOT HAVE DATA, THEREFORE NAs)

ALmat<-as.data.frame(maturity(GB.sp$val.sp,
	fields=c("tm","Lm","Speccode","Sex")))

#4.4# Growth parameters and mortality (SOME SPs DO NOT HAVE DATA, THEREFORE NAs)

growthData<-as.data.frame(popgrowth(GB.sp$val.sp,
	fields=c("K","Loo","M","Temperature","Locality","SpecCode","Sex")))


# ---------------
#5# Put all data together

#5.1# Compute means for Lmax for both sexes together and per sex

head(Lmax)

LmaxAll<-as.data.frame(tapply(Lmax$LengthMax,Lmax$SpecCode,mean,na.rm=T))
colnames(LmaxAll)<-"lmaxAll"
LmaxAll$SPC<-rownames(LmaxAll)

LmaxF<-filter(Lmax,Sex=="female")
LmaxFemale<-as.data.frame(tapply(LmaxF$LengthMax,LmaxF$SpecCode,mean,na.rm=T))
colnames(LmaxFemale)<-"lmaxF"
LmaxFemale$SPC<-rownames(LmaxFemale)

LmaxM<-filter(Lmax,Sex=="male")
LmaxMale<-as.data.frame(tapply(LmaxM$LengthMax,LmaxM$SpecCode,mean,na.rm=T))
colnames(LmaxMale)<-"lmaxM"
LmaxMale$SPC<-rownames(LmaxMale)

GB.sp<-merge(GB.sp,LmaxAll,all=T,by.x="spCode",by.y="SPC")
GB.sp<-merge(GB.sp,LmaxFemale,all=T,by.x="spCode",by.y="SPC")
GB.sp<-merge(GB.sp,LmaxMale,all=T,by.x="spCode",by.y="SPC")

#5.2# Compute means for ALmat for both sexes together and per sex

head(ALmat)

AmatAll<-as.data.frame(tapply(ALmat$tm,ALmat$Speccode,mean,na.rm=T))
colnames(AmatAll)<-"AmatAll"
AmatAll$SPC<-rownames(AmatAll)

LmatAll<-as.data.frame(tapply(ALmat$Lm,ALmat$Speccode,mean,na.rm=T))
colnames(LmatAll)<-"LmatAll"
LmatAll$SPC<-rownames(LmatAll)

ALmatF<-filter(ALmat,Sex=="female")
AmatFemale<-as.data.frame(tapply(ALmatF$tm,ALmatF$Speccode,mean,na.rm=T))
colnames(AmatFemale)<-"AmatF"
AmatFemale$SPC<-rownames(AmatFemale)

LmatFemale<-as.data.frame(tapply(ALmatF$Lm,ALmatF$Speccode,mean,na.rm=T))
colnames(LmatFemale)<-"LmatF"
LmatFemale$SPC<-rownames(LmatFemale)

ALmatM<-filter(ALmat,Sex=="male")
AmatMale<-as.data.frame(tapply(ALmatM$tm,ALmatM$Speccode,mean,na.rm=T))
colnames(AmatMale)<-"AmatM"
AmatMale$SPC<-rownames(AmatMale)

LmatMale<-as.data.frame(tapply(ALmatM$Lm,ALmatM$Speccode,mean,na.rm=T))
colnames(LmatMale)<-"LmatM"
LmatMale$SPC<-rownames(LmatMale)

GB.sp<-merge(GB.sp,AmatAll,all=T,by.x="spCode",by.y="SPC")
GB.sp<-merge(GB.sp,AmatFemale,all=T,by.x="spCode",by.y="SPC")
GB.sp<-merge(GB.sp,AmatMale,all=T,by.x="spCode",by.y="SPC")
GB.sp<-merge(GB.sp,LmatAll,all=T,by.x="spCode",by.y="SPC")
GB.sp<-merge(GB.sp,LmatFemale,all=T,by.x="spCode",by.y="SPC")
GB.sp<-merge(GB.sp,LmatMale,all=T,by.x="spCode",by.y="SPC")

#5.3# Compute means for growthData for both sexes together and per sex

head(growthData)

M.All<-as.data.frame(tapply(growthData$M,growthData$SpecCode,mean,na.rm=T))
colnames(M.All)<-"Mall"
M.All$SPC<-rownames(M.All)

Loo.All<-as.data.frame(tapply(growthData$Loo,growthData$SpecCode,mean,na.rm=T))
colnames(Loo.All)<-"Looall"
Loo.All$SPC<-rownames(Loo.All)

K.All<-as.data.frame(tapply(growthData$K,growthData$SpecCode,mean,na.rm=T))
colnames(K.All)<-"Kall"
K.All$SPC<-rownames(K.All)

growthF<-filter(growthData,Sex=="female")
Loo.Female<-as.data.frame(tapply(growthF$Loo,growthF$SpecCode,mean,na.rm=T))
colnames(Loo.Female)<-"Loo.F"
Loo.Female$SPC<-rownames(Loo.Female)

K.Female<-as.data.frame(tapply(growthF$K,growthF$SpecCode,mean,na.rm=T))
colnames(K.Female)<-"K.F"
K.Female$SPC<-rownames(K.Female)

growthM<-filter(growthData,Sex=="male")
Loo.Male<-as.data.frame(tapply(growthM$Loo,growthM$SpecCode,mean,na.rm=T))
colnames(Loo.Male)<-"Loo.M"
Loo.Male$SPC<-rownames(Loo.Male)

K.Male<-as.data.frame(tapply(growthM$K,growthM$SpecCode,mean,na.rm=T))
colnames(K.Male)<-"K.M"
K.Male$SPC<-rownames(K.Male)

GB.sp<-merge(GB.sp,M.All,all=T,by.x="spCode",by.y="SPC")
GB.sp<-merge(GB.sp,Loo.All,all=T,by.x="spCode",by.y="SPC")
GB.sp<-merge(GB.sp,K.All,all=T,by.x="spCode",by.y="SPC")
GB.sp<-merge(GB.sp,Loo.Female,all=T,by.x="spCode",by.y="SPC")
GB.sp<-merge(GB.sp,K.Female,all=T,by.x="spCode",by.y="SPC")
GB.sp<-merge(GB.sp,Loo.Male,all=T,by.x="spCode",by.y="SPC")
GB.sp<-merge(GB.sp,K.Male,all=T,by.x="spCode",by.y="SPC")

dim(GB.sp)
head(GB.sp)


# ---------------
#6# Many species have multiple entries usually for different areas,
# thus mean values might not be appropriate for a given species in a specific
# region. Therefore, we need a different way of assigning K and Loo to
# those species with several data. An idea to do this would be to run a model
# including temperature to separate zones when multiple data are available per
# species. This is just an idea, a test to be further evaluated....
# However, the best option would be to match area-specific traits for a widely 
# distributed species. An important case would be cod (see below)

#6.1# Plot data

head(growthData)

unique(growthData$Sex)

growthData$sciname<-factor(growthData$sciname)
growthData$Sex<-factor(growthData$Sex)
growthData$LogK<-log(growthData$K)
growthData$LogLoo<-log(growthData$Loo)

ggplot(data=growthData,aes(x=LogLoo,y=LogK))+
	geom_point(aes(colour=sciname,shape=Sex),size=3)+
	geom_smooth(method="lm")+
	theme(legend.position="none")

#6.2# Fit a mixed-effects model using species as a random effect

lme0<-lme(LogK~Sex+LogLoo+Temperature,data=growthData,random=~1|sciname,na.action=na.exclude)

summary(lme0) # Sex non-significant

lme1<-lme(LogK~LogLoo+Temperature,data=growthData,random=~1|sciname,na.action=na.exclude)

summary(lme1) # Correlation between Loo and Intcpt could be solved by centering Loo
intervals(lme1) # OK
plot(lme1) # OK
plot(coef(lme1)) # OK
plot(allEffects(lme1)) # OK

#6.3# Example of obtaining K predictions (at the population level and at the
# species level) for a given species and at a known temperature and Loo

newCod<-data.frame(sciname="Gadus morhua", # Which species?
					LogLoo=log(80), # Loo in cm
					Temperature=10) # Temperature in ÂºC

newCod.pred<-predict(lme1,newCod,level=0:1)
exp(newCod.pred[2]) # exponentiate predictions to obtain K values on their known scale 


# ---------------
#7# Final database

names(GB.sp)
GB.sp<-GB.sp[,-8] # remove empty 'StockCode' column

head(GB.sp)

pairs.panels(GB.sp[,c(9:11,14,17,20:22)],ellipses=F) # simple correlation pairs of sex-grouped variables

