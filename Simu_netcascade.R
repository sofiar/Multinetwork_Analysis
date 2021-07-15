#### Extinction simulations with netcascade Viera y Almeida (2014) DOI 10.1111/ele.12394


library(tidyverse)
library(dplyr)
library(infomapecology)
library(gridExtra)
source ('netcascade (April 2014).R')

# Load Networks
NI_Layer_disp17 <- read.csv("~/multicapas_tiño/NI_Layer_disp17.csv", sep=";",row.names=1)
NI_Layer_pol17 <- read.csv("~/multicapas_tiño/NI_Layer_pol17_carn.csv", sep=";",row.names=1)

disp.net=NI_Layer_disp17
pol.net=NI_Layer_pol17

ndisp=dim(disp.net)[1]
npol=dim(pol.net)[1]
nplants=dim(disp.net)[2]
ntot=nplants+npol+ndisp

# create IM
disp_indx=1:ndisp
pol_indx=(ndisp+1):(ndisp+npol)

IM=rbind(NI_Layer_disp17,NI_Layer_pol17)

# Set R values
Rplants=runif(nplants) 
Rpol=runif(npol)
Rdisp=runif(ndisp)
Ranim=c(Rdisp,Rpol)

nsims=100

####################
####  Scenarios ####
####################

# Removing random Species

# a. random plants 
PL.degree=numeric(nsims)
PL.dead.plants=numeric(nsims)
PL.dead.pols=numeric(nsims)
PL.dead.disp=numeric(nsims)

set.seed(999)
bye.plants=sample(1:nplants,size=nsims,replace = TRUE)

for (i in 1:nsims)
{
NC=netcascade(as.matrix(IM),Ranim,Rplants,deadPlants=NULL, deadAnimals=NULL, targetGuild='plant',target=bye.plants[i],return.matrix=F)
PL.degree[i]=max(NC$cascade_data$degree)
PL.dead.plants[i]=NC$cascade_data$n_extinctions[1]
if (is.data.frame(NC$animal_species_data)) # si se murio alguno
{PL.dead.pols[i]=sum(NC$animal_species_data$lost_animal %in% pol_indx)
PL.dead.disp[i]=sum(NC$animal_species_data$lost_animal %in% disp_indx)}
}

# b. random dips
DP.degree=numeric(nsims)
DP.dead.plants=numeric(nsims)
DP.dead.pols=numeric(nsims)
DP.dead.disp=numeric(nsims)

set.seed(9999)
bye.disp=sample(disp_indx,size=nsims,replace = TRUE)

for (i in 1:nsims)
{
  NC=netcascade(as.matrix(IM),Ranim,Rplants,deadPlants=NULL, deadAnimals=NULL, targetGuild='animal',target=bye.disp[i],return.matrix=F)
  DP.degree[i]=max(NC$cascade_data$degree)
  
  DP.dead.pols[i]=sum(NC$animal_species_data$lost_animal %in% pol_indx)
  DP.dead.disp[i]=sum(NC$animal_species_data$lost_animal %in% disp_indx)
  
  if (is.data.frame(NC$plant_species_data)) # si se murio alguna planta
  {
  DP.dead.plants[i]=NC$cascade_data$n_extinctions[2]
  }
}

# c. random pols
PO.degree=numeric(nsims)
PO.dead.plants=numeric(nsims)
PO.dead.pols=numeric(nsims)
PO.dead.disp=numeric(nsims)

set.seed(99999)
bye.pol=sample(pol_indx,size=nsims,replace = TRUE)

for (i in 1:nsims)
{
  NC=netcascade(as.matrix(IM),Ranim,Rplants,deadPlants=NULL, deadAnimals=NULL, targetGuild='animal',target=bye.pol[i],return.matrix=F)
  PO.degree[i]=max(NC$cascade_data$degree)
  
  PO.dead.pols[i]=sum(NC$animal_species_data$lost_animal %in% pol_indx)
  PO.dead.disp[i]=sum(NC$animal_species_data$lost_animal %in% disp_indx)
  
  if (is.data.frame(NC$plant_species_data)) # si se murio alguna planta
  {
    PO.dead.plants[i]=NC$cascade_data$n_extinctions[2]
  }
}

#######################################################
####### Create data frame and summarize results #######
#######################################################


dead.plants=c(PL.dead.plants,DP.dead.plants,PO.dead.plants)
dead.disp=c(PL.dead.disp,DP.dead.disp,PO.dead.disp)
dead.pols=c(PL.dead.pols,DP.dead.pols,PO.dead.pols)
degrees=c(PL.degree,DP.degree,PO.degree)
initial.random=c(rep('Plant',nsims),rep('Disp',nsims),rep('Pol',nsims))

Results=data.frame(initial.random,dead.plants,dead.disp,dead.pols,degrees)


D.plot=ggplot(Results)+geom_bar(aes(x=degree))+facet_grid(~initial.random)+theme_bw()

dead.disp.plot=ggplot(Results)+geom_bar(aes(x=dead.disp,fill=initial.random))+facet_grid(~initial.random)+theme_bw()
dead.plant.plot=ggplot(Results)+geom_bar(aes(x=dead.plants,fill=initial.random))+facet_grid(~initial.random)+theme_bw()
dead.pol.plot=ggplot(Results)+geom_bar(aes(x=dead.pols,fill=initial.random))+facet_grid(~initial.random)+theme_bw()

grid.arrange(dead.disp.plot,dead.pol.plot,dead.plant.plot)
