#### Extinction simulations with netcascade Viera y Almeida (2014) DOI 10.1111/ele.12394

library(tidyverse)
library(dplyr)
library(infomapecology)
library(gridExtra)
source ('netcascade (April 2014).R')

# set treatment
Tre='I'


# Load Networks
NI_pol <- read.csv("~/multicapas_tiño/NI_pol.csv", sep=",",row.names=1)
NI_disp <- read.csv("~/multicapas_tiño/NI_disp.csv", sep=",",row.names=1)
I_disp <- read.csv("~/multicapas_tiño/I_disp.csv", sep=",",row.names=1)
I_pol <- read.csv("~/multicapas_tiño/I_pol.csv", sep=";",row.names=1)

if (Tre=="NI")
{
  disp.net=NI_disp
  pol.net=NI_pol
  
  # Load R values NI
  Rdisp <- read.csv("~/multicapas_tiño/RdispNI.csv")
  Rdisp=Rdisp$x
  Rpol <- read.csv("~/multicapas_tiño/RpolNI.csv")
  Rpol=Rpol$x
  Rplants <- read.csv("~/multicapas_tiño/RplantsNI.csv")
  Rplants=Rplants$x
  Ranim=c(Rdisp,Rpol)
  
}

if (Tre=="I")
{
  disp.net=I_disp
  pol.net=I_pol
  
  # Load R values I
  Rdisp <- read.csv("~/multicapas_tiño/RdispI.csv")
  Rdisp=Rdisp$x
  Rpol <- read.csv("~/multicapas_tiño/RpolI.csv")
  Rpol=Rpol$x
  Rplants <- read.csv("~/multicapas_tiño/RplantsI.csv")
  Rplants=Rplants$x
  Ranim=c(Rdisp,Rpol)
  
}


ndisp=dim(disp.net)[1]
npol=dim(pol.net)[1]
nplants=dim(disp.net)[2]
ntot=nplants+npol+ndisp

# create IM
disp_indx=1:ndisp
pol_indx=(ndisp+1):(ndisp+npol)

IM=rbind(disp.net,pol.net)


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
PL.dead.plants[i]=length(NC$plant_species_data$lost_plant)
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
  DP.dead.plants[i]=length(NC$plant_species_data$lost_plant)
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
    PO.dead.plants[i]=length(NC$plant_species_data$lost_plant)
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
Results=Results %>% mutate(porcDeadPlants=dead.plants/nplants,porcDeadDisp=dead.disp/ndisp,porcDeadPols=dead.pols/npol)

dead.disp.plot=ggplot(Results)+geom_bar(aes(x=dead.disp,fill=initial.random))+facet_grid(~initial.random)+theme_bw()
dead.plant.plot=ggplot(Results)+geom_bar(aes(x=dead.plants,fill=initial.random))+facet_grid(~initial.random)+theme_bw()
dead.pol.plot=ggplot(Results)+geom_bar(aes(x=dead.pols,fill=initial.random))+facet_grid(~initial.random)+theme_bw()

grid.arrange(dead.disp.plot,dead.pol.plot,dead.plant.plot)

# Porcentajes
Pdead.disp.plot=ggplot(Results)+geom_bar(aes(x=porcDeadPols,fill=initial.random))+facet_grid(~initial.random)+theme_bw()
Pdead.plant.plot=ggplot(Results)+geom_bar(aes(x=porcDeadPlants,fill=initial.random))+facet_grid(~initial.random)+theme_bw()
Pdead.pol.plot=ggplot(Results)+geom_bar(aes(x=porcDeadDisp,fill=initial.random))+facet_grid(~initial.random)+theme_bw()

PlotP=grid.arrange(Pdead.disp.plot,Pdead.pol.plot,Pdead.plant.plot)
ggsave(paste('PorcentajesSim1_',as.character(Tre),'.pdf',sep=''),PlotP)

