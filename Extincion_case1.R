#### Extinction simulations with netcascade Viera y Almeida (2014) DOI 10.1111/ele.12394
#CHEQUEAR DEGREE!
library(tidyverse)
library(dplyr)
library(infomapecology)
library(gridExtra)
source ('netcascade_Vieira.R')

# Load Networks
#NI_disp <- read.csv("C:\\Users\\agust\\Desktop\\Tinio\\Papers\\Paper_multicapa\\Matrices_consecos\\Matrices\\Combinado\\NI_disp.csv", sep=",",row.names=1)
#NI_pol <- read.csv("C:\\Users\\agust\\Desktop\\Tinio\\Papers\\Paper_multicapa\\Matrices_consecos\\Matrices\\Combinado\\NI_pol.csv", sep=",",row.names=1)

I_disp <- read.csv("C:\\Users\\agust\\Desktop\\Tinio\\Papers\\Paper_multicapa\\Matrices_consecos\\Matrices\\Combinado\\I_disp.csv", sep=",",row.names=1)
I_pol <- read.csv("C:\\Users\\agust\\Desktop\\Tinio\\Papers\\Paper_multicapa\\Matrices_consecos\\Matrices\\Combinado\\I_pol.csv", sep=",",row.names=1)

disp.net=I_disp
pol.net=I_pol

ndisp=dim(disp.net)[1]
npol=dim(pol.net)[1]
nplants=dim(disp.net)[2]
ntot=nplants+npol+ndisp

# create IM (creamos matrix total pegando los disp arriba de los pol)
disp_indx=1:ndisp
pol_indx=(ndisp+1):(ndisp+npol)

IM=rbind(I_disp,I_pol) 

# Set R values
#plants
Tplants <-read.csv("C:\\Users\\agust\\Desktop\\Tinio\\Papers\\Paper_multicapa\\Matrices_consecos\\Analisis_Combinado\\Extincion\\Species_traits\\traits_plants.csv", sep=";")
Tplants2<-Tplants[,c(1,7)]

plant.common=colnames(IM)[colnames(IM)%in%Tplants2[,1]]
Rplants<- Tplants2 %>% 
  filter(Plant_name%in%plant.common)
Rplants<- Rplants[,2]

#pol
Rpol=runif(npol)

#disp
Tdisp <-read.csv("C:\\Users\\agust\\Desktop\\Tinio\\Papers\\Paper_multicapa\\Matrices_consecos\\Analisis_Combinado\\Extincion\\Species_traits\\traits_frug.csv", sep=";")
Tdisp2<-Tdisp[,c(1,7)]
disp.common=row.names(disp.net)[row.names(disp.net)%in%Tdisp2[,1]]
Rdisp<- Tdisp2 %>% 
  filter(Seeddisp_name%in%disp.common)
Rdisp<-Rdisp[,2]

Ranim=c(Rdisp,Rpol)#consideramos a los pol y disp como animales (y despues distinguimos si son pol o disp)

nsims=200

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
bye.plants=sample(1:nplants,size=nsims,replace = TRUE)#vector que contiene las 100 plantas a sacar al azar

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

#Plot extinction-degree
D.plot=ggplot(Results)+geom_bar(aes(x=degrees))+facet_grid(~initial.random)+theme_bw()
D.plot
#ggsave("Extinctiondegree_NI.png")

#Plot extinction-deads
dead.disp.plot=ggplot(Results)+geom_bar(aes(x=dead.disp,fill=initial.random))+facet_grid(~initial.random)+theme_bw()
dead.plant.plot=ggplot(Results)+geom_bar(aes(x=dead.plants,fill=initial.random))+facet_grid(~initial.random)+theme_bw()
dead.pol.plot=ggplot(Results)+geom_bar(aes(x=dead.pols,fill=initial.random))+facet_grid(~initial.random)+theme_bw()

#pdf("Extinctiondeads_I.pdf", height = 11, width = 8.5, paper = "letter")
grid.arrange(dead.disp.plot,dead.pol.plot,dead.plant.plot)
#dev.off()
