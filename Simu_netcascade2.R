#### Extinction simulations with netcascade Viera y Almeida (2014) DOI 10.1111/ele.12394

library(tidyverse)
library(dplyr)
library(infomapecology)
library(gridExtra)
source ('netcascade (April 2014).R')
source('ExtraFunctions.R')

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
int.m=as.matrix(IM)

# count number of interactions

n.interactions=as.numeric(c(apply(IM, 1,sum),apply(IM, 2,sum)))



nsims=100

# a) Random Species (1)
# b) Most to least (2)
# c) least to most (3)
scenario=c('random','MtoL','LtoM')

Scenario=c()
reps=c()
AUC=matrix(NA,ncol=3,nrow=nsims)
sp.rm=c()
sp.present=c()

for (j in 1:3)
{
if (scenario[j]=='random')
{
  bye.s=sample(1:ntot,size=nsims,replace = TRUE)  
}
  
if(scenario[j]=='MtoL')
{
  bye.s=rep(which.max(n.interactions),nsims)  
}
  
if(scenario[j]=='LtoM')
{
    bye.s=rep(which.min(n.interactions),nsims)  
}
  
  
for (i in 1:nsims)
{
  #last.dead=bye.s[i]
  all.dead=1
  deadPlants=NULL
  deadAnimals=NULL
  
  n.remove=c()
  n.dead=c()
  deads=NULL
  
  while(all.dead<(ntot-1))
  {
  
    
    #next dead
    if (scenario[j]=='random')
    {
      quienes=(1:ntot)[!(1:ntot)%in% deads]
      last.dead=sample(quienes,1)
    }
    
    if(scenario[j]=='MtoL')
    {
      n.interactionsNA[deads]=-999
      last.dead=which.max(n.interactionsNA)  
    }
    
    if(scenario[j]=='LtoM')
    {
      n.interactionsNA[deads]=9999
      last.dead=which.min(n.interactionsNA)  
    }
    
    
    
    
    
  if(last.dead<=length(Ranim))
  {tg='animal'}
  else
  {tg='plant'
  last.dead=last.dead-npol-ndisp
  }
  n.interactionsNA=n.interactions  
  
  NC=netcascade(imatrix=int.m,ranim=Ranim,rplants=Rplants,deadPlants=deadPlants, deadAnimals=deadAnimals, targetGuild=tg ,target=last.dead,return.matrix=T)
  
  deadPlants=c(deadPlants,NC$lost_plants)
  deadAnimals=c(deadAnimals,NC$lost_animals)
  all.dead=sum(length(deadAnimals),length(deadPlants))
  deads=c(deadAnimals,deadPlants+npol+ndisp)
  n.remove=c(n.remove,1)
  n.dead=c(n.dead,sum(length(NC$lost_plants),length(NC$lost_animals)))
  
  }

  sp.rms=c(0,cumsum(n.remove))/max(cumsum(n.remove))
  sp.presents=c(ntot,sum(n.dead)-cumsum(n.dead))/ntot
  
  Scenario=c(Scenario,rep(scenario[j],length(sp.rms)))
  reps=c(reps,rep(i,length(sp.rms)))
  AUC[i,j]=auc(sp.rms,sp.presents)
  sp.rm=c(sp.rm,sp.rms)
  sp.present=c(sp.present,sp.presents)
  
  }

}



#### Create data frame with the results

reps2=rep(1:nsims,3)
Scenario2=rep(scenario,each=nsims)
Auc=as.vector(AUC)
Results.AUC=data.frame(reps=reps2,Scenario=Scenario2,Auc)  

Results.ce=data.frame(reps,Scenario,sp.rm,sp.present)


### Figures ggplot

aucplot=ggplot(Results.AUC)+geom_boxplot(aes(y=Auc,x=Scenario,color=Scenario))+theme_bw()
ggsave(paste('auc_',as.character(Tre),'.pdf',sep=''),aucplot)



plot.ce=ggplot(Results.ce)+geom_point(aes(x=sp.rm,y=sp.present,color=as.factor(reps)))+
  geom_line(aes(x=sp.rm,y=sp.present,color=as.factor(reps)))+
  facet_grid(~Scenario)+theme_bw()+theme(legend.position =  'none')
ggsave(paste('ce_',as.character(Tre),'.pdf',sep=''),plot.ce)



