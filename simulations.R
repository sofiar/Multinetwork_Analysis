#### simulaciones

library(dplyr)
library(vegan)
library(infomapecology)
source('/home/sofi/multicapas_tiño/implementacion/ExtraFunctions.R')


# cargamos los datos

NI_Layer_disp17 <- read.csv("~/multicapas_tiño/implementacion/IntegerData/NI_Layer_disp17.csv", sep=";",row.names=1)
NI_Layer_pol17 <- read.csv("~/multicapas_tiño/implementacion/IntegerData/NI_Layer_pol17_carn.csv", sep=";",row.names=1)


NI_Layer_disp17[apply(NI_Layer_disp17[,-1], 1, function(x) !all(x==0)),]
NI_Layer_pol17[apply(NI_Layer_pol17[,-1], 1, function(x) !all(x==0)),]



########### Calculate Real values

# tofreq
RealfNetPol=ToFreq(NI_Layer_pol17)
RealfNetDisp=ToFreq(NI_Layer_disp17)


### Modularidad

files=CreateFiles(RealfNetDisp,RealfNetPol)
Edges=files$Edges.info
Nodes=files$Nodes.info
Layers=files$Layers.info

# creamos la para infomap
RealNetwork= create_multilayer_object(extended = Edges,nodes=Nodes,
                                  intra_output_extended = TRUE,layers=Layers)

Realsalida=run_infomap_multilayer(RealNetwork, relax = F, silent = T, trials = 100,seed=1111, 
                              temporal_network = F,flow_model='undirected')

MReal=Realsalida$m
LReal=Realsalida$L

MReal

#####################################################################################
############################# Intra-layer null model ################################
#####################################################################################

#‘r00_both’ algorithm: maintains the number of links, while redistributing the 
# individual interactions independently for each layer

nsim=1000

polnames=rownames(NI_Layer_pol17)
dispnames=rownames(NI_Layer_disp17)


nmpol17 <- vegan::nullmodel(NI_Layer_pol17, method = "r00_both")
nmdisp17 <- vegan::nullmodel(NI_Layer_disp17, method = "r00_both")

set.seed(11)
simupol17 <- simulate(nmpol17, nsim=nsim)
simudisp17 <- simulate(nmdisp17, nsim=nsim)

L=numeric(nsim)
M=numeric(nsim)


for (i in 1:nsim)
{
  IntNetPol=simupol17[,,i]
  IntNetDisp=simudisp17[,,i]
  
  IntNetPol=as.data.frame(IntNetPol)
  IntNetPol=IntNetPol[apply(IntNetPol[,-1], 1, function(x) !all(x==0)),]  
  
  IntNetDisp=as.data.frame(IntNetDisp)
  IntNetDisp=IntNetDisp[apply(IntNetDisp[,-1], 1, function(x) !all(x==0)),]  
  
  # tofreq
  fNetPol=ToFreq(IntNetPol)
  fNetDisp=ToFreq(IntNetDisp)
  
  
  ### modularidad
  # networkD=fNetDisp
  # networkP=fNetPol
  files=CreateFiles(fNetDisp,fNetPol)
  Edges=files$Edges.info
  Nodes=files$Nodes.info
  Layers=files$Layers.info
  
  # creamos la para infomap
  Network= create_multilayer_object(extended = Edges,nodes=Nodes,
                                    intra_output_extended = TRUE,layers=Layers)
  
  salida=run_infomap_multilayer(Network, relax = F, silent = T, trials = 100, 
                                 temporal_network = F,flow_model='undirected')
  
  
  L[i]=salida$L
  M[i]=salida$m
  
  
  ### MDCI
  
  
  

}
 

ResultsSimus=data.frame("M"=M,"L"=L)
  
ggplot(ResultsSimus)+geom_bar(aes(x=M))+theme_bw()
 
