#### simulaciones

setwd('/home/sofi/multicapas_tiño')
library(dplyr)
library(vegan)
library(infomapecology)
library(bipartite)
source('/home/sofi/multicapas_tiño/implementacion/ExtraFunctions.R')


# cargamos los datos
#2017
NI_Layer_disp17 <- read.csv("~/multicapas_tiño/implementacion/IntegerData/NI_Layer_disp17.csv", sep=";",row.names=1)
NI_Layer_pol17 <- read.csv("~/multicapas_tiño/implementacion/IntegerData/NI_Layer_pol17_carn.csv", sep=";",row.names=1)

I_Layer_disp17 <- read.csv("~/multicapas_tiño/implementacion/IntegerData/I_Layer_disp17.csv", sep=";",row.names=1)
I_Layer_pol17 <- read.csv("~/multicapas_tiño/implementacion/IntegerData/I_Layer_pol17carn.csv", sep=";",row.names=1)

#2018

NI_Layer_disp18 <- read.csv("~/multicapas_tiño/implementacion/IntegerData/NI_Layer_disp18.csv", row.names=1)
NI_Layer_pol18 <- read.csv("~/multicapas_tiño/implementacion/IntegerData/NI_Layer_pol18_carn.csv", sep=";",row.names=1)


I_Layer_disp18 <- read.csv("~/multicapas_tiño/implementacion/IntegerData/I_Layer_disp18.csv",row.names=1)
I_Layer_pol18 <- read.csv("~/multicapas_tiño/implementacion/IntegerData/I_Layer_pol18carn.csv", sep=";",row.names=1)


########### Calculate Real values


Ndisp=NI_Layer_disp17
Npol=NI_Layer_pol17


Ndisp=Ndisp[apply(Ndisp[,-1], 1, function(x) !all(x==0)),]
Npol=Npol[apply(Npol[,-1], 1, function(x) !all(x==0)),]
NpolE=empty(Npol)
NdispE=empty(Ndisp)

# toprop
RealfNetPol=ToProp(Npol)
RealfNetDisp=ToProp(Ndisp)


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


##### MIC

plantesName= c("Ari_chi", "Aza_mic", "Ber_dar", "Col_hys", "Gal_hyp", "Lum_api", "May_boa",
               "May_chu", "Pru_avi", "Rib_mag", "Rub_ida", "Sch_pat", "Tri_cor") # chequear si de esto falta algo

wp=which(plantesName %in% Nodes$name_node)
Edges2=Edges %>% select(node_from,layer_from,node_to,layer_to,weight)
chau=which(Edges2$layer_from!=Edges2$layer_to)
Edges.new=Edges2[-chau,]

Real_MIC=ModifiedIndexCoefficient(wp,edges.list=Edges.new)

#####################################################################################
############################# Intra-layer null model ################################
#####################################################################################

#‘r00_both’ algorithm: maintains the number of links, while redistributing the 
# individual interactions independently for each layer

nsim=1000

nmpol <- vegan::nullmodel(NpolE, method = "r00_both")
nmdisp <- vegan::nullmodel(NdispE, method = "r00_both")

set.seed(11)
simupol <- simulate(nmpol, nsim=nsim*2)
simudisp <- simulate(nmdisp, nsim=nsim*2)

L=numeric(nsim)
M=numeric(nsim)
NN=1

MIC=numeric(nsim)

for (i in 1:(nsim*2))
{
  if (NN<=nsim)
  {
    IntNetPol=simupol[,,i]
    IntNetDisp=simudisp[,,i]
    
    IntNetPol=as.data.frame(IntNetPol)
    IntNetPol=IntNetPol[apply(IntNetPol[,-1], 1, function(x) !all(x==0)),]  
    IntNetDisp=as.data.frame(IntNetDisp)
    IntNetDisp=IntNetDisp[apply(IntNetDisp[,-1], 1, function(x) !all(x==0)),]  
    
    
    # agregamoa las plantas que no estan en alguna de las dos capas (yo soy capa)
    plantas=unique(c(colnames(IntNetPol),colnames(IntNetDisp)))
    pln.extraPol=plantas[!plantas %in% colnames(IntNetPol)] # las que no estan en polinizacion
    pln.extraDisp=plantas[!plantas %in% colnames(IntNetDisp)] # las que no estan en dispersion
    
    IntNetDisp2=data.frame(IntNetDisp,matrix(0,nrow(IntNetDisp),length(pln.extraDisp)))
    if (length(pln.extraDisp)>0)
    {names(IntNetDisp2)[(ncol(IntNetDisp)+1):length(plantas)]=pln.extraDisp}
    
    IntNetPol2=data.frame(IntNetPol,matrix(0,nrow(IntNetPol),length(pln.extraPol)))
    if (length(pln.extraPol)>0)
    {names(IntNetPol2)[(ncol(IntNetPol)+1):length(plantas)]=pln.extraPol}
    
    fNetPol=ToProp(IntNetPol2)
    fNetDisp=ToProp(IntNetDisp2)
    
    # fNetPol=ToProp(IntNetPol)
    # fNetDisp=ToProp(IntNetDisp)
    # 
    
    
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
    
    
    
    if (!is.na(salida$m))
    {
      L[NN]=salida$L
      M[NN]=salida$m
      ### MDCI
      wp=which(plantesName %in% Nodes$name_node)
      Edges2=Edges %>% select(node_from,layer_from,node_to,layer_to,weight)
      chau=which(Edges2$layer_from!=Edges2$layer_to)
      Edges.new=Edges2[-chau,]
      
      MIC[NN]=ModifiedIndexCoefficient(wp,edges.list=Edges.new)
      
      NN=NN+1
      
      
    }
    
  }
  
  else
  {break}
  
  }


 

ResultsSimus=data.frame("M"=as.integer(M),"L"=L,"MIC"=MIC)
ResultsSimus$MIC

write.csv(ResultsSimus,file='HT_NI2017.csv')

ggplot(ResultsSimus)+geom_bar(aes(x=M))+theme_bw() +geom_vline(xintercept =MReal,linetype = "dashed")

ggplot(ResultsSimus)+geom_histogram(aes(x=MIC))+theme_bw()+geom_vline(xintercept =Real_MIC,linetype = "dashed")





