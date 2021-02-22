###### Analisis de sensibilidad Relax rate (miramos los datos de 2017)

library(infomapecology)
library(bipartite)
library(tidyverse)

# 1.  Cargo los datos 

# Sitio NO invadido

Edgest.ListNI <- read.table("~/multicapas_tiño/implementacion/NI_Extend17.edges", quote="\"", comment.char="")
nodesNI <- read.csv("~/multicapas_tiño/implementacion/NI_LayoutInfo17.txt", sep="")
LayersNI <- read.csv("~/multicapas_tiño/implementacion/NI_LayersInfo17.txt", sep="")


# Sitio invadido
# PolPlantaI <- read.csv("~/multicapas_tiño/implementacion/I_Layer_pol17_carn.csv", sep=";",row.names = 1)
# DispPlantaI <- read.csv("~/multicapas_tiño/implementacion/I_Layer_disp17.csv", sep=";",row.names=1)

Edgest.ListI <- read.table("~/multicapas_tiño/implementacion/I_Extend17.edges", quote="\"", comment.char="")
nodesI <- read.csv("~/multicapas_tiño/implementacion/I_LayoutInfo17.txt", sep="")
LayersI <- read.csv("~/multicapas_tiño/implementacion/I_LayersInfo17.txt", sep="")

# 2. Reordenamos las columnas para infomapecology: layer_from; node_from; layer_to; node_to; weight
Edgest.ListNI= Edgest.ListNI[,c(2,1,4,3,5)] 
names(Edgest.ListNI)= c("layer_from","node_from", "layer_to", "node_to", "weight") 
names(nodesNI)=c('node_id','name_node')
names(LayersNI) =c('layer_id','name_layer') 

Edgest.ListI= Edgest.ListI[,c(2,1,4,3,5)] 
names(Edgest.ListI)= c("layer_from","node_from", "layer_to", "node_to", "weight") 
names(nodesI)=c('node_id','name_node')
names(LayersI) =c('layer_id','name_layer') 

# 3. Sacamos los pesos intercapa
chauNI=which(Edgest.ListNI$layer_from!=Edgest.ListNI$layer_to)
Edgest.ListNI.new=Edgest.ListNI[-chauNI,]

chauI=which(Edgest.ListI$layer_from!=Edgest.ListI$layer_to)
Edgest.ListI.new=Edgest.ListI[-chauI,]

# 4. Creamos las redes
NetworkNI= create_multilayer_object(extended = Edgest.ListNI.new,nodes=nodesNI,intra_output_extended = FALSE,layers=LayersNI)
NetworkI= create_multilayer_object(extended = Edgest.ListI.new,nodes=nodesI,intra_output_extended = FALSE,layers=LayersI)

# 5. Analisis

r=seq(0,1,length.out=20)

LsNI=numeric(20)
MsNI=numeric(20)
LsI=numeric(20)
MsI=numeric(20)
percentegeNI=list()
percentegeI=list()


for (i in 1:length(r))
{
  salidaNI=run_infomap_multilayer(NetworkNI, relax = T, silent = T, trials = 50, 
                                 temporal_network = F,flow_model='undirected',multilayer_relax_rate = r[i])
  
  salidaI=run_infomap_multilayer(NetworkI, relax = T, silent = T, trials = 50, 
                                  temporal_network = F,flow_model='undirected',multilayer_relax_rate = r[i])
  
  LsNI[i]=salidaNI$L
  MsNI[i]=salidaNI$m
  
  LsI[i]=salidaI$L
  MsI[i]=salidaI$m
  
  Modulos=salidaNI$modules
  nbyMod=Modulos %>% group_by(module) %>% tally()
  percentegeNI[[i]]=nbyMod$n/sum(nbyMod$n)
  
  
  Modulos=salidaI$modules
  nbyMod=Modulos %>% group_by(module) %>% tally()
  percentegeI[[i]]=nbyMod$n/sum(nbyMod$n)
  
  
  }


# Create data frame to plot results with ggplot

network=rep(c('NI','I'),each=20)

RrateResults=data.frame(cbind(as.factor(network),r=rep(r,2),NM=c(MsNI,MsI),L=c(LsNI,LsI)))

SRrate.NM=ggplot(RrateResults) + geom_point(aes(x=r,y=NM,color=network))+geom_line(aes(x=r,y=NM,color=network))+
  theme_bw()+ylab('Number of modules')+xlab('Relax rate')

#ggsave('./implementacion/SRrate.NM.png',SRrate.NM)

SRrate.L=ggplot(RrateResults) + geom_point(aes(x=r,y=L,color=network))+geom_line(aes(x=r,y=L,color=network))+
  theme_bw()+ylab('L value')+xlab('Relax rate')

#ggsave('./implementacion/SRrate.L.png',SRrate.L)

# Percent of species per module
percentNI=c()
percentI=c()
ModulesNI=c()
ModulesI=c()
RrateNI=c()
RrateI=c()



for (i in 1:20)
{
  percentNI=c(percentNI,percentegeNI[[i]])
  ModulesNI=c(ModulesNI,seq(1,MsNI[i]))
  
  percentI=c(percentI,percentegeI[[i]])
  ModulesI=c(ModulesI,seq(1,MsI[i]))
  
  RrateNI=c(RrateNI,rep(r[i],MsNI[i]))
  RrateI=c(RrateI,rep(r[i],MsI[i]))
  
}




Percent.res=data.frame(cbind(percent=c(percentNI,percentI),nmoduls=c(ModulesNI,ModulesI),
                             Rrate=c(RrateNI,RrateI),
                             network=c(rep("NI",length(percentNI)),rep("I",length(percentI)))))

Percent.res$nmoduls=as.factor(Percent.res$nmoduls)
Percent.res$Rrate=as.factor(Percent.res$Rrate)


levels(Percent.res$Rrate)=c('0','0.05',"0.1","0.15","0.21","0.26","0.31","0.36","0.42","0.47","0.52",
                    "0.57","0.63","0.68","0.73","0.78","0.84","0.89","0.94",'1')

levels(Percent.res$nmoduls)
Percent.res=Percent.res %>% mutate(Nmoduls=fct_relevel(Percent.res$nmoduls,c("10","11"),after=c(10,11)))


Percent.plot=ggplot(Percent.res,aes(x=Rrate,fill=Nmoduls))+geom_bar(position = "fill") +facet_wrap(~network,ncol=1)+theme_bw()+
  xlab('Relax rate')+ylab('Percent of species')

#ggsave('./implementacion/Percent.png',Percent.plot,width = 10,height=8)
