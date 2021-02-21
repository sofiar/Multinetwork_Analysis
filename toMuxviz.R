#### Creamos un script para generar los archivos para ingresar al muxviz

library(tidyverse)

# 1. Abrimos archivos
PolPlanta <- read.csv("~/multicapas_tiño/implementacion/NI_Layer_pol17_carn.csv", sep=";",row.names = 1)
DispPlanta <- read.csv("~/multicapas_tiño/implementacion/NI_Layer_disp17.csv", sep=";",row.names=1)

# 2. Guardamos informacion  
Plantas=names(PolPlanta)
Polinizadores=row.names(PolPlanta)
Dispersores=row.names(DispPlanta)

DispPlanta=DispPlanta %>% select(Plantas)

n.plantas=length(Plantas)
n.polinizadores=length(Polinizadores)
n.dispersores=length(Dispersores)
n.nodos=n.plantas+n.polinizadores+n.dispersores

# Creamos Layoutfile (Chequear nombre de las plantas)

nodeID=seq(1,n.nodos) #  Primeros plantas (13), luego polinizadores (40), dispersores (3)
nodeLabel=c(Plantas,Polinizadores,Dispersores)
layoutInfo=cbind(nodeID,nodeLabel)

write.table(layoutInfo, "LayoutInfo.txt", append = FALSE, sep = " ", dec = ".",
            row.names = FALSE, col.names = TRUE,quote=FALSE)

# creamos las edges list PolPlanta

nodeFrom=c()
nodeTo=c()
weight=c()


for (i in 1:n.plantas)
{
  edges.tpm=PolPlanta[,i]>0
  quienes.tmp=which(edges.tpm)
 
  for (j in quienes.tmp)
  {
  nodeFrom=c(nodeFrom,i)
  nodeTo=c(nodeTo,j)
  weight=c(weight,as.numeric(PolPlanta[,i][j]))
  }
  
}


nodeTo=nodeTo+n.plantas

Edges.List1=as.data.frame(cbind(nodeFrom,nodeTo,weight))

# Exportamos a un archivo 
# write.table(Edges.List, "PlantaPol.edges", append = FALSE, sep = " ", dec = ".",
#             row.names = FALSE, col.names = TRUE)



# creamos las edges list plantaDispersor. Chequear orden de las plantas

nodeFrom=c()
nodeTo=c()
weight=c()


for (i in 1:n.plantas)
{
  edges.tpm=DispPlanta[,i]>0
  quienes.tmp=which(edges.tpm)
  
  for (j in quienes.tmp)
  {
    nodeFrom=c(nodeFrom,i)
    nodeTo=c(nodeTo,j)
    weight=c(weight,as.numeric(DispPlanta[,i][j]))
  }
  
}



nodeTo=nodeTo+n.plantas+n.polinizadores

Edges.List2=as.data.frame(cbind(nodeFrom,nodeTo,weight))

# Exportamos a un archivo 
# write.table(Edges.List, "PlantaDisp.edges", append = FALSE, sep = " ", dec = ".",
#             row.names = FALSE, col.names = TRUE)


### Extended edges list


Nlayer1=rep(1,nrow(Edges.List1))
Nlayer2=rep(2,nrow(Edges.List2))

Extend1=cbind(Edges.List1[,1],Nlayer1,Edges.List1[,2],Nlayer1,Edges.List1[,3])
Extend2=cbind(Edges.List2[,1],Nlayer2,Edges.List2[,2],Nlayer2,Edges.List2[,3])
# Conexion intercapa
Disp
Plantas

Qplan1=which(vapply(DispPlanta,sum,1)>0)
Qplan2=which(vapply(PolPlanta,sum,1)>0)

quienes.inter=Qplan2[Qplan2%in%Qplan1]
cuan.inter=length(quienes.inter)

extendInter=cbind(quienes.inter,rep(1,cuan.inter),quienes.inter,rep(2,cuan.inter),rep(1,cuan.inter))


AllExtend=rbind(Extend1,Extend2,extendInter)

write.table(AllExtend, "Extend.edges", append = FALSE, sep = " ", dec = ".",
            row.names = FALSE, col.names = FALSE)

# Layer info file
layerID=c(1,2)
layerLabel=c("PlantaPol","PlantaDisp")
LayerInfo=cbind(layerID,layerLabel)

write.table(LayerInfo, "LayersInfo.txt", append = FALSE, sep = " ", dec = ".",
            row.names = FALSE, col.names = TRUE,quote=FALSE)

