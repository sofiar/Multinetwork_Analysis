#### Extra functions

ModifiedIndexCoefficient=function(wp,edges.list,npol,ndisp)
{
  Nplants= length(wp)
  O=numeric(Nplants)  
  P=numeric(Nplants)
  for (j in 1:Nplants)
  {
    nodeP=wp[j]
    O[j]=as.numeric(edges.list %>% filter(node_from==nodeP) %>% summarise(o=sum(weight)))
    Nperlayer=edges.list %>% filter(node_from==nodeP) %>% group_by(layer_to) %>% summarise(N=n())
    Nperlayer=Nperlayer$N
    if (length(Nperlayer)>1)
    {
      P[j]=Nperlayer[1]*Nperlayer[2]
    }
  }
  Coef=sum(P*O)/(Nplants*npol*ndisp)
  return(Coef)
}



ToProp=function(IntNetwork)
{
NN=sum(IntNetwork)
out=IntNetwork/NN
return(out)
}  
# networkwD=IntNetPol
# networkP=simudisp17[,,i]

CreateFiles=function(networkD,networkP)
{
  require(tidyverse)
  Plantas=names(networkP)
  Polinizadores=row.names(networkP)
  Dispersores=row.names(networkD)
  
  #mismo orden para la plantas
  networkD=networkD %>% select(Plantas)
  
  n.plantas=length(Plantas)
  n.polinizadores=length(Polinizadores)
  n.dispersores=length(Dispersores)
  n.nodos=n.plantas+n.polinizadores+n.dispersores
  
  # PolPLanta

  nodeFrom=c()
  nodeTo=c()
  weight=c()
  
  
  for (i in 1:n.plantas)
  {
    edges.tpm=networkP[,i]>0
    quienes.tmp=which(edges.tpm)
    
    for (j in quienes.tmp)
    {
      nodeFrom=c(nodeFrom,i)
      nodeTo=c(nodeTo,j)
      weight=c(weight,as.numeric(networkP[,i][j]))
    }
    
  }
  
  nodeTo=nodeTo+n.plantas
  
  Edges.List1=as.data.frame(cbind(nodeFrom,nodeTo,weight))
  
  # DispPlanta
  
  nodeFrom=c()
  nodeTo=c()
  weight=c()
  
  
  for (i in 1:n.plantas)
  {
    edges.tpm=networkD[,i]>0
    quienes.tmp=which(edges.tpm)
    
    for (j in quienes.tmp)
    {
      nodeFrom=c(nodeFrom,i)
      nodeTo=c(nodeTo,j)
      weight=c(weight,as.numeric(networkD[,i][j]))
    }
    
  }
  
  nodeTo=nodeTo+n.plantas+n.polinizadores
  
  Edges.List2=as.data.frame(cbind(nodeFrom,nodeTo,weight))
  
  
  ### Extended edges list
  
  Nlayer1=rep(1,nrow(Edges.List1))
  Nlayer2=rep(2,nrow(Edges.List2))
  
  Extend1=data.frame('layer_from'=as.integer(Nlayer1),
                     "node_from"=as.integer(Edges.List1[,1]),
                     'layer_to'=as.integer(Nlayer1),
                     'node_to'=as.integer(Edges.List1[,2]),
                     'weight'=Edges.List1[,3])
#  names(Extend1)=c("layer_from","node_from", "layer_to", "node_to", "weight")
  
  
  #Extend2=data.frame(cbind(Nlayer2,Edges.List2[,1],Nlayer2,Edges.List2[,2],Edges.List2[,3]))
  Extend2=data.frame('layer_from'=as.integer(Nlayer2),
                     "node_from"=as.integer(Edges.List2[,1]),
                     'layer_to'=as.integer(Nlayer2),
                     'node_to'=as.integer(Edges.List2[,2]),
                     'weight'=Edges.List2[,3])
  
  
  # names(Extend2)=c("layer_from","node_from", "layer_to", "node_to", "weight")
  # Conexion intercapa
  
  Qplan1=which(vapply(networkD,sum,1)>0)
  Qplan2=which(vapply(networkP,sum,1)>0)
  
  quienes.inter=Qplan2[Qplan2%in%Qplan1]
  cuan.inter=length(quienes.inter)
  
  extendInter=data.frame("layer_from"=as.integer(rep(1,cuan.inter)),'node_from'=as.integer(quienes.inter),
                         'layer_to'=as.integer(rep(2,cuan.inter)),'node_to'=as.integer(quienes.inter),
                         "weight"=rep(1,cuan.inter))
  
  
  AllExtend=rbind(Extend1,Extend2,extendInter)
  
  ### Layers info 
  layerID=c(1,2)
  layerLabel=c("PlantaPol","PlantaDisp")
  LayerInfo=data.frame('layer_id'=as.integer(layerID),'name_layer'=layerLabel)
  
  
  ### Nodes info 
  
  nodeID=seq(1,n.nodos) #  Primeros plantas , luego polinizadores , luego dispersores 
  nodeLabel=c(Plantas,Polinizadores,Dispersores)
  NodesInfo=data.frame('node_id'=nodeID,'name_node'=nodeLabel)
  
  return(list(Edges.info=AllExtend,Layers.info=LayerInfo,Nodes.info=NodesInfo))

}
