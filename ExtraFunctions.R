#### Extra functions

ModifiedIndexCoefficient=function(wp,edges.list)
{
  Nplants= length(wp)
  O=numeric(Nplants)  
  P=numeric(Nplants)
  for (j in 1:Nplants)
  {
    nodeP=wp[j]
    O[j]=as.numeric(edges.list %>% filter(node_from==nodeP) %>% summarise(o=sum(weight)))
    Nperlayer=edges.list %>% filter(node_from==nodeP) %>% group_by(layer_to) %>% dplyr::summarise(N=n())
    Nperlayer=Nperlayer$N
    if (length(Nperlayer)>1)
    {
      P[j]=Nperlayer[1]*Nperlayer[2]
    }
  }
  Coef=sum(P*O)#/(Nplants*npol*ndisp)
  return(Coef)
}



ToProp=function(IntNetwork)
{
NN=sum(IntNetwork)
out=IntNetwork/NN
return(out)
}  



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
  
  # Edges lists PolPLanta
  
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
  

  Nlayer1=rep(1,nrow(Edges.List1))
  Nlayer2=rep(2,nrow(Edges.List2))
  
  Extend1=data.frame('layer_from'=as.integer(Nlayer1),
                     "node_from"=as.integer(Edges.List1[,1]),
                     'layer_to'=as.integer(Nlayer1),
                     'node_to'=as.integer(Edges.List1[,2]),
                     'weight'=Edges.List1[,3])
 
  Extend2=data.frame('layer_from'=as.integer(Nlayer2),
                     "node_from"=as.integer(Edges.List2[,1]),
                     'layer_to'=as.integer(Nlayer2),
                     'node_to'=as.integer(Edges.List2[,2]),
                     'weight'=Edges.List2[,3])
  

  Qplan1=which(vapply(networkD,sum,1)>0)
  Qplan2=which(vapply(networkP,sum,1)>0)
  
  quienes.inter=Qplan2[Qplan2%in%Qplan1] 
  W = vector(mode="numeric", length((quienes.inter))) 
  
  Edges.List3 = rbind(Extend1, Extend2) 
  
  for (j in seq_along(quienes.inter)){
    nodeP=quienes.inter[j]
    W[j]=as.numeric(Edges.List3 %>% filter(node_from ==nodeP)  %>% summarise(w = sum(weight)))
    
  } 
  
 
  tot<-sum(W)
 
  cuan.inter=length(quienes.inter)
  for (i in seq_along(W)){
    cuan.inter[i] = W[i]/tot 
  }
 
  
  extendInter=data.frame("layer_from"=as.integer(rep(1,length(cuan.inter))),'node_from'=as.integer(quienes.inter),
                         'layer_to'=as.integer(rep(2,length(cuan.inter))),'node_to'=as.integer(quienes.inter),
                         "weight"=as.numeric(cuan.inter))

  AllExtend=rbind(Extend1,Extend2,extendInter)
  
  ### Layers info 
  layerID=c(1,2)
  layerLabel=c("PlantaPol","PlantaDisp")
  LayerInfo=data.frame('layer_id'=as.integer(layerID),'name_layer'=layerLabel)
  
  
  ### Nodes info 
  
  nodeID=seq(1,n.nodos) #
  nodeLabel=c(Plantas,Polinizadores,Dispersores)
  NodesInfo=data.frame('node_id'=nodeID,'name_node'=nodeLabel)
  
  return(list(Edges.info=AllExtend,Layers.info=LayerInfo,Nodes.info=NodesInfo))
  
  
  
  
}





auc=function(rem,pre){
  y <- pre
  x <- rem
  ext.curve <- splinefun(x, y)
  ext.area <- integrate(ext.curve, 0, 1)
  return(as.numeric(ext.area[[1]]))
}
