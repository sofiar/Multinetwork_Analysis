#### Extra functions acá está MIC y se crean los archivos ordenados (edges, layers and layout para la modularidad)

###########ARREGLAR ENLACES ENTRE CAPA!!!!!!!!!!!!######################

#las matrices se cargan directamente del otro archivo!!
##################MODIFIED INDEX COEFFICIENTS########################

#creamos la funcion llamada "Modified Index Coefficient"
ModifiedIndexCoefficient=function(wp,edges.list) #wp es un argumento que despues le asignamos un significado
{
  Nplants= length(wp)#cuenta el numero de filas del vector wp (wp es which plantas-vector con las plantas-)
  O=numeric(Nplants)  #O hace referencia al peso de los enlaces de la planta en la triada
  P=numeric(Nplants) #el P hace referencia al num de triadas 
  for (j in 1:Nplants) #para cada planta(j) en Nplants
  {
    nodeP=wp[j]#selecciona el nodo j y lo llama nodeP
    #Calculo Oj
    O[j]=as.numeric(edges.list %>% filter(node_from==nodeP) %>% summarise(o=sum(weight)))# de la edge.list 
    #va a filtrar las especies (nodeFrom) que interaccinan con (j-nodeP) y va a sumar sus pesos
    #a eso lo va a llamar O de la planta j
    #Calculo PJ
    
    Nperlayer=edges.list %>% filter(node_from==nodeP) %>% group_by(layer_to) %>% dplyr::summarise(N=n())#N es el nombre que le asigna sofi al vector que contiene el reusltado de la funcion n minusculas 
    #lo que hace es filtrar las int con el nodoP (planta j), agruparlas segun la capa destino (pol o disp) y cuenta el num de filas (o sps que interactuan)
    Nperlayer=Nperlayer$N #selecciona el núm de sps que ejerce las int con la planta j de ambas capas.?
    if (length(Nperlayer)>1) #si la planta interacciona con las dos capas, calculame el Pj, si es 1 interacciona solo con una capa
    {
      P[j]=Nperlayer[1]*Nperlayer[2] #multiplicamos el Num de pol y disp q interaccionan
      #con la planta j para obtener el numero de traidas differentes de la sps J.
    }
  }
  Coef=sum(P*O)#/(Nplants*npol*ndisp)
  return(Coef)
}
#

#Pj el numero de triadas de la planta J y Oj es la sumatoria de los pesos de las int
#que conectan con J formando la triada

##########TO PROP FUNCTION! ##############################

ToProp=function(IntNetwork) #de donde salio IntNetwork?? 
{
  NN=sum(IntNetwork)
  out=IntNetwork/NN
  return(out)
}  


# networkwD=IntNetPol
# networkP=simudisp17[,,i]

########## CREATEFILES FUNCTION! ##############################

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
  
  
  for (i in 1:n.plantas)#para cada palnta for 1 a n, fijate los pesos que son positivos #y guardate quienes son esos polinizadores "quienes"
  {
    edges.tpm=networkP[,i]>0 #para la fila uno, ver todos los valores de las columnas que son amyores a 1
    quienes.tmp=which(edges.tpm)
    
    for (j in quienes.tmp) #para cada polinizadores que esta en "quienes",  #guardo el nombre dle nodo y el peso
    {
      nodeFrom=c(nodeFrom,i)
      nodeTo=c(nodeTo,j)
      weight=c(weight,as.numeric(networkP[,i][j]))
    }
    
  }
  
  nodeTo=nodeTo+n.plantas #lo que le digo acá es dejarle un espacio para las plantas y
  #empezar del x (polinizadores van del x al tanto), y lo copiamos en un dataframe:
  
  Edges.List1=as.data.frame(cbind(nodeFrom,nodeTo,weight))
  
  # edge list DispPlanta
  
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
  
  
  ### Extended edges list (pegamos las de pol y disp)
  
  Nlayer1=rep(1,nrow(Edges.List1)) #creamos el vector queva a corresponder a la columna layer 1 (polinizacion) (rep significa que se repite el valor 1 -capa 1- en todas las filas; nrow es la cantidad filas que va a tener el vector ( = cant. de conexiones ne la primer capa
  Nlayer2=rep(2,nrow(Edges.List2))
  
  Extend1=data.frame('layer_from'=as.integer(Nlayer1),
                     "node_from"=as.integer(Edges.List1[,1]),
                     'layer_to'=as.integer(Nlayer1),
                     'node_to'=as.integer(Edges.List1[,2]),
                     'weight'=Edges.List1[,3])
 # la diferencia con toMuxViz es que el orden de las columas varia (acorde al requisito de infomap)
  #  names(Extend1)=c("layer_from","node_from", "layer_to", "node_to", "weight")
  
  
  #Extend2=data.frame(cbind(Nlayer2,Edges.List2[,1],Nlayer2,Edges.List2[,2],Edges.List2[,3]))
  Extend2=data.frame('layer_from'=as.integer(Nlayer2),
                     "node_from"=as.integer(Edges.List2[,1]),
                     'layer_to'=as.integer(Nlayer2),
                     'node_to'=as.integer(Edges.List2[,2]),
                     'weight'=Edges.List2[,3])
  
  # names(Extend2)=c("layer_from","node_from", "layer_to", "node_to", "weight")
  
  # Conexion intercapa
  #seleccionamos aquellas plantas que presente enlaces en ambas capas
  Qplan1=which(vapply(networkD,sum,1)>0)
  Qplan2=which(vapply(networkP,sum,1)>0)
  
  #creamos el vector quienes inter con la identidad de las plantas y cuantificamos
  #las plantas que conectan ambos procesos
  quienes.inter=Qplan2[Qplan2%in%Qplan1] 
  W = vector(mode="numeric", length((quienes.inter))) # acá guardamos los pesos de cada planta considerando ambas capas
  
  #Loop para calcular el peso entre capa de cada planta compartida
  #primero armamos un Edges.list3 con los datos intracapas de ambas capas
  Edges.List3 = rbind(Extend1, Extend2) #interacciones intracapa totales
  
  for (j in seq_along(quienes.inter)){
    nodeP=quienes.inter[j]
    W[j]=as.numeric(Edges.List3 %>% filter(node_from ==nodeP)  %>% summarise(w = sum(weight)))
    #calculamos el peso total intra para cada planta que se comparten en ambas capas 
  } 
  W
  #una vez que tenemos el vector, hay que  sumar los elemetos de ese vector y dividir cada elemento por dicho valor
  tot<-sum(W)#sumamos los elemetos de ese vector
  tot
  
  #dividir cada elemento por la sumatoria y lo guardamos en cuant.inter
  cuan.inter=length(quienes.inter)
  for (i in seq_along(W)){
    cuan.inter[i] = W[i]/tot 
  }
  cuan.inter
  
  #creamos el archivo de interacciones inter
  extendInter=data.frame("layer_from"=as.integer(rep(1,length(cuan.inter))),'node_from'=as.integer(quienes.inter),
                         'layer_to'=as.integer(rep(2,length(cuan.inter))),'node_to'=as.integer(quienes.inter),
                         "weight"=as.numeric(cuan.inter))
  
  #unimos enlaces intra y inter en un archivo
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


