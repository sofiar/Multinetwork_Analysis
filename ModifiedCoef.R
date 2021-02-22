###############################################################################################
################################## Modified index coeficcient #################################
###############################################################################################

ModifiedIndexCoefficient=function(wp,edges.list)
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
Coef=sum(P*O)
return(Coef)
}


##### Calculamos este indice para nuestras redes 

plantesName= c("Ari_chi", "Aza_mic", "Ber_dar", "Col_hys", "Gal_hyp", "Lum_api", "May_boa",
               "May_chu", "Pru_avi", "Rib_mag", "Rub_ida", "Sch_pat", "Tri_cor") # chequear si de esto falta algo


# ingresamos los datos

Edgest.ListNI17 <- read.table("~/multicapas_tiño/implementacion/NI_Extend17.edges", quote="\"", comment.char="")
nodesNI17 <- read.csv("~/multicapas_tiño/implementacion/NI_LayoutInfo17.txt", sep="")

Edgest.ListNI18 <- read.table("~/multicapas_tiño/implementacion/NI_Extend18.edges", quote="\"", comment.char="")
nodesNI18 <- read.csv("~/multicapas_tiño/implementacion/NI_LayoutInfo18.txt", sep="")

Edgest.ListI17 <- read.table("~/multicapas_tiño/implementacion/I_Extend17.edges", quote="\"", comment.char="")
nodesI17 <- read.csv("~/multicapas_tiño/implementacion/I_LayoutInfo17.txt", sep="")

Edgest.ListI18 <- read.table("~/multicapas_tiño/implementacion/I_Extend18.edges", quote="\"", comment.char="")
nodesI18 <- read.csv("~/multicapas_tiño/implementacion/I_LayoutInfo18.txt", sep="")


Edges=list(Edgest.ListNI17,Edgest.ListNI18,Edgest.ListI17,Edgest.ListI18)
Nodes=list(nodesNI17,nodesNI18,nodesI17,nodesI18)
MCI=numeric(length(Edges))
for (n in 1:length(Edges))
{
  Qplantas=which(plantesName %in% Nodes[[n]]$nodeLabel)
  names(Edges[[n]])= c("node_from","layer_from", "node_to", "layer_to", "weight") 
  chau=which(Edges[[n]]$layer_from!=Edges[[n]]$layer_to)
  Edgest.new=Edges[[n]][-chau,]
  
  MCI[n]=ModifiedIndexCoefficient(wp=Qplantas, edges.list=Edgest.new)

}

## data frame to plot

year=rep(c('2017','2018'),2)
Network=rep(c('NI','I'),each=2)

res.MCI=data.frame(MCI,year,Network)


ggplot(res.MCI)+geom_point(aes(y=MCI,x=year,color=In))+theme_bw()
