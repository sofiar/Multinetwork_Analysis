### Extinction Simualation

################################
## parametros de la simulacion
###############################

tempo=100 # determinar criterio

########################
## parametros de la red
########################
K=30 # numero total de especies

R_plants=runif(10,0,1)
R_pols=runif(10,0,1)
R_disp=runif(10,0,1)

R=c(R_plants,R_pols,R_disp)
## Supongamos matriz de intereccion
MI=matrix(data=runif(K^2),nrow=K,ncol=K)
diag(MI)=rep(0,K)
#simetrica
MI=MI%*% t(MI)
MI=MI/max(MI)
MI.chau=MI


#Probability matrix
PE=matrix(data=NA,nrow=K,ncol=K)
DP=matrix(data=NA,nrow=K,ncol=K)
sp.out=list()

sp.out[[1]]=sample(1:10,1)
sp.chau=c(sp.out[[1]])
sp.o=sp.out[[1]]

for (t in 1:tempo)
{
  sp.out[[t+1]]=0
  sp.new=c()
  for (k in sp.o)
  {
  for (j in ((1:K)[-c(sp.chau,sp.o)]))
  {

    PE[j,k]=MI[j,k]/sum(MI[j,-sp.chau])
  }
  ps=PE[,k]*R  
  quienes=which(runif(K)<=ps)   
  
  if (length(quienes)==0)
  {
    sp.out[[t+1]]=c(sp.out[[t+1]],0)
  }
  else
  {
    sp.out[[t+1]]=c(sp.out[[t+1]],quienes)
    # sacamos llas filas y columnas de esa/esas especie(s)
    sp.new=c(sp.new,quienes)
    }
  }
  
  if (length(sp.new)>0) # si se extingueron mas especies
  {
    sp.o=sp.new
    sp.chau=c(sp.chau,sp.o)
  }
    
}

