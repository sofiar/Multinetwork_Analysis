#### All results
library(tidyverse)
library(gridExtra)

HT_I2017 <- read.csv("~/multicapas_tiño/implementacion/HT_I2017.csv")
HT_I2018 <- read.csv("~/multicapas_tiño/implementacion/HT_I2018.csv")

HT_NI2017 <- read.csv("~/multicapas_tiño/implementacion/HT_NI2017.csv")
HT_NI2018 <- read.csv("~/multicapas_tiño/implementacion/HT_NI2018.csv")


year=rep(c('2017','2018'),each=2000)
treatment=rep(c('NU','WU','NU','WU'),each=1000)

Mreal=c(rep(10,1000),rep(6,1000),rep(12,1000),rep(9,1000))

data=rbind(HT_NI2017,HT_I2017,HT_NI2018,HT_I2018)
HTall=data.frame(cbind(data,year,treatment,Mreal))

hi1=HTall %>% filter(year=='2018'&treatment=='WU')
p1=ggplot(hi1)+geom_bar(aes(x=M))+theme_bw()+geom_vline(xintercept =unique(hi1$Mreal),linetype = "dashed")+ggtitle('WU 2018')

hi2=HTall %>% filter(year=='2017'&treatment=='WU')
p2=ggplot(hi2)+geom_bar(aes(x=M))+theme_bw()+geom_vline(xintercept =unique(hi1$Mreal),linetype = "dashed")+ggtitle('WU 2017')

hi3=HTall %>% filter(year=='2018'&treatment=='NU')
p3=ggplot(hi3)+geom_bar(aes(x=M))+theme_bw()+geom_vline(xintercept =unique(hi1$Mreal),linetype = "dashed")+ggtitle('NU 2018')

hi4=HTall %>% filter(year=='2017'&treatment=='NU')
p4=ggplot(hi4)+geom_bar(aes(x=M))+theme_bw()+geom_vline(xintercept =unique(hi1$Mreal),linetype = "dashed")+ggtitle('NU 2017')

grid.arrange(p1, p2,p3, p4,nrow = 1)


#ggplot(HTall)+geom_bar(aes(x=M))+theme_bw()+geom_vline(xintercept =Mreal,linetype = "dashed") +facet_wrap(~year+treatment)



