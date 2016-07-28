################
library(ggplot2)
library(dplyr)
library(corrplot)
library(psych)
library(MASS)
setwd("C:/Users/Para2x/Dropbox/R projects/UEFA 2016")
###################
dataset<-read.csv("Dataset_16.csv")
dataset$Standing<-as.factor(dataset$Standing)
d_stan = as.matrix(scale(dataset[,3:25]))
############### FA
FA<-fac(cor(d_stan),2, rotate = "promax", fm = "minres")
scor<-factor.scores((dataset[,3:25]), FA)
FA.dat<-dataset%>%mutate(score1=scor$scores[,1],score2=scor$scores[,2])

FA.dat%>%ggplot(aes(x=score1,y=score2))+
  theme_classic(base_size = 15)+
  geom_point(aes(color=Standing),size=4)+
  geom_text(aes(label = Team,y=score2-0.1),size=6)+
  labs(x="FA1- (Score on attempts and passes)",y="FA2-(Goal Score)")+
  geom_vline(xintercept = 0,color="red",size=1.2)+
  geom_hline(yintercept = 0,color="red",size=1.2)

corrplot(cor(FA.dat[,3:ncol(FA.dat)]), mar = c(1,0, 0, 0),tl.cex=0.9,method="square",type="lower", tl.col = "black")




