# Nettoyage de l’espace de travail
rm(list=ls())
# Instruction pour meme alea de depart
RNGkind(sample.kind = "Rounding")
set.seed(111)

n=400
theta=5
x=(-log(1-runif(n)))^(1/theta)
t=seq(0,30,0.1)
Titre = "Echantillon de la loi de Weibull"
hist(x,proba=TRUE,col="cyan",cex.lab=1.6,cex.main=1.7,main=Titre)
lines(t,theta*t^(theta-1)*exp(-t^theta),lwd=4)

