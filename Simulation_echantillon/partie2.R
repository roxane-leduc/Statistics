# Nettoyage de l’espace de travail
rm(list=ls())
# Instruction pour que tout le monde ait le meme alea de depart
RNGkind(sample.kind = "Rounding")
set.seed(111)

a=1
b=1

los =function(x,y){
  return(ifelse((abs(x/a)+abs(y/b)>1),TRUE,FALSE))
}

n=400
X = matrix(NA,ncol = 2, nrow = n)
for(i in 1:n){
  x=runif(1,min=-a,max=a)
  y=runif(1,min=-b,max=b)
  while (los(x,y)==TRUE) {
    x=runif(1,min=-a,max=a)
    y=runif(1,min=-b,max=b)
  }
  X[i,] = c(x,y)
}

Titre = "Simulation d'un echantillon"
plot(X,xlim=c(-a,a),ylim=c(-b,b),xlab="",ylab="",main =Titre,cex.main=1.6)
polygon(c(-a,0,a,0), c(0,-b,0,b), lwd=2)