# On souhaite modeliser le nombre de calories apportees par 100 grammes de fromage a partir de ses propriees nutritives. 

# Se mettre dans le repertoire de travail pour savoir ou vous etes
getwd()
# Pour vous mettre dans le bon repertoire
nom.rep = "~/Semestre7/Stat/CodesR/MGL_mult"
setwd(nom.rep)
# Nettoyage de l espace de travail
rm(list=ls())
# Lecture des donnees et construction du dataframe "fromages"
nomfile = "fromages_data.txt"
fromages = read.table(file=nomfile,header=T, sep="\t",dec=".")
# Affichage des 5 premieres lignes du dataframe
fromages[1:5,]
# Donnees sans le nom des fromages
data = fromages[,-1]

# Presentation plus synthetique des donnees:
statbase = NULL
for (j in 1:ncol(data)){
statbase =rbind(statbase, summary(data[,j]))
}
rownames(statbase) = names(data)
statbase

# Existe-t-il des correlations fortes evidentes a l oeil nu entre les differentes variables explicatives ? 
# Construction de la matrice des regresseurs
X = data[,-1]
plot(X)
round(cor(X),2)

# Etude des multi-colinearites
VIF = diag(solve(cor(X)))
round(VIF, 2)

# Construction du  modele lineaire 
# Equivalente a l instruction : reslm = lm(calories ~ sodium + calcium + lipides + retinol + folates + proteines + cholesterol + magnesium, data = data)
reslm = lm(calories~. , data = data)
# Si on garde l'ensemble des regresseurs: reslm = lm(calories ~., data = data)

# Resumer les principales caracteristiques de ce modele
summary(reslm)

# Etudier les performances de ce modele en construisant le nuage de points
Y = data[,1]
NomY = "Calories"
Titre = " Nuage de points (observes, estimes)"
plot(Y,predict(reslm), xlab=paste(NomY," observee"), ylab=paste(NomY," estimee"),
main=Titre, xlim=c(50,450), ylim=c(50,450),
cex=1.5, cex.lab=1.6, cex.main = 1.7,cex.axis=1.5,pch=19)
abline(0,1, col="black", lwd=2)

#On veut maintenant ne garder dans le modele que les regresseurs juges significatifs a 5%, c est a dire ceux qui ont une influence reelle sur la variable a expliquer calories.
# Regression pas a pas ascendante (on selectionne parmi les variables explicatives qui ont une influence reelle sur calories, celle qui est la plus significative, autrement dit celle qui a la p-value la plus petite et < 0.05, ou bien celle qui conduit au R2 le plus grand. Puis, on effectue toutes les regressions lineaires a 2 variables explicatives comprenant la variable explicative qui vient d etre selectionnee. On arrete le processus de selection lorsqu’on ne trouve plus de regresseurs ayant une influence significative):
summary(lm(calories ~sodium, data=data))[[4]]
summary(lm(calories ~calcium, data=data))[[4]] #...

# Regression pas a pas descendante
# On commence par lancer la regression lineaire avec tous les regresseurs
summary(lm(calories~., data=data))[[4]]
# Puis, on selectionne les variables qui n ont pas une influence reelle sur calories et on elimine du modele celle qui a la p-value la plus grande et ≥ 0.05. On relance alors la regression lineaire avec le modele ampute de la variable eliminee.
summary(lm(calories~. -nomvar, data=data))[[4]] # A COMPLETER

# Estimation de l erreur de Prevision : modele complet
n = length(data[,1])
Intercept = rep(1,n)
X = as.matrix(cbind(Intercept,data[,-1]))
Y = data$calories
# Calcul des previsions "Jackknife" Estimation Jecknife des pa
beta.lmc = matrix(NA, ncol=ncol(X), nrow=n)
yp.lmc= rep(NA, n)
for (j in 1:n){
XpX = t(X[-j,]) %*% X[-j,] # Construction de (X’X)
beta.lmc[j,] = solve(XpX) %*% t(X[-j,]) %*% Y[-j]
yp.lmc[j] = as.numeric(beta.lmc[j,]%*%X[j,])
}
# Estimation de l’erreur de prevision (RMSE)
sqrt(mean((Y-yp.lmc)^2))
