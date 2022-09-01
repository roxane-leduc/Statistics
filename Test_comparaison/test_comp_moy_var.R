# Nettoyage de l’espace de travail
rm(list=ls())

data = read.table("Arbre_data.txt",header=TRUE,sep=";",dec=".")
attach(data)

StatBase = summary(Hauteur[Type=="T1"])
StatBase = rbind(StatBase, summary(Hauteur[Type=="T2"]))
# Ajout des écart-types
std = c(sd(Hauteur[Type=="T1"]), sd(Hauteur[Type=="T2"]))
StatBase = cbind(StatBase, std)
row.names(StatBase) = c("T1","T2")
round(StatBase, 2)

boxplot(Hauteur ~ Type, col="cyan", names=c("T1","T2"),cex.axis=1.5,
        ylab = "Hauteur des arbres (en m)", cex.lab=1.5)
# Ajout des moyennes
moy1 = mean(Hauteur[Type=="T1"])
moy2 = mean(Hauteur[Type=="T2"])
points(c(moy1,moy2), cex=1.5)

x = Hauteur[Type=="T1"]
hist(x, freq=F, col="cyan",main="Type d’arbres : T1", cex.main=1.6)
lines(density(x), lwd=2)
#
x = Hauteur[Type=="T2"]
hist(x, freq=F, col="cyan",main="Type d’arbres : T2", cex.main=1.6)
lines(density(x), lwd=2)

# Tests de Normalité
shapiro.test(Hauteur[Type=="T1"])
shapiro.test(Hauteur[Type=="T2"])

# Tests de variances
var.test(Hauteur ~ Type)

# Tests de moyennes
t.test(Hauteur ~ Type, var.equal=TRUE)

