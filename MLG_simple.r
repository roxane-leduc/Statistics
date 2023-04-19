# Nettoyage de l’espace de travail
rm(list=ls())

# Lecture des donnees
NomFile = "ASE12_QDP_periode1.csv"
data = read.table(NomFile,sep=";",header=T)

# Affichage des 6 premieres lignes
head(data)

#Verifier si le fichier contient des valeurs manquantes
summary(data)
# Suppression des valeurs manquantes
data = na.omit(data)

# --------------------------------------------------------------------
#Justifier la modelisation des donnees par un modele lineaire gaussien
# Taille de l echantillon
n = nrow(data)
# Coefficient de correlation lineaire
X = data$NO2_ASE
Y = data$NO2_QDP
r = cor(X, Y)
round(r, 4)
# Calcul des coefficients de la DMC
a = cov(X,Y)/ var(X)
round(a,4)
b = mean(Y) - a*mean(X)
round(b, 4)
# Ajustement de la DMC
# Representation graphique du nuage de points et de la DMC
Titre = paste("Nuage de points, R = ", round(r,4))
plot(X, Y, xlab="NO2 en mV", ylab="NO2 en microgramme/m3",
main= Titre , cex=1.5, cex.lab=1.6, cex.main = 1.7,cex.axis=1.5)
lines(X, a*X+b, lwd=2)

# --------------------------------------------------------------------
# Afin d etudier au mieux les performances d une modelisation lineaire de ces donnees, on scinde en deux: un echantillon d apprentissage et un echantillon de test
# Instruction pour que tout le monde ait le meme alea de depart
RNGkind(sample.kind = "Rounding")
set.seed(111)
# Verifier que vous obtenez la meme serie de nombres
sample(1:10, 5, replace=T)
# [1] 6 8 4 6 4

# Pour que nous ayons tous les memes jeux de donnees
set.seed(1111) # initialisation du generateur
# Construction des echantillons APP et Test
# Extraction des echantillons
test.ratio = 0.5 # part de l echantillon test
npop = nrow(data) # nombre de lignes dans les donnees
ntest = ceiling(npop*test.ratio) # taille de l’echantillon test
testi = sample(1:npop,ntest) # indices de l’echantillon test
appri = setdiff(1:npop,testi) # indices de l’echant. d’apprentissage
# Construction des echantillons avec les variables explicatives.
dataApp = data[appri,] # construction de l’echantillon d’apprentissage
dataTest = data[testi,] # construction de l’echantillon test

# --------------------------------------------------------------------
# Apres avoir construit le modele lineaire simple, on examine ses caracteristiques et on évalue ses performances en prediction (sur dataApp)
# Modele lineaire simple
# La fonction lm permet d’ajuster un modele lineaire
reslm = lm(NO2_QDP ~ NO2_ASE, data=dataApp)
summary(reslm)
# La fonction names appliquee a un objet de classe lm permet de connaitre les noms de tous ses attribus
names(reslm)

#Calcul des predictions sur Eapp
y.lm = predict(reslm)
#Calcul des previsions sur Etest
yt.lm = predict(reslm, newdata = dataTest)

# Nuage de points (observes, predits)
y.app = dataApp$NO2_QDP
Lim = c(min(y.app,y.lm), max(y.app, y.lm))
Titre = "Nuage de points ((Observe,Predit) - Eapp)"

# Construction des intervalles de confiance (à 95%)
summary(reslm)
qt(0.975,reslm$df)
qchisq(c(0.025,0.975),reslm$df)
# Instruction pour verification
round(confint(reslm), 4)
# Calcul des residus
residus = reslm$residuals

# Construction du graphe des residus
TitreRes = "Graphe des residus"
plot(y.app,residus, xlab="NO2", ylab="Residus",main=TitreRes,
cex=1.5, cex.lab=1.6, cex.main = 1.7,cex.axis=1.5, pch=19)
abline(0,0, col=2, lwd=2)

# Histogramme en frequences des residus
Titre = "Histogramme des residus"
hist(reslm$resid, freq=FALSE, col="cyan",,main= Titre , cex=1.5,
cex.main = 1.7,cex.axis=1.5)
lines(density(residus),lwd=2)

# Graphe Quantile-Quantile
qqnorm(scale(residus),cex.main=1.6,cex.axis=1.3,cex.lab=1.4,cex=1.2)
abline(0,1,col="red",lwd=2)
plot(y.app, y.lm, xlab="NO2 observe", ylab="NO2 predit",
main= Titre , cex=1.5, cex.lab=1.6, cex.main = 1.7,cex.axis=1.5,
xlim=Lim, ylim=Lim)
abline(0,1, col=2, lwd=2)

# Verifier le caractere gaussien des residus a l’aide du test de Shapiro-Wilk
# Si p-value<risque on rejette H0 (hypothèse que résidus suivent loi gaussienne)
shapiro.test(reslm$residuals)

# --------------------------------------------------------------------
# On souhaite etudier maintenant la capacite du modele a se generaliser. Pour cela, nous allons etudier les performances du modele en prevision (sur dataTest) et les comparer a celles obtenues sur dataApp.
#Calcul des previsions sur Etest
yt.lm = predict(reslm, newdata = dataTest)
# Definition de la fonction score
score = function(y, ychap){
rmse = sqrt(mean((y - ychap)^2))
mae = mean(abs(y - ychap))
EV = (1 - sum((y - ychap)^2) / sum((y - mean(y))^2))*100
Sc = c(rmse,mae,EV)
names(Sc) = c("RMSE", "MAE", "EV (%)") # % variance expliquee
score = round(Sc,2)
}

# Comparaison des performances:
# Performances sur Eapp
y.app = dataApp$NO2_QDP
print(score(y.app, y.lm))
# Performances sur Etest
y.test = dataTest$NO2_QDP
print(score(y.test, yt.lm))

# Comparaison des nuages de points:
par(mfrow=c(2,1))
# Nuage de points (observes, predits)
screen(1)
Lim = c(min(y.app,y.lm), max(y.app, y.lm))
Titre = "Nuage de points ((Observe,Predit) - Eapp)"
plot(y.app, y.lm, xlab="NO2 observe", ylab="NO2 predit",
main= Titre , cex=1.5, cex.lab=1.6, cex.main = 1.7,cex.axis=1.5,
xlim=Lim, ylim=Lim)
abline(0,1, col=2, lwd=2)
# Nuage de points (observes, prevus)
screen(2)
Lim = c(min(y.test,yt.lm), max(y.test, yt.lm))
Titre = "Nuage de points ((Observe,Prevu) - Etest)"
plot(y.test, yt.lm, xlab="NO2 observe", ylab="NO2 predit",
main= Titre , cex=1.5, cex.lab=1.6, cex.main = 1.7,cex.axis=1.5,
xlim=Lim, ylim=Lim)
abline(0,1, col=2, lwd=2)
