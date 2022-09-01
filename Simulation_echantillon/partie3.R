# Nettoyage de l’espace de travail
rm(list=ls())
# Instruction pour que tout le monde ait le meme alea de depart
RNGkind(sample.kind = "Rounding")
set.seed(111)

f = function(x){f = 2 / (pi * (1 + x^2)^2)}
M = integrate(f, lower=-20, upper=20)$value
h = function(x){
  h = 1/(1 + x^2) 
}
N = 200
X = nrejet = rep(NA, N)
for(j in 1:N){
  n = 1
  u = runif(1)
  y = rcauchy(1)
  while (u > h(y)) {
    u = runif(1)
    y = rcauchy(1)
    n = n+1
  }
  X[j] = y
  nrejet[j] = n
}

Titre = "Simulation d'un echantillon"
t = seq(-4,4,0.05)
lines(t, f(t), type = "l", lwd = 2)