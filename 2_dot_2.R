# Inversion vis à vis des graphes

library(sm) # Chargement de sm

##### Question 1 #####

data <- read.table('notes.txt') # Initialisation des données
Y <- as.matrix(data) # Matrice notes -> Y
n <- dim(Y)[1] # Nombre d'individus = 9
X <- Y - matrix(1, n, 1) %*% apply(Y, 2, mean) # Centrage et réduction éventuelle du tableau
#X <- X/matrix(1, n, 1) %*% apply(X, 2, sd) réduction entre 0 et 1
V <- (1/n)*t(X) %*% X #Calcul de la matrice de covariance ou de correlation
tmp <- eigen(V, symmetric=TRUE) # Calcul des valeurs propres et des axes d'inertie 
L <- diag(tmp$values)
U <- tmp$vectors
C <- X %*% U # Calcul des composantes principales des individus
COR <- diag(1/apply(X^2, 1, sum)) %*% C^2 # Calcul des contributions
CTR <- (1/n)*C^2 %*% diag(1/diag(L))
D <- diag(1/(sqrt((n-1)/n)*apply(X, 2, sd))) %*% U %*% sqrt(L) # Représentation des variables

# Tracé des graphiques

plot(-1:1, -1:1, type="n", xlab='Axe 1', ylab='Axe 2')
text(D[,1], D[,2], colnames(data)); abline(h=0);abline(v=0)
curve(sqrt(1-x^2), -1, 1, add=TRUE)
curve(-sqrt(1-x^2), -1, 1, add=TRUE)

pause()

plot(-1:1, -1:1, type="n", xlab='Axe 1', ylab='Axe 3')
text(D[,1], D[,3], colnames(data)); abline(h=0);abline(v=0)
curve(sqrt(1-x^2), -1, 1, add=TRUE)
curve(-sqrt(1-x^2), -1, 1, add=TRUE)

# Conclusion : 
# 2eme axe : dure à définir; Ordonnée : math / francais un peu ? // Abscisse : Science / Latin (léger !) très très léger ! Ordonné négatif : latin/fran // positif : science / maths
# 3eme Axe : Ordonnée : Bon en dessin ; Abscisse : bon dans les 4 autres matières
#

pause()

plot(C[,1], C[,2], type="n")
text(C[,1], C[,2], rownames(data))
abline(h=0);abline(v=0)

pause()

plot(C[,1], C[,3], type="n")
text(C[,1], C[,3], rownames(data))
abline(h=0);abline(v=0);

# Conclusion : 
# 2eme Axe : Trop léger, on peut pas définir
# 3eme Axe : Ordonnée -> evel très bon en dessin : Bon en dessin ; Abscisse : bon dans les 4 autres matières -> Moni très bonne dans les 4 autres matières // Jean très mauvais

##### Question 2 ######

data <- read.table('notes.txt') # Initialisation des données
res <- princomp(data) # Calcul de l'ACP

summary(res)
(res$sdev)^2 # L
res$loadings
res$scores
plot(res)
biplot(res)
biplot(res, c(1,3))
