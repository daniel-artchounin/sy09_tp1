library(sm) # Chargement de sm


############################### Question 1 ############################### 

data <- read.table('notes.txt') # Initialisation des données
Y <- as.matrix(data) # Matrice notes -> Y
n <- dim(Y)[1] # Nombre d'individus = 9
X <- Y - matrix(1, n, 1) %*% apply(Y, 2, mean) # Centrage et réduction éventuelle du tableau
# X <- X/matrix(1, n, 1) %*% apply(X, 2, sd) réduction entre 0 et 1
V <- (1/n)*t(X) %*% X #Calcul de la matrice de covariance ou de correlation
tmp <- eigen(V, symmetric=TRUE) # Calcul des valeurs propres et des axes principaux d'inertie 
L <- diag(tmp$values) # Matrice diagonale des valeurs propres
U <- -tmp$vectors # Matrice des vecteurs propres
C <- X %*% U # Calcul des composantes principales
COR <- diag(1/apply(X^2, 1, sum)) %*% C^2 # Calcul des contributions des axes aux individus
CTR <- (1/n)*C^2 %*% diag(1/diag(L)) # Calcul des contributions des individus aux axes
D <- diag(1/(sqrt((n-1)/n)*apply(X, 2, sd))) %*% U %*% sqrt(L) # Représentation des variables

# Représentation des variables dans le plan formé à partir de C1 et C2
png("./images/2_dot_2/variables_c1_c2.png")
plot(-1:1, -1:1, type="n", xlab='C1', ylab='C2', 
	main='Représentation des variables dans le \nplan formé à partir de C1 et C2')
text(D[,1], D[,2], colnames(data)); abline(h=0);abline(v=0)
curve(sqrt(1-x^2), -1, 1, add=TRUE)
curve(-sqrt(1-x^2), -1, 1, add=TRUE)
dev.off()

pause()

# Représentation des variables dans le plan formé à partir de C1 et C3
png("./images/2_dot_2/variables_c1_c3.png")
plot(-1:1, -1:1, type="n", xlab='C1', ylab='C3', 
	main='Représentation des variables dans le \nplan formé à partir de C1 et C3')
text(D[,1], D[,3], colnames(data)); abline(h=0);abline(v=0)
curve(sqrt(1-x^2), -1, 1, add=TRUE)
curve(-sqrt(1-x^2), -1, 1, add=TRUE)
dev.off()

# Conclusion : 
# 2eme axe : dure à définir; Ordonnée : math / francais un peu ? // 
# Abscisse : Science / Latin (léger !) très très léger ! Ordonné négatif : latin/fran // 
# positif : science / maths
# 3eme Axe : Ordonnée : Bon en dessin ; Abscisse : bon dans les 4 autres matières

pause()

# Représentation des individus dans le plan formé à partir de U1 et U2
png("./images/2_dot_2/individus_u1_u2.png")
plot(C[,1], C[,2], type="n", xlab='U1', ylab='U2', 
	main='Représentation des individus dans le\n plan formé à partir de U1 et U2')
text(C[,1], C[,2], rownames(data))
abline(h=0);abline(v=0)
dev.off()

pause()

# Représentation des individus dans le plan formé à partir de U1 et U3
png("./images/2_dot_2/individus_u1_u3.png")
plot(C[,1], C[,3], type="n", xlab='U1', ylab='U3', 
	main='Représentation des individus dans le\n plan formé à partir de U1 et U3')
text(C[,1], C[,3], rownames(data))
abline(h=0);abline(v=0);
dev.off()

pause()

# Conclusion : 
# 2eme Axe : Trop léger, on peut pas définir
# 3eme Axe : Ordonnée -> evel très bon en dessin : Bon en dessin ; 
# Abscisse : bon dans les 4 autres matières -> Moni très bonne dans les 
# 4 autres matières // Jean très mauvais


############################### Question 2 ############################### 

data <- read.table('notes.txt') # Initialisation des données
res <- princomp(data) # Calcul de l'ACP
summary(res)
(res$sdev)^2 # L
res$loadings
res$scores

# Diagramme en bâtons des valeurs propres
png("./images/2_dot_2/batons_valeurs_propres.png")
plot(res, main='Diagramme en bâtons des valeurs propres')
dev.off()

pause()

# Représentation de l'ancienne base et des individus dans le plan formé à partir de U1 et U2
png("./images/2_dot_2/ex_base_et_individus_u1_u2.png")
biplot(res, xlab='U1', ylab='U2', 
	main='Représentation de l\'ancienne base et des individus\n dans le plan formé à partir de U1 et U2')
dev.off()

pause()

# Représentation de l'ancienne base et des individus dans le plan formé à partir de U1 et U3
png("./images/2_dot_2/ex_base_et_individus_u1_u3.png")
biplot(res, c(1,3), xlab='U1', ylab='U3', 
	main='Représentation de l\'ancienne base et des individus\n dans le plan formé à partir de U1 et U3')
dev.off()