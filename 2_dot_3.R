library(sm) # Chargement de sm

# Chargement des données crabs
library(MASS)
data(crabs)
crabsquant<-crabs[,4:8]


############################### Question 1 ############################### 

Y <- as.matrix(crabsquant) # Matrice crabs -> Y
n <- dim(Y)[1] # Nombre d'individus = 200
X <- Y - matrix(1, n, 1) %*% apply(Y, 2, mean) # Centrage et réduction éventuelle du tableau
# X <- X/matrix(1, n, 1) %*% apply(X, 2, sd) réduction entre 0 et 1
V <- (1/n)*t(X) %*% X #Calcul de la matrice de covariance ou de correlation
tmp <- eigen(V, symmetric=TRUE) # Calcul des valeurs propres et des axes principaux d'inertie 
L <- diag(tmp$values) # Matrice diagonale des valeurs propres
U <- tmp$vectors # Matrice des vecteurs propres
# U[,1] = -U[,1]
# U[,2] = -U[,2]
# U[,3] = -U[,3]
# U[,4] = -U[,4]
# U[,5] = -U[,5]
C <- X %*% U # Calcul des composantes principales
COR <- diag(1/apply(X^2, 1, sum)) %*% C^2 # Calcul des contributions des axes aux individus
CTR <- (1/n)*C^2 %*% diag(1/diag(L)) # Calcul des contributions des individus aux axes
D <- diag(1/(sqrt((n-1)/n)*apply(X, 2, sd))) %*% U %*% sqrt(L) # Représentation des variables

# Représentation des variables dans le plan formé à partir de C1 et C2
pdf("./images/2_dot_3/variables_crabsc1c2.pdf")
plot(-1:1, -1:1, type="n", xlab='C1', ylab='C2', 
	main='ACP sur les données Crabs')
text(D[,1], D[,2], colnames(crabsquant)); abline(h=0);abline(v=0)
curve(sqrt(1-x^2), -1, 1, add=TRUE)
curve(-sqrt(1-x^2), -1, 1, add=TRUE)
dev.off()

pause()

# Représentation des variables dans le plan formé à partir de C1 et C3
pdf("./images/2_dot_3/variables_crabsc1c3.pdf")
plot(-1:1, -1:1, type="n", xlab='C1', ylab='C3', 
	main='Représentation des variables dans le \nplan formé à partir de C1 et C3')
text(D[,1], D[,3], colnames(crabsquant)); abline(h=0);abline(v=0)
curve(sqrt(1-x^2), -1, 1, add=TRUE)
curve(-sqrt(1-x^2), -1, 1, add=TRUE)
dev.off()

pause()

# Représentation des variables dans le plan formé à partir de C1 et C4
pdf("./images/2_dot_3/variables_c1_c4.pdf")
plot(-1:1, -1:1, type="n", xlab='C1', ylab='C4', 
	main='Représentation des variables dans le \nplan formé à partir de C1 et C4')
text(D[,1], D[,4], colnames(crabsquant)); abline(h=0);abline(v=0)
curve(sqrt(1-x^2), -1, 1, add=TRUE)
curve(-sqrt(1-x^2), -1, 1, add=TRUE)
dev.off()

pause()

# Représentation des variables dans le plan formé à partir de C1 et C5
pdf("./images/2_dot_3/variables_c1_c5.pdf")
plot(-1:1, -1:1, type="n", xlab='C1', ylab='C5', 
	main='Représentation des variables dans le \nplan formé à partir de C1 et C5')
text(D[,1], D[,5], colnames(crabsquant)); abline(h=0);abline(v=0)
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
pdf("./images/2_dot_2/individus_u1_u2.pdf")
plot(C[,1], C[,2], type="n", xlab='U1', ylab='U2', 
	main='Représentation des individus dans le\n plan formé à partir de U1 et U2')
text(C[,1], C[,2], rownames(crabsquant))
abline(h=0);abline(v=0)
dev.off()

pause()

# Représentation des individus dans le plan formé à partir de U1 et U3
pdf("./images/2_dot_3/individus_u1_u3.pdf")
plot(C[,1], C[,3], type="n", xlab='U1', ylab='U3', 
	main='Représentation des individus dans le\n plan formé à partir de U1 et U3')
text(C[,1], C[,3], rownames(crabsquant))
abline(h=0);abline(v=0);
dev.off()

pause()

# Conclusion : 
# 2eme Axe : Trop léger, on peut pas définir


############################### Question 1 suite ############################### 


res <- princomp(crabsquant) # Calcul de l'ACP
summary(res)
(res$sdev)^2 # L
res$loadings
res$scores

# Diagramme en bâtons des valeurs propres
pdf("./images/2_dot_3/batons_valeurs_propres.pdf")
plot(res, main='Variance des composantes principales')
dev.off()

pause()

# Représentation de l'ancienne base et des individus dans le plan formé à partir de U1 et U2
pdf("./images/2_dot_3/ex_base_et_individus_u1_u2.pdf")
biplot(res, xlab='C1/U1', ylab='C2/U2', 
	main='ACP sur les données Crabs')
dev.off()

pause()

# Représentation de l'ancienne base et des individus dans le plan formé à partir de U1 et U3
pdf("./images/2_dot_3/ex_base_et_individus_u1_u3.pdf")
biplot(res, c(1,3), xlab='C1/U1', ylab='C3/U3', 
	main='Représentation des variables dans le plan formé \n à partir de C1 et C3 et des individus\n dans le plan formé à partir de U1 et U3')
dev.off()

############################### Question 2 ###############################

crabsquant2 = crabsquant / crabsquant$CL
crabsquant2 = crabsquant2[,-3]

Y2 <- as.matrix(crabsquant2) # Matrice crabs réduite -> Y2
n2 <- dim(Y2)[1] # Nombre d'individus = 200
X2 <- Y - matrix(1, n2, 1) %*% apply(Y2, 2, mean) # Centrage et réduction éventuelle du tableau
# X <- X/matrix(1, n2, 1) %*% apply(X2, 2, sd) réduction entre 0 et 1
V2 <- (1/n2)*t(X2) %*% X2 #Calcul de la matrice de covariance ou de correlation
tmp2 <- eigen(V2, symmetric=TRUE) # Calcul des valeurs propres et des axes principaux d'inertie 
L2 <- diag(tmp2$values) # Matrice diagonale des valeurs propres
U2 <- tmp2$vectors # Matrice des vecteurs propres
# U[,1] = -U[,1]
# U[,2] = -U[,2]
# U[,3] = -U[,3]
# U[,4] = -U[,4]
# U[,5] = -U[,5]
C2 <- X2 %*% U2 # Calcul des composantes principales
COR2 <- diag(1/apply(X2^2, 1, sum)) %*% C2^2 # Calcul des contributions des axes aux individus
CTR2 <- (1/n2)*C2^2 %*% diag(1/diag(L2)) # Calcul des contributions des individus aux axes
D2 <- diag(1/(sqrt((n2-1)/n2)*apply(X2, 2, sd))) %*% U2 %*% sqrt(L2) # Représentation des variables

# Représentation des variables dans le plan formé à partir de C1 et C2
pdf("./images/2_dot_3/PTvariables_crabsc1c2.pdf")
plot(-1:1, -1:1, type="n", xlab='C1', ylab='C2', 
	main='ACP sur les données Crabs pré traitées')
text(D2[,1], D2[,2], colnames(crabsquant2)); abline(h=0);abline(v=0)
curve(sqrt(1-x^2), -1, 1, add=TRUE)
curve(-sqrt(1-x^2), -1, 1, add=TRUE)
dev.off()

pause()

# Représentation des individus dans le plan formé à partir de U1 et U2
pdf("./images/2_dot_3/PTindividus_u1_u2.pdf")
plot(C2[,1], C2[,2], type="n", xlab='U1', ylab='U2', 
	main='Représentation des individus dans le\n plan formé à partir de U1 et U2')
text(C2[,1], C2[,2], rownames(crabsquant2))
abline(h=0);abline(v=0)
dev.off()

pause()

res2 <- princomp(crabsquant2) # Calcul de l'ACP
summary(res2)
(res2$sdev)^2 # L2
res2$loadings
res2$scores

# Diagramme en bâtons des valeurs propres
pdf("./images/2_dot_3/PTbatons_valeurs_propres.pdf")
plot(res2, main='Variance des composantes principales avec données pré traitées')
dev.off()

pause()

# Représentation de l'ancienne base et des individus dans le plan formé à partir de U1 et U2
pdf("./images/2_dot_3/PTex_base_et_individus_u1_u2.pdf")
biplot(res2, xlab='C1/U1', ylab='C2/U2', 
	main='ACP sur les données Crabs pré traitées')
dev.off()

pause()

# Représentation de l'ancienne base et des individus dans le plan formé à partir de U1 et U3
pdf("./images/2_dot_3/PTex_base_et_individus_u1_u3.pdf")
biplot(res2, c(1,3), xlab='C1/U1', ylab='C3/U3', 
	main='Représentation des variables dans le plan formé \n à partir de C1 et C3 et des individus\n dans le plan formé à partir de U1 et U3')
dev.off()

pause()

labelsNames <- c("Fontal lobe size (mm)", "Rear width (mm)", 
                "Carapace width (mm)", "Body depth (mm)")

pdf("./images/2_dot_3/crabes_selon_sexe.pdf")
plot(crabsquant2, main="Caractéristiques morphologiques\n des crabes selon leur sexe", 
	pch=21, 
	col=c("firebrick", "darkcyan")[crabs[,2]],
	labels=labelsNames	
	)
legend("bottomright", 200,legend=c("Femelle", "Mâle"), 
	col = c("firebrick", "darkcyan"), pch = 21, inset=0) 
dev.off()

pause()

pdf("./images/2_dot_3/crabes_selon_espece.pdf")
plot(crabsquant2, main="Caractéristiques morphologiques \ndes crabes selon leur espèce", 
	pch=21, 
	col=c("lightslateblue", "orangered")[crabs[,1]],
	labels=labelsNames)
legend("bottomright", 200,legend=c("Espèce bleue", "Espèce orange"), 
	col = c("lightslateblue", "orangered"), pch = 21, inset=0)
dev.off()

############################### Question 2 option bis############################### 

crabsquant2 = crabsquant / crabsquant$CL
crabsquant2 = crabsquant2[,-3]

res2 = princomp(crabsquant2)


pdf(file = "./images/2_dot_3/PTplot_acp2_crabs.pdf")
plot(res2, main = "")
dev.off()

pdf(file = "./images/2_dot_3/PTbiplot_acp2_crabs.pdf")
biplot(res2, main = "Représentation de la corrélation après traitement")
abline(lty = 1, a = 0, b = 0, col = 1)
abline(lty = 1, a = 0, b = 1000000, col = 1)
dev.off()

pdf(file = "./images/2_dot_3/PTplot_crabs_sp_2.pdf")
plot(crabsquant2, col = crabs$sp)
dev.off()

pdf(file = "./images/2_dot_3/PTplot_crabs_sex_2.pdf")
plot(crabsquant2, col = crabs$sex)
dev.off()