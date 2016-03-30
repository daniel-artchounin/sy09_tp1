library(sm) # Chargement de sm

############################### Question 1 : sans fonction R ############################### 

# Chargement des données crabs
library(MASS)
data(crabs)
crabsquant<-crabs[,4:8]

# Création de l'ACP sans les fonctions R
Y <- as.matrix(crabsquant) # Matrice crabs -> Y
n <- dim(Y)[1] # Nombre d'individus = 200
X <- Y - matrix(1, n, 1) %*% apply(Y, 2, mean) # Centrage et réduction éventuelle du tableau
# X <- X/matrix(1, n, 1) %*% apply(X, 2, sd) réduction entre 0 et 1
V <- (1/n)*t(X) %*% X #Calcul de la matrice de covariance ou de correlation
tmp <- eigen(V, symmetric=TRUE) # Calcul des valeurs propres et des axes principaux d'inertie 
L <- diag(tmp$values) # Matrice diagonale des valeurs propres
U <- tmp$vectors # Matrice des vecteurs propres

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
# Les variables sont très corrélés à l'axe 1 mais pas du tout à l'axe 2

pause()

# Représentation des individus dans le plan formé à partir de U1 et U2
pdf("./images/2_dot_2/individus_u1_u2.pdf")
plot(C[,1], C[,2], type="n", xlab='U1', ylab='U2', 
	main='Représentation des individus dans le\n plan formé à partir de U1 et U2')
text(C[,1], C[,2], rownames(crabsquant))
abline(h=0);abline(v=0)
dev.off()

pause()

# Conclusion : 
#  Trop d'individus éparpillé et trop de corrélation des variables autour de l'axe 1 pour pouvoir en déduire quelque chose de très clair


############################### Question 1 suite  : fonction R ############################### 

# Création de l'ACP à partir des fonctions R
res <- princomp(crabsquant) # Calcul de l'ACP
summary(res)
(res$sdev)^2 # L
res$loadings # Vecteurs propres
res$scores # Composantes
res$sdev # Valeurs propres

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

# On ne peut rien en déduire : il faut un pré-traitement des données. On décide de prendre la valeur la plus corrélé avec l'axe des abscisse : CL

############################### Question 2 : pré-traitement et nouvelle ACP via fonctions R ###############################

# Pré-traitement
crabsquant2 = crabsquant / crabsquant$CL # Division par CL
crabsquant2 = crabsquant2[,-3] # Suppression de la colonne CL rempli de 1 et inutile

colnames(crabsquant2) <- c("FL/CL","RW/CL","CW/CL","BD/CL")
res2 <- princomp(crabsquant2) # Calcul de l'ACP après pré-traitement
summary(res2)
(res2$sdev)^2 # L2
res2$loadings # Vecteurs propres
res2$scores # Composantes principales
res2$sdev # Valeurs propres
	

# Nouvelle visualisation avec la nouvelle ACP 

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

# Caractéristiques  des données selon le sexe et l'espèce avec pré-traitement

labelsNames <- c("Fontal lobe size (mm) / CL", "Rear width (mm) / CL", 
                "Carapace width (mm) / CL", "Body depth (mm) / CL")

pdf("./images/2_dot_3/PTcrabes_selon_sexe.pdf")
plot(crabsquant2, main="Caractéristiques morphologiques\n des crabes selon leur sexe", 
	pch=21, 
	col=c("firebrick", "darkcyan")[crabs[,2]],
	labels=labelsNames	
	)
#legend("topright", 200,legend=c("Femelle", "Mâle"), 
#	col = c("firebrick", "darkcyan"), pch = 21, inset=0) 
dev.off()

pause()

pdf("./images/2_dot_3/PTcrabes_selon_espece.pdf")
plot(crabsquant2, main="Caractéristiques morphologiques \ndes crabes selon leur espèce", 
	pch=21, 
	col=c("lightslateblue", "orangered")[crabs[,1]],
	labels=labelsNames)
#legend("bottomright", 200,legend=c("Espèce bleue", "Espèce orange"), 
#	col = c("lightslateblue", "orangered"), pch = 21, inset=0)
dev.off()