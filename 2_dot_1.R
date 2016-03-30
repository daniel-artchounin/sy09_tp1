library(sm) # Chargement de sm


############################### Question 1 ############################### 
X <- matrix(c(3, 4, 3, 1, 4, 3, 2, 3, 6, 4, 1, 2), nrow=4, byrow=T)
X <- scale(X, scale = FALSE) # Centrage de la matrice
p <- dim(X)[2] # Initial number of variables
n <- dim(X)[1] # Initial number of individuals
DP <- diag(n)/n
M <- diag(p)
V <- t(X) %*% DP %*% X
EIGEN <- eigen(M %*% V %*% M)
U <- EIGEN$vectors
LAMBDA <- EIGEN$values

# Pourcentage d'inertie expliquée par l'axe 1
axis1Percentage <- (LAMBDA[1]/sum(LAMBDA))*100

# Pourcentage d'inertie expliquée par l'axe 2
axis2Percentage <- (LAMBDA[2]/sum(LAMBDA))*100

# Pourcentage d'inertie expliquée par l'axe 3
axis3Percentage <- (LAMBDA[3]/sum(LAMBDA))*100

# Pourcentage d'inertie expliquée par le sous espace vectoriel E1
vectorSubspace1Percentage <- (LAMBDA[1]/sum(LAMBDA))*100

# Pourcentage d'inertie expliquée par le sous espace vectoriel E2
vectorSubspace2Percentage <- (sum(LAMBDA[1:2])/sum(LAMBDA))*100

# Pourcentage d'inertie expliquée par le sous espace vectoriel E3
vectorSubspace3Percentage <- (sum(LAMBDA)/sum(LAMBDA))*100

pause()


############################### Question 2 ############################### 
k <- 2 # Pour le premier plan factoriel
C <- X %*% M %*% U[, 1:k] # Principal components

pdf("./images/2_dot_1/individus_premier_plan_factorial.pdf")
plot(C, col="royalblue", main="Les quatre individus dans le \npremier plan factoriel", 
	xlab="u1", ylab="u2")
dev.off()

pause()


############################### Question 3 ############################### 

XBis <- matrix(c(3, 4, 3, 1, 4, 3, 2, 3, 6, 4, 1, 2), nrow=4, byrow=T)
XBis <- t(XBis) # On fait effectue les même calculs avec la transposée de X
XBis <- scale(XBis, scale = FALSE) # Centrage de la matrice
pBis <- dim(XBis)[2] # Initial number of individuals
nBis <- dim(XBis)[1] # Initial number of variables
DPBis <- diag(nBis)/nBis
MBis <- diag(pBis)
VBis <- t(XBis) %*% DPBis %*% XBis
EIGENBis <- eigen(MBis %*% VBis %*% MBis)
UBis <- EIGENBis$vectors
LAMBDABis <- EIGENBis$values

kBis <- 2 # Pour le premier plan factoriel
CBis <- XBis %*% MBis %*% UBis[, 1:kBis] # Principal components

pdf("./images/2_dot_1/variables_premier_plan_factorial.pdf")
plot(CBis, col="royalblue", main="Les trois variables dans le \npremier plan factoriel", 
	xlab="u1", ylab="u2")
dev.off()

pause()


############################### Question 4 ############################### 

C <- X %*% M %*% U # Principal components

X1 <- C[,1:1] %*% t(U[,1:1]) # Pour k=1
X2 <- C[,1:2] %*% t(U[,1:2]) # Pour k=2 
X3 <- C[,1:3] %*% t(U[,1:3]) # Pour k=3
# On retrouve avec X3 la matrice initiale après centrage (aux 
# erreurs de calcul près liées à la précision machine)