library(sm) # Chargement de sm

library(MASS)
data(crabs)
crabsquant <- crabs[,4:8]


############################### Question 1 ############################### 

table(crabs$sex) # Autant de femelles que de mâles
table(crabs$sp) # Autant d'espèces O que d'espèces B

labelsNames <- c("Fontal lobe size (mm)", "Rear width (mm)", "Carapace length (mm)",
                "Carapace width (mm)", "Body depth (mm)")

pdf("./images/1_dot_2/crabes_selon_sexe.pdf")
plot(crabsquant, main="Caractéristiques morphologiques\n des crabes selon leur sexe", 
	pch=21, 
	col=c("firebrick", "darkcyan")[crabs[,2]],
	labels=labelsNames	
	)
legend("bottomright", 200,legend=c("Femelle", "Mâle"), 
	col = c("firebrick", "darkcyan"), pch = 21, inset=0)
# Il semblerait que le sexe impacte notablement le paramètre "Rear width" 
dev.off()

pause()

inter <- seq(min(crabs$RW), max(crabs$RW), by=(max(crabs$RW)-min(crabs$RW))/10)
# Création d'un vecteur commencant par le plus petit RW et se terminant par le plus grand RW
# Le pas est de (1/10 * max-min) -> le vecteur contient donc 11 valeurs
hF <- hist(plot=F, crabs$RW[crabs$sex=='F'], breaks=inter)
# Une list de 'breaks' (les valeurs limites de chaque 'bin') et du nombre de valeurs dans chaque bin 
# est retournée.
hM <- hist(plot=F, crabs$RW[crabs$sex=='M'], breaks=inter)
# Une list de 'breaks' (les valeurs limites de chaque 'bin') et du nombre de valeurs dans chaque bin 
# est retournée.

pdf("./images/1_dot_2/hist_rear_width_fonction_sexe.pdf")
barplot(rbind(hF$counts,hM$counts),space=0,
#legend=levels(crabs$sex), main="Rear width (mm) en fonction du sexe", 
#	col=c('firebrick', 'darkcyan'))
dev.off()

pause()

pdf("./images/1_dot_2/boxplot_rear_width_fonction_sexe.pdf")
boxplot(crabs$RW[crabs$sex=='F'], crabs$RW[crabs$sex=='M'],
	main="Rear width (mm) en fonction du sexe",
	names=c("Femelle", "Mâle"), ylab="Rear width (mm)")
dev.off()

pause()


pdf("./images/1_dot_2/crabes_selon_espece.pdf")
plot(crabsquant, main="Caractéristiques morphologiques \ndes crabes selon leur espèce", 
	pch=21, 
	col=c("lightslateblue", "orangered")[crabs[,1]],
	labels=labelsNames)
#legend("bottomright", 200,legend=c("Espèce bleue", "Espèce orange"), 
#	col = c("lightslateblue", "orangered"), pch = 21, inset=0)
# Il semblerait que les paramètres "Carapace width" et "Fontal lobe size" soient
# impactés par l'espèce de crabe
dev.off()

pause()

inter <- seq(min(crabs$FL), max(crabs$FL), by=(max(crabs$FL)-min(crabs$FL))/10)
# Création d'un vecteur commencant par le plus petit FL et se terminant par le plus grand FL
# Le pas est de (1/10 * max-min) -> le vecteur contient donc 11 valeurs
hB <- hist(plot=F, crabs$FL[crabs$sp=='B'], breaks=inter)
# Une list de 'breaks' (les valeurs limites de chaque 'bin') et du nombre de valeurs dans chaque bin 
# est retournée.
hO <- hist(plot=F, crabs$FL[crabs$sp=='O'], breaks=inter)
# Une list de 'breaks' (les valeurs limites de chaque 'bin') et du nombre de valeurs dans chaque bin 
# est retournée.

pdf("./images/1_dot_2/hist_frontal_lobe_size_fonction_espece.pdf")
barplot(rbind(hB$counts, hO$counts), space=0,
legend=levels(crabs$sp), main="Frontal lobe size (mm) en \nfonction de la couleur", 
	col=c('lightslateblue', 'orangered'))
dev.off()

pause()

pdf("./images/1_dot_2/boxplot_frontal_lobe_size_fonction_espece.pdf")
boxplot(crabs$FL[crabs$sp=='B'], crabs$FL[crabs$sp=='O'],
	main="Frontal lobe size (mm) en \nfonction de la couleur",
	names=c("Bleu", "Orange"), ylab="Frontal lobe size (mm)")
dev.off()

pause()


############################### Question 2 ############################### 

# Graphiquement, on remarque assez facilement l'existence de corrélations, de relations linéaires
# entre toutes les variables quantitatives prises 2 à 2.

print(cov(crabsquant))

print(cor(crabsquant, method="pearson"))
# Le calcul de la matrice de variance-covariance empirique semble confirmer l'existence de corrélations entre 
# entre les différentes variables.
pause()

# Afin d'illustrer mes dires, je vais réaliser un test de corélation entre les variables 
# Carapace length (mm) et Carapace width (mm). L'hypothèse nulle de ce test est l'absence de 
# corrélation entre ces deux variables.
print(cor.test(crabsquant$CL, crabsquant$CW, method="pearson"))
# Le degré de signification (p-value) est striment inférieur à 2.2e-16. Cela signifie que pour
# un niveau de signification (alpha) de 1%, on s'autoriserait à rejeter l'hypothèse nulle.
# Il y a donc corrélation entre les deux variables.
# Cette corrélation semble linéaire.

pause()

# Afin de prouve cela, nous allons tenter une régression linéaire entre ces deux variables
lM <- lm(crabsquant$CL ~ crabsquant$CW)
print(lM) # On peut consulter le modèle (l'ordonnée à l'origine et le coefficient directeur)

pause()

pdf("./images/1_dot_2/carapace_length_fonction_carapace_width.pdf")
plot(crabsquant$CW, crabsquant$CL, col="royalblue", 
	main="Carapace length (mm) en fonction \nde Carapace width (mm)", 
	xlab="Carapace width (mm)", ylab="Carapace length (mm)")
abline(lM, col="firebrick4")
legend("bottomright", 200, legend=c("Carapace", "Linear model"), 
	col=c("royalblue", "firebrick4"), pch=c('o','-'))
dev.off()
