library(sm) # Chargement de sm

library(MASS)
data(crabs)
crabsquant <- crabs[,4:8]


############################### Question 1 ############################### 


table(crabs$sex) # Autant de femelles que de mâles
table(crabs$sp) # Autant d'espèces O que d'espèces B

labelsNames <- c("Fontal lobe size (mm)", "Rear width (mm)", "Carapace length (mm)",
                "Carapace width (mm)", "Body depth (mm)")

plot(crabsquant, main="Caractéristiques morphologiques\n des crabes selon leur sexe", 
	pch=21, 
	col=c("firebrick", "darkcyan")[crabs[,2]],
	labels=labelsNames	
	)
legend("bottomright", 200,legend=c("Femelle", "Mâle"), col = c("firebrick", "darkcyan"), pch = 21, inset=0)
# Il semblerait que le sexe impacte notablement le paramètre "Rear width" 

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

barplot(rbind(hF$counts,hM$counts),space=0,
legend=levels(crabs$sex), main="Fontal lobe size (mm) en fonction du sexe", col=c('firebrick', 'darkcyan'))

pause()

plot(crabsquant, main="Caractéristiques morphologiques des crabes selon leur espèce", 
	pch=21, 
	col=c("lightslateblue", "orangered")[crabs[,1]],
	labels=labelsNames)
legend("bottomright", 200,legend=c("Espèce bleue", "Espèce orange"), col = c("lightslateblue", "orangered"), pch = 21, inset=0)
# Il semblerait que les paramètres "Carapace width" et "Fontal lobe size" soient
# impactés par l'espèce de crabe

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

barplot(rbind(hB$counts,hO$counts),space=0,
legend=levels(crabs$sp), main="Fontal lobe size (mm) en fonction du sexe", col=c('lightslateblue', 'orangered'))