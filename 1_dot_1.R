library(sm) # Chargement de sm

books <- read.csv("anonymous-betting-data.csv") # Chargement des données
source("pretraitements.R") # Prétraitements des données


############################### Question 1 ############################### 
nrow(books.sel) # Nombre de paris : 126461
summary(books.sel) # Résumé des données
length(unique(books.sel$match_uid)) # Le nombre de matchs : 25993
length(unique(c(books.sel$winner, books.sel$loser))) # Le nombre de joueurs : 1523
min(books.sel$year) # Année minimale : 2009
max(books.sel$year) # Année maximale : 2015
max(books.sel$year) - min(books.sel$year) # Période de temps : 6 ans
length(unique(books.sel$book)) # Nombre de bookmakers : 7


############################### Question 2 ###############################

# data <-aggregate(x=books[,c("winner","loser")] , by=list(match=books$match_uid), FUN=unique) # Useless
# test1 <- function (data){ # Useless
# 	joueurs <- levels(as.factor(c(levels(data$winner), levels(data$loser)))); # Useless
# 	joueur = c() # Useless
# 	gagne = c() # Useless
# 	perdu = c() # Useless
# 	totale = c() # Useless
# 	for(i in 1:length(joueurs)){ # Useless
# 		joueur <- c(joueur, joueurs[i]) # Useless
# 		gagne <- c(gagne, length(data[which(data$winner == joueurs[i]),])) # Useless
# 		perdu <- c(perdu, length(data[which(data$loser == joueurs[i]),])) # Useless
# 		total <- c(perdu[i-1]+gagne[i-1]) # Useless
# 	} # Useless
# 	print(data.frame(joueur, gagne, perdu, total)) # Useless
# } # Useless

# Tous les joueurs présents dans le tableau : books.sel
joueurs <- factor(unique(c(as.character(books.sel$winner), as.character(books.sel$loser))))

# Récupération des identifiants des match, des joueurs gagnants et des joueurs perdants
matches <- books.sel[which(!duplicated(books.sel$match_uid)),-c(1,2,3,4,5,7,8,9,10,11,12)]

# Tri selon l'identifiant du match
matches <- matches[sort.int(as.character(matches$match_uid), index.return=T)$ix,]

# Changement des modalités des variables qualitatives: "identifiant du joueur gagnant"
# et "identifiant du joueur perdant"
matches$winner <- factor(matches$winner, levels=levels(joueurs))
matches$loser <- factor(matches$loser, levels=levels(joueurs))

players <- data.frame(
	player_uid = factor(levels(matches$winner), levels=levels(matches$winner)),
	nb_win = data.frame(table(matches$winner))$Freq,
	nb_lose = data.frame(table(matches$loser))$Freq
)

players$nb_played <- players$nb_win + players$nb_lose # Calcul du nombre de match joués
players$level <- players$nb_win/(players$nb_win + players$nb_lose) # Calcul du niveau
# -> Il faudrait trouver un indicateur qui dépende également du nombre de match joués

hist(players$level, main="Catégorisation des joueurs en fonction de leur niveau", 
	xlab="Niveau") 
# Il y près de 600 joueurs vraiment nuls (ayant un ratio compris entre 0 et 0.1)
# Sont-ils réellement nuls ont n'ont-ils pas assez joué de match ?

pause()

plot(players$level, players$nb_played, main="Nombre de match joués en fontion du niveau des joueurs", 
	xlab="Niveau", ylab="Nombre de match joués") 
# Nombre de match joués en fonction du niveau du joueur

pause()

plot(players$level, players$nb_win, main="Nombre de match gagnés en fontion du niveau des joueurs", 
	xlab="Niveau", ylab="Nombre de match gagnés") 
# Nombre de match gagnés en fonction du niveau du joueur
# -> Les joueurs ayant un niveau élevé ont soit joué très peu de match soit beaucoup (les vrais bons joueurs)
# -> Notre indicateur n'est pas terrible

pause()

plot(players$level, players$nb_lose, main="Nombre de match perdus en fontion du niveau des joueurs", 
	xlab="Niveau", ylab="Nombre de match perdus") 
# Nombre de match perdus en fonction du niveau du joueur

pause()

boxplot(players[,2:4], names=c('Match joués', 'Match gagnés', 'Match perdus'),
main="Nombre de match joués, gagnés et perdus") 
# Diagramme en boites du nombre de match joués, du nombre de match gagnés et 
# du nombre de match perdus pour chaque joueur
# Au moins 75 % des joueurs ont joué moins de 30 matchs

pause()

boxplot(players[1:500,2:4]) # Diagramme en boites du nombre de match joués, du nombre de match gagnés et 
# du nombre de match perdus pour les 500 premiers joueurs du tableau (inutile vu que les joueurs 
# ne sont pas triés selon leur identifiant)

pause()


############################### Question 3 ###############################

winnerProbabilityEvolution <- books.sel$implied_prob_winner_close - books.sel$implied_prob_winner_open 
# Evolutions de probabilité de gain du match pour le gagnant

loserProbabilityEvolution <- books.sel$implied_prob_loser_close - books.sel$implied_prob_loser_open
# Evolutions de probabilité de gain du match pour le perdant

books.sel$absProbabilityEvolution <- abs(books.sel$implied_prob_winner_open - books.sel$implied_prob_winner_close)
# Evolutions de probabilité de gain du match en valeur absolu

unique(books.sel[which(books.sel$absProbabilityEvolution >= 0.1), 15])
# Les match suspects

length(unique(books.sel[which(books.sel$absProbabilityEvolution >=0.1), 15]))
# Nombre de match suspects : 2798

matchProbEvolution <- aggregate(absProbabilityEvolution~match_uid, books.sel, FUN=max) 
# Evolution maximale de probabilité de chaque match

suspectMatches <- matchProbEvolution[which(matchProbEvolution[2] >= 0.1), c(1, 2)]
# Restriction sur les match suspects

hist(suspectMatches$absProbabilityEvolution, main="Catégorisation des match suspects à partir de leur évolution maximale de probabilité", 
	xlab="Niveau")
# Caractérisation des matchs suspects

pause()

length(unique(books.sel[,12])) # Nb total de bookmakers

books.sel[which(abs(books.sel$absProbabilityEvolution)>=0.1), 12] 
# Noms des bookmakers impliqués dans des paris suspects

length(unique(books.sel[which(books.sel$absProbabilityEvolution>=0.1), 12]))
# Nombre de bookmakers impliqués dans des paris suspects

numberOfSuspiciousBets <- table(books.sel[which(books.sel$absProbabilityEvolution>=0.1), 12])
# Nombre de paris suspects dans lesquels sont impliqués chaque bookmaker

barplot(numberOfSuspiciousBets, main="Nombre de paris suspects dans lesquels sont impliqués chaque bookmaker")
# Diagramme bâtons représentants les nombres de paris suspects dans lesquels sont
# impliqués chaque bookmaker

pause()

unique(books.sel[which(books.sel$absProbabilityEvolution>=0.1),13])
# Perdants supects

length(unique(books.sel[which(books.sel$absProbabilityEvolution>=0.1),13]))
# Nombre de perdants supects : 559

unique(books.sel[which(books.sel$absProbabilityEvolution>=0.1),14])
# Gagnants suspects

length(unique(books.sel[which(books.sel$absProbabilityEvolution>=0.1),14]))
# Nombre de gagnants supects : 455

unique(c(as.character(books.sel[which(books.sel$absProbabilityEvolution>=0.1),13]), as.character(books.sel[which(books.sel$absProbabilityEvolution>=0.1),14])))
# Joueurs suspects

length(unique(c(as.character(books.sel[which(books.sel$absProbabilityEvolution>=0.1),13]), as.character(books.sel[which(books.sel$absProbabilityEvolution>=0.1),14]))))
# Nombre de joueurs suspects, tjrs les mêmes...

suspectBets <- books.sel[books.sel$absProbabilityEvolution >= 0.1, c(15, 13)]
# Les identifiants des match et les identifiants des perdants des paris suspects

suspectMatchesWthLoser <- suspectBets[which(!duplicated(suspectBets$match_uid)), c(1, 2)]
# On enlève les duplications

suspectLosers <- data.frame(table(suspectMatchesWthLoser$loser))
# Nombre de match suspects perdus par chaque joueur

suspectLosers <- suspectLosers[suspectLosers$Freq >= 10, c(1,2)]
# Nombre de match suspects perdus par chaque joueur considéré comme suspects

length(suspectLosers[,2])
# Nombre de perdants supects : 104

hist(suspectLosers[,2], main="Catégorisation des perdants suspects à partir du nombre de match suspects dans lesquels ils sont impliqués")
# Caractérisation des perdants suspects

pause()