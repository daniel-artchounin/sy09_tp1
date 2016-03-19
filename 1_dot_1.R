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

which(abs(books.sel$implied_prob_winner_open - books.sel$implied_prob_winner_close)>=0.1)
unique(books.sel[which(abs(books.sel$implied_prob_winner_open - books.sel$implied_prob_winner_close)>=0.1), 15])
length(unique(books.sel[which(abs(books.sel$implied_prob_winner_open - books.sel$implied_prob_winner_close)>=0.1), 15]))

books.sel[which(abs(books.sel$implied_prob_winner_open - books.sel$implied_prob_winner_close)>=0.1), 12]

names(books.sel)

