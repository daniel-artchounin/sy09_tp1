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
data <-aggregate(x=books[,c("winner","loser")] , by=list(match=books$match_uid), FUN=unique)
test1 <- function (data){
	joueurs <- levels(as.factor(c(levels(data$winner), levels(data$loser))));
	joueur = c()
	gagne = c()
	perdu = c()
	totale = c()
	for(i in 1:length(joueurs)){
		joueur <- c(joueur, joueurs[i])
		gagne <- c(gagne, length(data[which(data$winner == joueurs[i]),]))
		perdu <- c(perdu, length(data[which(data$loser == joueurs[i]),]))
		total <- c(perdu[i-1]+gagne[i-1])
	}
	print(data.frame(joueur, gagne, perdu, total))
}

matches <- books.sel[which(!duplicated(books.sel$match_uid)),-c(1,2,3,4,5,7,8,9,10,11,12)]
matches <- matches[sort.int(as.character(matches$match_uid), index.return=T)$ix,]

players <- data.frame(
	player_uid = factor(levels(matches$winner), levels=levels(matches$winner)),
	nb_win = table(matches$winner),
	nb_lose = table(matches$loser),
	play_level = table(matches$winner)/(table(matches$winner)+table(matches$loser))
)

players <- data.frame(
	player_uid = players$player_uid,
	nb_win = players$nb_win.Freq,
	nb_lose = players$nb_lose.Freq,
	play_level = players$play_level.Freq
)

boxplot(players[,2:4])
boxplot(players[1:500,2:4])

which(abs(books.sel$implied_prob_winner_open - books.sel$implied_prob_winner_close)>=0.1)
unique(books.sel[which(abs(books.sel$implied_prob_winner_open - books.sel$implied_prob_winner_close)>=0.1), 15])
length(unique(books.sel[which(abs(books.sel$implied_prob_winner_open - books.sel$implied_prob_winner_close)>=0.1), 15]))

books.sel[which(abs(books.sel$implied_prob_winner_open - books.sel$implied_prob_winner_close)>=0.1), 12]

names(books.sel)

