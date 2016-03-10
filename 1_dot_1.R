books <- read.csv("anonymous-betting-data.csv")
source("pretraitements.R")
nrow(books) # nombre de paris
summary(books) # résumé des données
length(levels(books$match_uid)) # Le nombre de match
length(levels(as.factor(c(books$winner, books$loser)))) # Le nombre de parieurs
min(books$year) # Année minimale
min(books$year) # Année maximale
length(levels(books$book)) # Nombre de bookmakers
# Nombre de match annulés  2151


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



