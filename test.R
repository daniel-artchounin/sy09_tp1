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