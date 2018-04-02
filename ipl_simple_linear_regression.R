setwd("~/R Work")
ipl_deliveries <- read.csv("./deliveries.csv", stringsAsFactors = FALSE, header = TRUE)

#Which match is in which season?
matches <- read.csv("./matches.csv", header = TRUE, stringsAsFactors = FALSE)
match_season <- subset(matches, select = c("id", "winner", "season"))

ipl_ball <- merge(ipl_deliveries, match_season, by.x = "match_id", by.y = "id")

#Let's subset only the first innings part, because the two innings are gonna be different
innings_one <- ipl_ball[ipl_ball$inning == 1,]
#Get runs in each over, cumulative score after each over, split the scores by innings
over_by_over <- aggregate(innings_one$total_runs ~ innings_one$over + innings_one$inning + innings_one$match_id + innings_one$season, innings_one, sum)
cumulative_score <- ave(over_by_over$`innings_one$total_runs`, over_by_over$`innings_one$inning`, over_by_over$`innings_one$match_id`, FUN = cumsum)
over_by_over <- cbind(over_by_over, cumulative_score)
over_by_over_08 <- over_by_over[over_by_over$`innings_one$season` <= 2010,]
score_by_over_08 <- split(over_by_over_08, f = list(over_by_over_08$`innings_one$match_id`, over_by_over_08$`innings_one$inning`))


#Plot the worm from the first match in 2008
scatter.smooth(x = unlist(score_by_over_08[[22]][[1]]), y = unlist(score_by_over_08[[22]][[6]]), xlab = "Overs", ylab = "Runs", main = "Run progression in IPL 1, Match 22")
#Convert the awkward score_by_over_08 list to a data frame with all the scores
score_by_over_08_df <- as.data.frame(score_by_over_08[[1]][[1]])

colnames(score_by_over_08_df) <- "Overs"
for(i in 1:175)
{
  runs <- as.data.frame(cbind(score_by_over_08[[i]][1], score_by_over_08[[i]][6])) 
  colnames(runs) <- c("Overs", "Runs")
  score_by_over_08_df <- merge(score_by_over_08_df, runs, by = "Overs", all = TRUE)
}
names_df <- vector(mode = "character")
names_df[1] <- "Overs"
for(i in 2:176)
{
  names_df[i] <- paste0("Match ", i-1) 
}

colnames(score_by_over_08_df) <- names_df


max_score <- vector(mode = "integer")
max_score[1] <- 20
for(i in 2:176)
{
  max_score[i] <- max(score_by_over_08_df[i], na.rm = TRUE)
}
score_by_over_08_df <- rbind(score_by_over_08_df, max_score)
t_score_08_df <- as.data.frame((t(score_by_over_08_df)))

#f <- as.formula(paste('V21 ~', paste(colnames(t_score_08_df)[1:15], collapse='+')))
linear_model <- lm(t_score_08_df$V21 ~ t_score_08_df$V6 + t_score_08_df$V15, t_score_08_df)
summary(linear_model)
predict(linear_model)