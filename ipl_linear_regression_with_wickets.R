#Must somehow add a column with wickets after each over
setwd("C://Users/apoorva.bhide/Documents/R files")
ipl_deliveries <- read.csv("./deliveries.csv", stringsAsFactors = FALSE, header = TRUE)

#Which match is in which season?
matches <- read.csv("./matches.csv", header = TRUE, stringsAsFactors = FALSE)
match_season <- subset(matches, select = c("id", "winner", "season"))

ipl_ball <- merge(ipl_deliveries, match_season, by.x = "match_id", by.y = "id")

#Let's subset only the first innings part, because the two innings are gonna be different
innings_one <- ipl_ball[ipl_ball$inning == 1,]
over_by_over <- aggregate(innings_one$total_runs ~ innings_one$over + innings_one$inning + innings_one$match_id + innings_one$season, innings_one, sum)
wickets_by_over <- aggregate(innings_one$player_dismissed ~innings_one$over + innings_one$inning + innings_one$match_id + innings_one$season, innings_one, function(x)sum(x != ""))
cumulative_wickets <- ave(wickets_by_over$`innings_one$player_dismissed`, wickets_by_over$`innings_one$inning`, wickets_by_over$`innings_one$match_id`, FUN = cumsum)
cumulative_score <- ave(over_by_over$`innings_one$total_runs`, over_by_over$`innings_one$inning`, over_by_over$`innings_one$match_id`, FUN = cumsum)
over_by_over_wic <- cbind(over_by_over, cumulative_score, cumulative_wickets)

over_by_over_08_wic <- over_by_over_wic
score_by_over_wic <- split(over_by_over_08_wic, f = list(over_by_over_08_wic$`innings_one$match_id`, over_by_over_08_wic$`innings_one$inning`))
#Convert the awkward score_by_over_08 list to a data frame with all the scores
score_by_over_wic_df <- as.data.frame(score_by_over_08[[1]][[1]])
colnames(score_by_over_wic_df) <- "Overs"

for(i in 1:577)
{
  runs_wic <- as.data.frame(cbind(score_by_over_wic[[i]][1], score_by_over_wic[[i]][6], score_by_over_wic[[i]][7])) 
  colnames(runs_wic) <- c("Overs", "Runs", "Wickets")
  score_by_over_wic_df <- merge(score_by_over_wic_df, runs_wic, by = "Overs", all = TRUE)
}
names_wic_df <- vector(mode = "character")
names_wic_df[1] <- "Overs"

for(i in 2:1155)
{
  if(i %% 2 == 0)
  {
    names_wic_df[i] <- paste0("Match ", (i %/% 2), "Runs") 
  }
  else
  {
    names_wic_df[i] <- paste0("Match", (i %/% 2), "Wickets" )
  }
}

colnames(score_by_over_wic_df) <- names_wic_df
#Calculate the maximum score in each innings (might not always be the 20th over score) and maximum wickets
max_score_wic <- vector(mode = "integer")
max_score_wic[1] <- 20
for(i in 2:1155)
{
  max_score_wic[i] <- max(score_by_over_wic_df[i], na.rm = TRUE)
}
score_by_over_wic_df <- rbind(score_by_over_wic_df, max_score_wic)

#Take transpose so the variable to predict is a column
t_score_wic_df <- as.data.frame((t(score_by_over_wic_df)))

#Let's separate out the wickets
wickets_set <- data.frame()
for(i in 2:1155)
{
  if(i %% 2 != 0)
  {
    wickets_set <- rbind(wickets_set, t_score_wic_df[i,])
  }
}

#Delete the wickets rows from the data frame
toDelete <- seq(3, 1155, 2)
t_score_wic_df <- t_score_wic_df[-toDelete,]

#cbind the wickets to the end of this
overs <- t_score_wic_df[1,]
wickets_set <- rbind(overs, wickets_set)
wickets_names <- vector(mode = "character")
for(i in 1:20)
{
  wickets_names[i] <- paste0("Over", i, "Wickets")
}
wickets_names[21] <- "total_wickets"
colnames(wickets_set) <- wickets_names
runs_names <- vector(mode = "character")
for(i in 1:20)
{
  runs_names[i] <- paste0("Over", i, "Runs")
}
runs_names[21] <- "total_runs"
colnames(t_score_wic_df) <- runs_names
t_score_wic_df <- cbind(t_score_wic_df, wickets_set)

#Now, build the linear model
linear_model_wic <- lm(t_score_wic_df$total_runs ~ t_score_wic_df$Over6Runs + t_score_wic_df$Over6Wickets + t_score_wic_df$Over15Runs + t_score_wic_df$Over15Wickets, t_score_08_df)
summary(linear_model_wic)