setwd("~/R Work")
ipl_deliveries <- read.csv("./deliveries.csv", stringsAsFactors = FALSE, header = TRUE)

#Which match is in which season?
matches <- read.csv("./matches.csv", header = TRUE, stringsAsFactors = FALSE)
match_season <- subset(matches, select = c("id", "winner", "season"))

ipl_ball <- merge(ipl_deliveries, match_season, by.x = "match_id", by.y = "id")

#Let's subset only the first innings part, because the two innings are gonna be different
innings_one <- ipl_ball[ipl_ball$inning == 1,]
innings_one$balls_count <- rep(1)
innings_one$cumulative_balls <- ave(innings_one$balls_count, innings_one$inning, innings_one$match_id, FUN = cumsum)
cumulative_run_ball <- ave(innings_one$total_runs, innings_one$inning, innings_one$match_id, FUN = cumsum)
cumulative_wic_ball <- ave(innings_one$player_dismissed, innings_one$inning, innings_one$match_id, FUN = function(x)cumsum(x != ""))

ball_by_ball_score <- as.data.frame(cbind(innings_one$cumulative_balls, innings_one$over, innings_one$ball, cumulative_run_ball, as.numeric(cumulative_wic_ball), innings_one$inning, innings_one$match_id))
colnames(ball_by_ball_score) <- c("Balls", "Over_no", "Ball_no", "Runs", "Wickets", "Innings", "Match")
ball_by_ball_score$Innings <- NULL
final_score <- aggregate(ball_by_ball_score$Runs ~ ball_by_ball_score$Match, ball_by_ball_score, max)
colnames(final_score) <- c("Match", "Score")
ball_by_ball_score <- merge(ball_by_ball_score, final_score, by = "Match")

#Fit a linear model on the ball-by-ball data
final_score_lin <- lm(ball_by_ball_score$Score ~ ball_by_ball_score$Runs + ball_by_ball_score$Wickets, ball_by_ball_score)
summary(final_score_lin)


