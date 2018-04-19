setwd("~/R Work")
ipl_deliveries <- read.csv("./deliveries.csv", stringsAsFactors = FALSE, header = TRUE)

#Which match is in which season?
matches <- read.csv("./matches.csv", header = TRUE, stringsAsFactors = FALSE)
match_season <- subset(matches, select = c("id", "winner", "season"))

ipl_ball <- merge(ipl_deliveries, match_season, by.x = "match_id", by.y = "id")

#Let's subset only the first innings part, because the two innings are gonna be different
innings_one <- ipl_ball[ipl_ball$inning == 1,]
innings_one$balls_count <- rep(1)
innings_one$balls_count[innings_one$wide_runs > 0 | innings_one$noball_runs > 0] <- 0
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
final_score_lin <- lm(Score ~ Runs + Wickets + Balls, ball_by_ball_score)
summary(final_score_lin)
testing_match <- ball_by_ball_score[ball_by_ball_score$Match == 274,]
predictions <- data.frame()

for(i in 1:length(testing_match$Balls))
{
runs_test <- testing_match$Runs[i]
wickets_test <- testing_match$Wickets[i]
balls_test <- testing_match$Balls[i]
test_random <- data.frame(cbind(runs_test,wickets_test,balls_test))
colnames(test_random) <- c("Runs", "Wickets", "Balls")

# wicket_factor <- (1.01) * ((1.01) ^ ((1 - wickets_test))) * ((1.01) ^ ((2*(runs_test / balls_test) - 1)))
  wicket_factor <- 1

predict_random <- predict(final_score_lin, test_random)
predict_random_w <- predict_random*wicket_factor

#Let's look only at similar games: train lm model on similar matches
matches_over_test <- ball_by_ball_score[ball_by_ball_score$Balls == balls_test,]
matches_over_test$Ball_no <- NULL
similar_matches <- subset(matches_over_test, matches_over_test$Runs <= runs_test + 3 & matches_over_test$Runs >= runs_test - 3 & matches_over_test$Wickets <= wickets_test + 1 & matches_over_test$Wickets >= wickets_test - 1)
similar_set <- subset(ball_by_ball_score, ball_by_ball_score$Match %in% similar_matches$Match)
similar_model <- lm(Score ~ Runs + Wickets + Balls, similar_set)
summary(similar_model)
predict_similar <- predict(similar_model, test_random)
predict_similar_w <- predict_similar*wicket_factor

#Let's look at the average score at that venue
city_ex <- "Chandigarh"
city_matches <- subset(matches, matches$city == city_ex)
matches_venue <- subset(ball_by_ball_score, ball_by_ball_score$Match %in% city_matches$id)
venue_model <- lm(Score ~ Runs + Wickets + Balls, matches_venue)
summary(venue_model)
predict_venue <- predict(venue_model, test_random)
predict_venue_w <- predict_venue*wicket_factor

predict_w <- data.frame(cbind(predict_random_w, predict_similar_w, predict_venue_w))
predictions <- rbind(predictions, predict_w)
}

predictions$index <- seq(1, length(predictions$predict_similar_w), 1)

plot(x = predictions$index, y = predictions$predict_similar_w)
ggplot(predictions, )