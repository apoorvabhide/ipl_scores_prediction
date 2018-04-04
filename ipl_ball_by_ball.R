#First, let's get ball-by-ball cumulative data
setwd("~/R Work")
ipl_deliveries <- read.csv("./deliveries.csv", stringsAsFactors = FALSE, header = TRUE)

#Which match is in which season?
matches <- read.csv("./matches.csv", header = TRUE, stringsAsFactors = FALSE)
match_season <- subset(matches, select = c("id", "winner", "season"))

ipl_ball <- merge(ipl_deliveries, match_season, by.x = "match_id", by.y = "id")

#For now, consider only the first innings, get a ball-by-ball score in terms of runs and wickets
innings_one <- ipl_ball[ipl_ball$inning == 1,]
innings_one$balls_count <- rep(1)
innings_one$cumulative_balls <- ave(innings_one$balls_count, innings_one$inning, innings_one$match_id, FUN = cumsum)
cumulative_run_ball <- ave(innings_one$total_runs, innings_one$inning, innings_one$match_id, FUN = cumsum)
cumulative_wic_ball <- ave(innings_one$player_dismissed, innings_one$inning, innings_one$match_id, FUN = function(x)cumsum(x != ""))

ball_by_ball_score <- as.data.frame(cbind(innings_one$cumulative_balls, innings_one$over, innings_one$ball, cumulative_run_ball, as.numeric(cumulative_wic_ball), innings_one$inning, innings_one$match_id))
colnames(ball_by_ball_score) <- c("Balls", "Over_no", "Ball_no", "Runs", "Wickets", "Innings", "Match")

#Let's take a dynamic input
# match_number <- as.numeric(readline(prompt = "Enter Match No. between 1 and 577: "))

#As a test, I will use the first 8 overs to train the model and try to predict the score after 
#Over 10.
#The output of the model is the output of linear regression, with a correction term to account
#for which way the momentum has been swinging in the last two overs.
predicted_score_ten <- vector(mode = "numeric")
# for(match_number in 1:636)
# {
match_number <- 467
test_one <- ball_by_ball_score[ball_by_ball_score$Match == match_number,]

# predict_after <- as.numeric(readline(prompt = "When do you want to begin prediction? (>7 Overs)"))
predict_after <- 8
test_one_eight <- test_one[test_one$Over_no <= predict_after,]
test_lm <- lm(Runs ~ Balls + Wickets, test_one_eight)
summary(test_lm)
test_b_w <- data.frame(cbind(test_one[max(test_one$Balls[test_one$Over_no == (predict_after + 2)]),1], test_one[max(test_one$Balls[test_one$Over_no == (predict_after + 2)]),5]))
colnames(test_b_w) <- c("Balls", "Wickets")
#The score given by the linear regression model is:
lm_score <- predict(test_lm, test_b_w)
print(lm_score)
#Calculate the run rate at eight overs:
run_rate <- 6 * (max(test_one_eight$Runs) / max(test_one_eight$Balls))
print(run_rate)
#The run rate in the last two overs:
run_rate_2 <- 6* (max(test_one_eight$Runs[test_one_eight$Over_no == predict_after]) - max(test_one_eight$Runs[test_one_eight$Over_no == (predict_after - 2)])) / (max(test_one_eight$Balls[test_one_eight$Over_no == predict_after]) - max(test_one_eight$Balls[test_one_eight$Over_no == (predict_after - 2)]))
print(run_rate_2)
#Quantifying effect of momentum - ((run rate from last 2 overs / innings run rate) - 1) X 10
momentum_runs <- ((run_rate_2/run_rate) - 1) * 16.25
print(momentum_runs)

#Use the past matches at that venue to make another lm prediction
matches_at_venue <- subset(ball_by_ball_score, ball_by_ball_score$Match == which(matches$venue == matches$venue[matches$id[match_number]]))
matches_venue_train <- matches_at_venue[matches_at_venue$Over_no <= predict_after,]
venue_lm <- lm(Runs ~ Balls + Wickets, matches_venue_train)
summary(venue_lm)
venue_lm_score <- predict(venue_lm, test_b_w)
print(venue_lm_score)

predicted_score_ten <- ((lm_score + venue_lm_score) / 2) + momentum_runs
print(predicted_score_ten)
# }

#Collect the actual scores after 10 overs in a data frame. Only one match, ID 393, ended after
#eight, so including that especially to not fuck up the order. Attach the predictions to the
#same data frame.
actual <- subset(ball_by_ball_score, ball_by_ball_score$Over_no == 10 | ball_by_ball_score$Match == 393)
actual <- aggregate(actual$Runs ~ actual$Match, actual, max)
actual_vs_pred <- data.frame(cbind(actual, predicted_score_ten))

#Get a vector of the differences between the predictions and actual results, and get its mean
#and standard deviation.
diff <- actual_vs_pred$predicted_score_ten - actual_vs_pred$actual.Runs
mean(diff, na.rm = TRUE)
var(diff, na.rm = TRUE)
sd(diff, na.rm = TRUE)

#The mean is -0.004217496 which means there is practically no bias in the model. However, the
#standard deviation is 6.4753111, which is too large and can be narrowed. To be continued...
