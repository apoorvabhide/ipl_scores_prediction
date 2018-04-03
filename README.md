# ipl_scores_prediction

Using the IPL dataset from Kaggle, I've tried to predict what the final score will be.
I'm using data from all 10 seasons, but only the first innings so far, because this will probably be remarkably different while chasing.

1. ipl_simple_linear_regression.r: This is the simplest possible regression you can run. I use simply the score at the end of 6 overs and the score at the end of 15 overs to predict what the final score will be, using linear regression. The results are fairly good for a such a simple model: the adjusted R-squared value was 0.7487.

2. ipl_linear_regression_with_wickets.r: I've added the basic impact of wickets in this case. This time, I consider the wickets AND the runs, as seen at the 6 and 15 over mark, and fit a linear regression model. The results are quite good: the R-squared value is 0.7962.

3. ipl_ball_by_ball.r: I decided that overs wasn't granular enough, I wanted to test out short-term predictions on ball-by-ball data. So, I collect ball-by-ball data for X number of overs and then try to predict what the score will be 2 overs later. As of now, just to test it out, X is 8. To predict, I use a linear regression model based on balls and wickets, and add a term to account for momentum (whether the innings has gotten faster or slower recently.)
