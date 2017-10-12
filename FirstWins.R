##################################################
#PROBLEM 32: Two players alternately roll an n-sided die. The player who fails to improve
# upon the previous roll loses. What is the probability that the first player wins?
##################################################
#TIPS: Construct a series of Booleans based on whether the latter point outnumbers 
# the former one. The number of times when the first player wins equals the number 
# of 1s at the odd index plus the number of 0s at the even index in the series.
# ################################################

"FirstWins" <- function(n = 10, nter = 1000){
  roll <- 1:nter
  x <- roll
  score1 <- 0
  x[1] <- 0
  roll[1] <- sample.int(n, size = 1, replace = TRUE)
  for(i in 2:nter){
    roll[i] <- sample.int(n, size = 1, replace = TRUE)
    x[i] <- (roll[i-1] < roll[i])
    score1 <- score1 + (x[i] == i%%2)
  }
  freq1 <- score1/(nter-1)
}

haveatry <- FirstWins(6, 1000000)
