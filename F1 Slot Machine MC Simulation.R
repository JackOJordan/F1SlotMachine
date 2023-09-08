# Creating Reels

r1 <- c(1,1,1,1,1,2,2,3,3,3,4,5,6,6)
r2 <- c(1,1,1,1,2,2,2,3,3,4,4,5,5,6)
r3 <- c(1,1,1,2,2,2,3,3,4,4,4,5,5,6)

# Creating Payout Table

combos <- c(555,444,333,222,111,55,44,33,22,11,5)
payouts <- c(100.0463,18,15,12,10,8,6,4,2,1,1)
df <- data.frame(combos,payouts)

# Creating Bonus Game and Checking its Expected Value

set.seed(100001)
runs <- 1000
tilePayouts <- c(25,10,5,20,10,5,10,10,5,0,15,10,20,250)
dice <- c(1:6)
totalBonusWinnings <- c()
for (sim in 1:runs) {
  bonusWinnings <- 25
  currentTile <- 0
  for(turn in 1:4){
    diceRoll <- sample(dice,1,TRUE)
    currentTile <- (currentTile + diceRoll) %% 14
    bonusWinnings <- bonusWinnings + tilePayouts[currentTile+1]
    if(tilePayouts[currentTile+1] == 0) break
  }
  totalBonusWinnings <- c(totalBonusWinnings,bonusWinnings)
}
expectedBonusPayout <- sum(totalBonusWinnings)/50000
expectedBonusPayout
table(totalBonusWinnings)

# Extra Bonus Game Analysis

# Graph

plot(table(totalBonusWinnings), xaxt = "n", main = "Pay-out Distribution in 50,000 Bonus Games", xlab = "Pay-out Amount", ylab = "Frequency")
axis(1, at = c(30,70,100,290,305,330),las = 2)

# Confidence Interval for No Jackpot Group

noJackpot <- totalBonusWinnings[totalBonusWinnings < 250]
mean_value1 <- mean(noJackpot)
n1 <- length(noJackpot)
wn1 <- n1/length(totalBonusWinnings) # Weighted n1
std1 <- sd(noJackpot)
se1 <- std1/sqrt(n1)
alpha1 <- 0.05
degsFreedom1 <- n1 - 1
t_score1 <- qt(p = alpha1/2, df = degsFreedom1, lower.tail = FALSE)
marginError1 <- t_score1 * se1
lb1 <- mean_value1 - marginError1
ub1 <- mean_value1 + marginError1
confInterval1 <- c(lb1,ub1)

# Confidence Interval for Jackpot Group

jackpot <- totalBonusWinnings[totalBonusWinnings >= 250]
mean_value2 <- mean(jackpot)
n2 <- length(jackpot)
wn2 <- n2/length(totalBonusWinnings) # Weighted n2
std2 <- sd(jackpot)
se2 <- std2/sqrt(n2)
alpha2 <- 0.05
degsFreedom2 <- n2 - 1
t_score2 <- qt(p = alpha2/2, df = degsFreedom2, lower.tail = FALSE)
marginError2 <- t_score2 * se2
lb2 <- mean_value2 - marginError2
ub2 <- mean_value2 + marginError2
confInterval2 <- c(lb2,ub2)

# Combining the two (?)

combinedMean <- wn1*mean_value1 + wn2*mean_value2
combinedMean
combinedSd <- sqrt((wn1^2)*(std1^2)+(wn2^2)*(std2^2))
combinedSd
combinedCI <- c(combinedMean - 1.96*combinedSd/sqrt(n1+n2), combinedMean + 1.96*combinedSd/sqrt(n1+n2))
combinedCI

# Monte Carlo Simulation to find RTP in 50,000 games

totalWinnings <- 0 # Stores the value of all player winnings
totalWins <- 0 # Counts the number of times a player wins
bonuses <- 0 # Counts the number of times the bonus game is activated
betAmount <- 1
for(sim in 1:50000){
  winnings <- 0
  test1 <- as.numeric(paste(c(sample(r1,1,TRUE),sample(r2,1,TRUE),sample(r3,1,TRUE)),collapse = ""))
  test2 <- floor(test1/10)
  test3 <- floor(test1/100)
  if (test1 == 555){
    bonuses <- bonuses + 1
    tilePayouts <- c(25,10,5,20,10,5,10,10,5,0,15,10,20,250)
    dice <- c(1:6)
    winnings <- betAmount*25
    currentTile <- 0
    for(turn in 1:4){
      diceRoll <- sample(dice,1,TRUE)
      currentTile <- (currentTile + diceRoll) %% 14
      winnings <- winnings + betAmount*tilePayouts[currentTile + 1]
      if(tilePayouts[currentTile + 1] == 0) break
    }
  } else if (test1 %in% combos) {
    winnings <- betAmount*df[which(combos == test1),2]
  } else if (test2 %in% combos) {
    winnings <- betAmount*df[which(combos == test2),2]
  } else if (test3 %in% combos) {
    winnings <- betAmount*df[which(combos == test3),2]
  }
  if(!(winnings == 0)) totalWins <- totalWins + 1
  totalWinnings <- totalWinnings + winnings
}
simulatedRTP <- 100*totalWinnings/(50000*betAmount)
gamesPerWin <- 50000/totalWins
gamesPerBonus <- 50000/bonuses
simulatedRTP
gamesPerWin
gamesPerBonus

# Fully-functioning Game (Select + Ctrl-Enter everything from betAmount onwards)

bank <- 25000
r1 <- c(1,1,1,1,1,2,2,3,3,3,4,5,6,6)
r2 <- c(1,1,1,1,2,2,2,3,3,4,4,5,5,6)
r3 <- c(1,1,1,2,2,2,3,3,4,4,4,5,5,6)
combos <- c(555,444,333,222,111,55,44,33,22,11,5)
payouts <- c(100.0463,18,15,12,10,8,6,4,2,1,1)
df <- data.frame(combos,payouts)

betAmount <- 5
winnings <- 0
test1 <- as.numeric(paste(c(sample(r1,1,FALSE),sample(r2,1,FALSE),sample(r3,1,FALSE)),collapse = ""))
test2 <- floor(test1/10)
test3 <- floor(test1/100)
if (test1 == 555){
  tilePayouts <- c(25,10,5,20,10,5,10,10,5,0,15,10,20,250)
  dice <- c(1:6)
  winnings <- betAmount*25
  currentTile <- 0
  for(turn in 1:4){
    diceRoll <- sample(dice,1,TRUE)
    currentTile <- (currentTile + diceRoll) %% 14
    winnings <- winnings + betAmount*tilePayouts[currentTile + 1]
    if(tilePayouts[currentTile + 1] == 0) break
  }
} else if (test1 %in% combos) {
  winnings <- betAmount*df[which(combos == test1),2]
} else if (test2 %in% combos) {
  winnings <- betAmount*df[which(combos == test2),2]
} else if (test3 %in% combos) {
  winnings <- betAmount*df[which(combos == test3),2]
}

bank <- bank - betAmount + winnings
print(c(test1,winnings,bank))
