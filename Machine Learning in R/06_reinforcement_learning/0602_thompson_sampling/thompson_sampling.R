
dataSet <- read.csv("Ads_CTR_Optimisation.csv")

# Implementing Thompson Sampling
N <-  nrow(dataSet)
d <- 10 #number of Ads

adsSelected <- integer(d)
numbersOfRewards1 <- integer(d)
numbersOfRewards0 <- integer(d)
totalReward <- 0

for (n in 1:N) {
  ad <- 0
  maxRandom <- 0

  for (i in 1:d) {
    randomBeta <- rbeta(
      n = 1, 
      shape1 = numbersOfRewards1[i] + 1, 
      shape2 = numbersOfRewards0[i] + 1
    )

    if (randomBeta > maxRandom) {
        maxRandom <-  randomBeta
        ad <- i 
    }
  }
    
  adsSelected <- append(adsSelected, ad) 
  reward <- dataSet[n, ad]
  totalReward <-  totalReward + reward
  if (reward == 1) {
    numbersOfRewards1[ad] <- numbersOfRewards1[ad] + 1
  }  else {
    numbersOfRewards0[ad] <- numbersOfRewards0[ad] + 1
  }

}


# Visualizing - histogram
hist(
  adsSelected,
  col = "royalblue",
  main  = "Histogram of Ads Selections",
  xlab = "Ads", 
  ylab = "Number of times each ad was selected"
)
