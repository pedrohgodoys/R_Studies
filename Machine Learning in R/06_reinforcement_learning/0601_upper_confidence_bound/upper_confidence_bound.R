
dataSet <- read.csv("Ads_CTR_Optimisation.csv")

# Implementing UCB
N <-  nrow(dataSet)
d <- 10 #number of Ads

adsSelected <- integer(d)
numbersOfSelections <- integer(d)
sumsOfRewards <- integer(d)
totalReward <- 0

for (n in 1:N) {
  ad <- 0
  maxUpperBound <- 0

  for (i in 1:d) {
    if  (numbersOfSelections[i] > 0) {
      averageReward  <- sumsOfRewards[i]/ numbersOfSelections[i]
      delta_i <-  sqrt(3/2 * log(n) / numbersOfSelections[i])
      upperBound <- averageReward + delta_i
    } else {
      upperBound <- 1e400
    }

    if (upperBound > maxUpperBound) {
        maxUpperBound <-  upperBound
        ad <- i 
    }
  }
    
  adsSelected <- append(adsSelected, ad) 
  numbersOfSelections[ad] <- numbersOfSelections[ad] + 1
  reward <- dataSet[n, ad]
  sumsOfRewards[ad] <- sumsOfRewards[ad] + reward
  totalReward <-  totalReward + reward

}

# Visualizing - histogram
hist(
  adsSelected,
  col = "royalblue",
  main  = "Histogram of Ads Selections",
  xlab = "Ads", 
  ylab = "Number of times each ad was selected"
)
