#install.packages("tm")
#install.packages("SnowballC")
library(tm) # to deal with text
library(SnowballC) # to use the list of non relevant words
library(caTools)
library(randomForest)


dataSetOriginal <- read.delim(
  "Restaurant_Reviews.tsv", 
  quote = "", 
  stringsAsFactors = TRUE
) # just default the separator as "\t"

# Cleaning the text
corpus <- VCorpus(VectorSource(dataSet$Review))
corpus <- tm_map(x = corpus, content_transformer(tolower)) # put the reviews in lowercase
corpus <- tm_map(x = corpus, removeNumbers) # removing numbers from the reviews
corpus <- tm_map(x = corpus, removePunctuation) # remove ponctuations
corpus <- tm_map(x = corpus, removeWords, stopwords()) # removing non relevant words
corpus <- tm_map(x = corpus, stemDocument) # take the root of the words, simplifying the dataset
corpus <- tm_map(x = corpus, stripWhitespace) # removing whitespaces

# Creating the Bag of Words model
dtm <- DocumentTermMatrix(x = corpus)
dtm <- removeSparseTerms(x = dtm, sparse = 0.999) # filter non frequent words out (keep x%)

dataSet <- as.data.frame(as.matrix(dtm))
dataSet$Liked <- dataSetOriginal$Liked

# Encoding to factors
dataSet$Liked <- factor(dataSet$Liked, levels = c(0, 1))

# Split data
set.seed(123)

split <- sample.split(dataSet$Liked, SplitRatio = 0.8)
trainSet <- subset(dataSet, split == TRUE)
testSet <- subset(dataSet, split == FALSE)

# Fit Random Forest
classifier <- randomForest(
  x = trainSet[, -692], 
  y = trainSet$Liked, 
  ntree = 10
)

# Predict
yPred <-predict(classifier, newdata = testSet[, -692])

# Confusion Matrix
cm <- table(testSet[, 692], yPred)

# Metrics

TP <- 77 # cm[2,2]
TN <- 82 # cm[1,1]
FP <- 18 # cm[2,1]
FN <- 23 # cm[1,2]

accuracy <- (82+77)/200
acc <- (TP + TN) / (TP + TN + FP + FN)
precision <- TP / (TP + FP)
recall <- TP / (TP + FN)
f1Score <- 2 * precision * recall / (precision + recall)
