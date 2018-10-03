
# include libraries
library("syuzhet")

# Read dataset
dataset <- read.csv("APR.csv", head = TRUE)

#shuffle rows from the dataset
df <- dataset[sample(nrow(dataset)),]
review <- df$reviews.text
word.df <- as.vector(review)

# Get the sentiment value
sent.value <- get_sentiment(word.df)

# Assign label to Sentiment value
category_senti <- ifelse(sent.value > 0, "Positive", ifelse(sent.value < 0, "Negative", "Neutral"))

# Replace "," with "" in name column
product_name <- gsub(",", "", df$name)

# Create data frame for neccesary features
dataframe_final <- data.frame(product_name, df$reviews.doRecommend, df$reviews.rating, category_senti)

# remove the rows which have NA values.
df1 <- dataframe_final[complete.cases(dataframe_final),]

# spliting dataset into training and testing data
library(caTools)
set.seed(123)
split = sample.split(df1$category_senti, SplitRatio = 0.8)
training_set = subset(df1, split == TRUE)
test_set = subset(df1, split == FALSE)


# Applying Random Forest
library(randomForest)
regressor = randomForest(training_set$category_senti ~ . , data = training_set, ntrees = 10)

y_pred = predict(regressor, test_set)

library(caret)

cm <- table(y_pred,test_set$category_senti)

confusionMatrix(cm)
