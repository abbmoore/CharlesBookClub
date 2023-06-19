### Data Information ###
# Seq#: Sequence number in the partition
# ID#: Identification number in the full (unpartitioned) market test dataset
# Gender: 0 = Male, 1 = Female
# M: Monetary—Total money spent on books
# R: Recency—Months since last purchase
# F: Frequency—Total number of purchases
# FirstPurch: Months since first purchase
# ChildBks: Number of purchases from the category child books
# YouthBks: Number of purchases from the category youth books
# CookBks: Number of purchases from the category cookbooks
# DoItYBks: Number of purchases from the category do-it-yourself books
# RefBks: Number of purchases from the category reference books (atlases, encyclopedias, dictionaries)
# ArtBks: Number of purchases from the category art books
# GeoBks: Number of purchases from the category geography books
# ItalCook: Number of purchases of book title Secrets of Italian Cooking
# ItalAtlas: Number of purchases of book title Historical Atlas of Italy
# ItalArt: Number of purchases of book title Italian Art
# Florence: 1 = the Art History of Florence was purchased ; 0 = the Art History of Florence was not purchased
# Related.Purchase: Number of related books purchased

## Recency: Amount of time since last purchase
# 0–2 months (Rcode = 1)
# 3–6 months (Rcode = 2)
# 7–12 months (Rcode = 3)
# 13 months and up (Rcode = 4)

## Frequency: Number of previous purchases from the company
# 1 book (Fcode = l)
# 2 books (Fcode = 2)
# 3 books and up (Fcode = 3)

## Monetary: Amount of money spent on the company's products
# $0–$25 (Mcode = 1)
# $26–$50 (Mcode = 2)
# $51–$100 (Mcode = 3)
# $101–$200 (Mcode = 4)
# $201 and up (Mcode = 5)

## Chose to do analysis of the following variables: Mcode, Rcode, Fcode, ArtBks, ItalArt, and Related.Purchase ##

### Import data ###
data <- read.csv('Portfolio/R/CharlesBookClub/CharlesBookClub.csv')
head(data)

### Data Preprocessing ###
colnames(data)
# Renaming a column
colnames(book)[19] = 'Related.Purchase'

# Convert categorical variables to factors
data$Florence = as.factor(data$Florence)
data$Gender = as.factor(ifelse(data$Gender == 1, 'F', 'M'))
data$Mcode = as.factor(data$Mcode)
data$Rcode = as.factor(data$Rcode)
data$Fcode = as.factor(data$Fcode)

# Split data into 60% training/ 40% validation split
set.seed(1)
train.index <- sample(1:nrow(data), 0.6*nrow(data))
train <- data[train.index,]
valid <- book[-train.index,]

### Exploratory Data Analysis ###
library(ggplot2)

## Comparison of Gender and Florence variables
gender_n <- aggregate(train$Gender, list(train$Gender), length)
# Renaming columns 
names(gender_n) <- c('Gender', 'gender_n')
gender_n
# there are 1686 females in the train data
# there are 714 males in the train data

gender_florence_n <- aggregate(train$Florence, list(train$Gender, train$Florence), length)
names(gender_florence_n) <- c('Gender', 'Florence', 'gender_florence_n')
gender_florence_n
# there were 1552 females who did not purchase the Art History of Florence in the train data
# there were 134 females who purchased the Art History of Florence in the train data

# there were 639 males who did not purchase the Art History of Florence in the train data
# there were 75 males who purchased the Art History of Florence in the train data

gender_florence_fraction <- merge(gender_florence_n, gender_n, by = 'Gender')
gender_florence_fraction$fraction <- (gender_florence_fraction$gender_florence_n) / gender_florence_fraction$gender_n
gender_florence_fraction
# 92.1% of females in the train data did not purchase the Art History of Florence
# 7.9% of females in the train data did purchase the Art History of Florence

# 89.5% of males in the train data did not purchase the Art History of Florence
# 10.5% of males in the train data did purchase the Art History of Florence

# Visualization of correlation between Gender and Florence
ggplot(gender_florence_fraction, aes(Gender, fraction, fill = Florence)) + 
  geom_bar(stat = "identity") + 
  labs(title='Correlation between Gender and Florence')
# There does not seem to be any major correlation between Gender and Florence variables



## Comparison of RCode and Florence variables
Rcode_n <- aggregate(train$Rcode, list(train$Rcode), length)
names(Rcode_n) <- c('Rcode', 'Rcode_n')
Rcode_n
# There are 4 levels of R_code: 1, 2, 3, 4

Rcode_florence_n <- aggregate(train$Florence, list(train$Rcode, train$Florence), length)
names(Rcode_florence_n) <- c('Rcode', 'Florence', 'Rcode_florence_n')
Rcode_florence_n

Rcode_florence_fraction <- merge(Rcode_florence_n, Rcode_n, by = 'Rcode')
Rcode_florence_fraction$fraction <- (Rcode_florence_fraction$Rcode_florence_n) / (Rcode_florence_fraction$Rcode_n)
Rcode_florence_fraction
# 87.4 % of customers in the train data with Rcode = 1 did not purchase the Art History of Florence 
# 12.6 % of customers in the train data with Rcode = 1 did purchase the Art History of Florence 

# 85.7 % of customers in the train data with Rcode = 2 did not purchase the Art History of Florence 
# 14.3 % of customers in the train data with Rcode = 2 did purchase the Art History of Florence 

# 91.8 % of customers in the train data with Rcode = 3 did not purchase the Art History of Florence 
# 8.2 % of customers in the train data with Rcode = 3 did purchase the Art History of Florence 

# 93.2 % of customers in the train data with Rcode = 4 did not purchase the Art History of Florence 
# 6.8 % of customers in the train data with Rcode = 4 did purchase the Art History of Florence 

# Visualization of correlation between Rcode and Florence
ggplot(Rcode_florence_fraction, aes(Rcode, fraction, fill = Florence)) + 
  geom_bar(stat = "identity") + 
  labs(title='Correlation between Rcode and Florence')
# There is a slight correlation between Rcode and Florence variables
# As the Rcode level decreased, the percentage of those who purchased the Art History of Florence increased
# This makes sense, since a lower Rcode indicates a customer who has purchased from the company more recently than those with a higher Rcode



## Comparison of Mcode and Florence Variables
Mcode_n <- aggregate(train$Mcode, list(train$Mcode), length)
names(Mcode_n) <- c('Mcode', 'Mcode_n')
Mcode_n
# There are 5 levels of Mcode: 1, 2, 3, 4, 5

Mcode_florence_n <- aggregate(train$Florence, list(train$Mcode, train$Florence), length)
names(Mcode_florence_n) <- c('Mcode', 'Florence', 'Mcode_florence_n')
Mcode_florence_n

Mcode_florence_fraction <- merge(Mcode_florence_n, Mcode_n, by = 'Mcode')
Mcode_florence_fraction$fraction <- (Mcode_florence_fraction$Mcode_florence_n) / (Mcode_florence_fraction$Mcode_n)
Mcode_florence_fraction
# 95.0 % of customers in the train data with Mcode = 1 did not purchase the Art History of Florence 
# 5.0 % of customers in the train data with Mcode = 1 did purchase the Art History of Florence 

# 92.6 % of customers in the train data with Mcode = 2 did not purchase the Art History of Florence 
# 7.4 % of customers in the train data with Mcode = 2 did purchase the Art History of Florence 

# 92.0 % of customers in the train data with Mcode = 3 did not purchase the Art History of Florence 
# 8.0 % of customers in the train data with Mcode = 3 did purchase the Art History of Florence 

# 92.4 % of customers in the train data with Mcode = 4 did not purchase the Art History of Florence 
# 7.6 % of customers in the train data with Mcode = 4 did purchase the Art History of Florence 

# 90.4 % of customers in the train data with Mcode = 5 did not purchase the Art History of Florence 
# 9.6 % of customers in the train data with Mcode = 5 did purchase the Art History of Florence

# Visualization of correlation between Mcode and Florence
ggplot(Mcode_florence_fraction, aes(Mcode, fraction, fill = Florence)) + 
  geom_bar(stat = "identity") + 
  labs(title='Correlation between Mcode and Florence')
# There is a slight correlation between Mcode and Florence
# As the Mcode level increased, the percentage of those who purchased the Art History of Florence increased
# This makes sense, since a higher Mcode indicates a customer who has spent more on company products than those with a lower Mcode


## Comparison of Fcode and Florence variables
Fcode_n <- aggregate(train$Fcode, list(train$Fcode), length)
names(Fcode_n) <- c('Fcode', 'Fcode_n')
Fcode_n
#There are 3 levels of Fcode: 1, 2, 3

Fcode_florence_n <- aggregate(train$Florence, list(train$Fcode, train$Florence), length)
names(Fcode_florence_n) <- c('Fcode', 'Florence', 'Fcode_florence_n')
Fcode_florence_n

Fcode_florence_fraction <- merge(Fcode_florence_n, Fcode_n, by = 'Fcode')
Fcode_florence_fraction$fraction <- (Fcode_florence_fraction$Fcode_florence_n) / (Fcode_florence_fraction$Fcode_n)
Fcode_florence_fraction
# 92.3 % of customers in the train data with Fcode = 1 did not purchase the Art History of Florence 
# 7.7 % of customers in the train data with Fcode = 1 did purchase the Art History of Florence 

# 93.1 % of customers in the train data with Fcode = 2 did not purchase the Art History of Florence 
# 6.9 % of customers in the train data with Fcode = 2 did purchase the Art History of Florence 

# 89.2 % of customers in the train data with Fcode = 3 did not purchase the Art History of Florence 
# 10.8 % of customers in the train data with Fcode = 3 did purchase the Art History of Florence 

# Visualization of correlation between Fcode and Florence
ggplot(Fcode_florence_fraction, aes(Fcode, fraction, fill = Florence)) + 
  geom_bar(stat = "identity") + 
  labs(title='Correlation between Fcode and Florence')
# there is a slight correlation between the Fcode and Florence variables
# As the Fcode increased, the percentage of those who purchased the Art History of Florence increased
# This makes sense, since a higher Fcode indicates a greater frequency of book purchases from the company than those with a lower Fcode


## Boxplot visualization
# Mcode
ggplot(train) + geom_boxplot(aes(Florence, Mcode, group = Florence))
#Fcode
ggplot(train) + geom_boxplot(aes(Florence, Fcode, group = Florence))
#Rcode
ggplot(train) + geom_boxplot(aes(Florence, Rcode, group = Florence))


## Comparison of Artbks and Florence variables
ArtBks_n <- aggregate(train$ArtBks, list(train$ArtBks), length)
names(ArtBks_n) <- c("ArtBks", "ArtBks_n")
ArtBks_n
#There are 6 levels of ArtBks: 0, 1, 2, 3, 4, 5

ArtBks_florence_n <- aggregate(train$Florence, list(train$ArtBks, train$Florence), length)
names(ArtBks_florence_n) <- c('ArtBks', 'Florence', 'ArtBks_florence_n')
ArtBks_florence_n

ArtBks_florence_fraction <- merge(ArtBks_florence_n, ArtBks_n, by = 'ArtBks')
ArtBks_florence_fraction$fraction <- (ArtBks_florence_fraction$ArtBks_florence_n) / (ArtBks_florence_fraction$ArtBks_n)
ArtBks_florence_fraction
# 92.8 % of customers in the train data who purchased 0 Art Books did not purchase the Art History of Florence 
# 7.2 % of customers in the train data who purchased 0 Art Books did purchase the Art History of Florence 

# 89.2 % of customers in the train data who purchased 1 Art Book did not purchase the Art History of Florence 
# 10.8 % of customers in the train data who purchased 1 Art Book did purchase the Art History of Florence

# 79.3 % of customers in the train data who purchased 2 Art Books did not purchase the Art History of Florence 
# 20.7 % of customers in the train data who purchased 2 Art Books did purchase the Art History of Florence 

# 75.0 % of customers in the train data who purchased 3 Art Books did not purchase the Art History of Florence 
# 25.0 % of customers in the train data who purchased 3 Art Books did purchase the Art History of Florence 

# 50.0 % of customers in the train data who purchased 4 Art Books did not purchase the Art History of Florence 
# 50.0 % of customers in the train data who purchased 4 Art Books did purchase the Art History of Florence 

# 0 % of customers in the train data who purchased 5 Art Books did not purchase the Art History of Florence 
# 100 % of customers in the train data who purchased 5 Art Books did purchase the Art History of Florence

# Visualization of correlation between ArtBks and Florence
ggplot(ArtBks_florence_fraction, aes(ArtBks, fraction, fill = Florence)) + 
  geom_bar(stat = "identity") + 
  labs(title='Correlation between ArtBks and Florence')
# There is a strong correlation between ArtBks and Florence variables
# The more art books purchased by the customer in the train data, the greater the percentage of customers who did purchase the Art History of Florence
# This makes sense, since the Art History of Florence is an art book, which those who purchase more books of that type are more likely to purchase this book



## Comparison of ItalArt and Florence variables
ItalArt_n <- aggregate(train$ItalArt, list(train$ItalArt), length)
names(ItalArt_n) <- c('ItalArt', 'ItalArt_n')
ItalArt_n
#There are 3 levels of ItalArt: 0, 1, 2

ItalArt_florence_n <- aggregate(train$Florence, list(train$ItalArt, train$Florence), length)
names(ItalArt_florence_n) <- c('ItalArt', 'Florence', 'ItalArt_florence_n')
ItalArt_florence_n

ItalArt_florence_fraction <- merge(ItalArt_florence_n, ItalArt_n, by = 'ItalArt')
ItalArt_florence_fraction$fraction <- (ItalArt_florence_fraction$ItalArt_florence_n) / (ItalArt_florence_fraction$ItalArt_n)
ItalArt_florence_fraction
# 91.5 % of customers in the train data who purchased 0 Italian Art Books did not purchase the Art History of Florence 
# 8.5 % of customers in the train data who purchased 0 Italian Art Books did purchase the Art History of Florence 

# 87.3 % of customers in the train data who purchased 1 Italian Art Book did not purchase the Art History of Florence 
# 12.7 % of customers in the train data who purchased 1 Italian Art Book did purchase the Art History of Florence

# 77.8 % of customers in the train data who purchased 2 Italian Art Books did not purchase the Art History of Florence 
# 22.2 % of customers in the train data who purchased 2 Italian Art Books did purchase the Art History of Florence 

# Visualization of correlation between ItalArt and Florence
ggplot(ItalArt_florence_fraction, aes(ItalArt, fraction, fill = Florence)) + 
  geom_bar(stat = "identity") + 
  labs(title='Correlation between ItalArt and Florence')
# There is a correlation between ItalArt and Florence variables
# The more copies of the Italian Art book purchased by the customer in the train data, the greater the percentage of customers who did purchase the Art History of Florence
# This makes sense, since the Art History of Florence is similar to the Italian Art book, which those who purchase more books of a similar book type are more likely to purchase this book



## Comparison of Related.Purchase and Florence variables
Related.Purchase_n <- aggregate(train$Related.Purchase, list(train$Related.Purchase), length)
names(Related.Purchase_n)<- c('Related.Purchase', 'Related.Purchase_n')
Related.Purchase_n
# There are 8 levels of Related.Purchase: 0, 1, 2, 3, 4, 5, 6, 7, 8

Related.Purchase_florence_n <- aggregate(train$Florence, list(train$Related.Purchase, train$Florence), length)
names(Related.Purchase_florence_n) <- c('Related.Purchase', 'Florence', 'Related.Purchase_florence_n')
Related.Purchase_florence_n

Related.Purchase_florence_fraction <- merge(Related.Purchase_florence_n, Related.Purchase_n, by = 'Related.Purchase')
Related.Purchase_florence_fraction$fraction <- (Related.Purchase_florence_fraction$Related.Purchase_florence_n) / (Related.Purchase_florence_fraction$Related.Purchase_n)
Related.Purchase_florence_fraction
# 93.2 % of customers in the train data who purchased 0 related books did not purchase the Art History of Florence 
# 8.5 % of customers in the train data who purchased 0 related books did purchase the Art History of Florence 

# 92.1 % of customers in the train data who purchased 1 related book did not purchase the Art History of Florence 
# 7.9 % of customers in the train data who purchased 1 related book did purchase the Art History of Florence

# 89.7 % of customers in the train data who purchased 2 related books did not purchase the Art History of Florence 
# 10.3 % of customers in the train data who purchased 2 related books did purchase the Art History of Florence 

# 84.2 % of customers in the train data who purchased 3 related books did not purchase the Art History of Florence 
# 12.8 % of customers in the train data who purchased 3 related books did purchase the Art History of Florence 

# 80.8 % of customers in the train data who purchased 4 related books did not purchase the Art History of Florence 
# 19.2 % of customers in the train data who purchased 4 related books did purchase the Art History of Florence 

# 65.2 % of customers in the train data who purchased 5 related books did not purchase the Art History of Florence 
# 34.8 % of customers in the train data who purchased 5 related books did purchase the Art History of Florence 

# 92.3 % of customers in the train data who purchased 6 related books did not purchase the Art History of Florence 
# 7.7 % of customers in the train data who purchased 6 related books did purchase the Art History of Florence 

# 75.0 % of customers in the train data who purchased 7 related books did not purchase the Art History of Florence 
# 25.0 % of customers in the train data who purchased 7 related books did purchase the Art History of Florence 

# Visualization of correlation between Related.Purchase and Florence
ggplot(Related.Purchase_florence_fraction, aes(Related.Purchase, fraction, fill = Florence)) + 
  geom_bar(stat = "identity") + 
  labs(title='Correlation between Related.Purchase and Florence')
# There is a strong correlation between Related.Purchase and Florence variables
# Overall, the more related books purchased by the customer in the train data, the greater the percentage of customers who did purchase the Art History of Florence
# This makes sense, since the purchased books are similar to the Art History of Florence, and are more likely to enjoy the Art History of Florence

## RFM Segmentation
#Train response rate using all data
train_response_rate = sum(train$Florence == 1) / nrow(train)
train_response_rate

# Train response rate per RFM combination categories
response_rates_by_rfm_group <- aggregate(Florence ~ Rcode + Fcode + Mcode, data = train, FUN = function(x) {mean(x == 1)})
response_rates_by_rfm_group <- response_rates_by_rfm_group[order(-response_rates_by_rfm_group$Florence), ]
response_rates_by_rfm_group

# Train response rates above train_response_rate
rfm_groups_response_rate_gt_mean <- response_rates_by_rfm_group[response_rates_by_rfm_group$Florence > train_response_rate, ]
rfm_groups_response_rate_gt_mean
#19 segments with response rate above train_response_rate

# Validation response rate above train_response_rate
validation_response_rate_gt_mean <- merge(valid, rfm_groups_response_rate_gt_mean, by = c("Rcode", "Fcode", "Mcode"))
mean(validation_response_rate_gt_mean$Yes_Florence == 1)
# Validation response rate is greater than Train response rate



## KNN using Rcode, Fcode, Mcode, FirstPurch, and RelatedPurch
# Find best k
features = c("Rcode", "Fcode", "Mcode", "FirstPurch", "Related.Purchase")

# Convert variables from factors to numeric
train.knn <- train
train.knn$Rcode <- as.numeric(as.character(train.knn$Rcode))
train.knn$Fcode <- as.numeric(as.character(train.knn$Fcode))
train.knn$Mcode <- as.numeric(as.character(train.knn$Mcode))
train.knn$Related.Purchase <- as.numeric(as.character(train.knn$Related.Purchase))

# Convert factors to the same levels
train.knn$Florence <- as.factor(train.knn$Florence)
valid.knn$Florence <- factor(valid.knn$Florence, levels = levels(train.knn$Florence))

valid.knn <- valid
valid.knn$Rcode <- as.numeric(as.character(valid$Rcode))
valid.knn$Fcode <- as.numeric(as.character(valid$Fcode))
valid.knn$Mcode <- as.numeric(as.character(valid$Mcode))
valid.knn$Related.Purchase <- as.numeric(as.character(valid$Related.Purchase))

# Normalize data
library(caret)
library(FNN)
preprocess <- preProcess(train.knn[, features], method = c('center', 'scale'))
features_train <- predict(preprocess, train.knn[, features])
features_valid <- predict(preprocess, valid.knn[, features])
k_val <- 1:11


# loop through k values to find best k
for(k in k_val) {
  pred <- knn(train = features_train, test = features_valid, cl = relevel(train$Florence, ref = "1"), k = k)
  pred <- factor(pred, levels = levels(train.knn$Florence))
  cm <- confusionMatrix(data = pred, reference = valid.knn$Florence, positive = "1")
  print(k)
  print(cm)
}

# Chose k=4 due to high accuracy and specificity
pred <- knn(train = features_train, test = features_valid, cl = relevel(train.knn$Florence, ref = '1'), k = 7)
valid.knn$pred <- pred
valid.knn$pred

# Sort based on predicted probabilities
lift_df <- data.frame(actual = valid.knn$Florence, predicted = valid.knn$pred)
lift_df

# Positive cases
positive <- sum(lift_df$actual == 1)
positive
#129 positive cases

# Cumulative response rate
cum_response_rate <- cumsum(lift_df$actual == 1) / positive

# Cumulative lift
cum_lift <- cum_response_rate / (1:length(lift_df$actual))

# Plot lift chart
plot(1:length(lift_df$actual), cum_lift, type = "l", xlab = "# cases", ylab = "Cumulative Lift",
     main = "KNN Lift Chart")

## KNN Prediction
# Train model
train.knn$Florence <- as.numeric(as.character(train.knn$Florence))
model <- knnreg(features_train, features_valid, y = train.knn$Florence, k = 4)

# Make predictions on validation data
pred <- predict(model, features_valid)
valid.knn$pred <- pred

# Sort based on predicted probabilities
lift_df <- data.frame(actual = valid.knn$Florence, predicted = valid.knn$pred)
lift_df

# Positive cases
positive <- sum(lift_df$actual == 1)
positive
#129 positive cases

# Cumulative response rate
cum_response_rate <- cumsum(lift_df$actual == 1) / positive

# Cumulative lift
cum_lift <- cum_response_rate / (1:length(lift_df$actual))

# Plot lift chart
plot(1:length(lift_df$actual), cum_lift, type = "l", xlab = "# cases", ylab = "Cumulative Lift",
     main = "KNN Lift Chart")




### Logistic Regression
train.lr <- train
valid.lr <- valid
train.lr$Gender <- as.factor(train.lr$Gender)
valid.lr$Gender <- factor(valid.lr$Gender, levels = levels(train.lr$Gender))
train.lr$Mcode = as.factor(train.lr$Mcode)
train.lr$Rcode = as.factor(train.lr$Rcode)
train.lr$Fcode = as.factor(train.lr$Fcode)
valid.lr$Mcode = as.factor(valid.lr$Mcode)
valid.lr$Rcode = as.factor(valid.lr$Rcode)
valid.lr$Fcode = as.factor(valid.lr$Fcode)

# Chose M, R, F, ArtBks, ItalArt, and Related.Purchase as predictors
# Model using chosen predictors
lr.best <- glm(Florence ~ M + R + F + ArtBks + ItalArt + Related.Purchase, data = train.lr, family = 'binomial')

# Model using only RFM
lr.rfm <- glm( Florence ~ M + F + R, data = train.lr, family = 'binomial')

## Predictors
library(gains)

# Chosen predictors
pred.lr.best <- predict(lr.best, valid.lr, type = 'response')

# RFM predictors
pred.lr.rfm <- predict(lr.rfm, valid.lr, type = 'response')

gain.best <- gains(valid.lr$Florence, pred.lr.best, groups=100)
gain.rfm <- gains(valid.lr$Florence, pred.lr.rfm, groups=100)

## Lift Chart
# Chosen
plot(c(0,gain.best$cume.pct.of.total*sum(valid.lr$Florence))~
       c(0,gain.best$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="Best Predictor", type="l")
lines(c(0,sum(valid.lr$Florence))~c(0, dim(valid.lr)[1]), lty=2)

# RFM
plot(c(0,gain.rfm$cume.pct.of.total*sum(valid.lr$Florence))~
       c(0,gain.rfm$cume.obs), 
     xlab="# cases", ylab="Cumulative", main="RFM", type="l")
lines(c(0,sum(valid.lr$Florence))~c(0, dim(valid.lr)[1]), lty=2)


# Cutoff of 0.3 for probability
pred.lr.best.factor <- ifelse(pred.lr.best > 0.3, 1, 0)
pred.lr.rfm.factor <- ifelse(pred.lr.rfm > 0.3, 1, 0)

# Data frame of predicted vs actual
best <- data.frame(predicted = pred.lr.best.factor, actual = valid.lr$Florence)
rfm <- data.frame(predicted = pred.lr.rfm.factor, actual = valid.lr$Florence)

# Factors
best.pred <- as.factor(best$predicted)
best.actual <- as.factor(best$actual)

rfm.pred <- factor(rfm$predicted, levels = c('0', '1'))
rfm.actual <- as.factor(rfm$actual)

# Confusion matrices of model results
confusionMatrix(best.pred, best.actual, positive = '1')
confusionMatrix(rfm.pred, rfm.actual, positive = '1')