# Classification Analysis: Logistic Regression, kNN and Decision Trees
# Cardiovascular Disease Dataset(https://www.kaggle.com/datasets/sulianova/cardiovascular-disease-dataset)
library(readr)
library(tidyverse)
library(ggplot2)
library(DescTools)
library(caret)
library(NeuralNetTools)

cdata <- read.csv("cardio.csv", sep=';')
# data is separated by ; not columns so we convert it do a data frame
View(cdata)
str(cdata)
Abstract(cdata)
# no missing values
glimpse(cdata)

# dropping id column
cdata <- cdata %>% select(-id)

# Predictor Variables
# all of our predictor variables are numerical (including the ordinal ones), which is what we want them to be
# gender is binary but expressed as 1 (woman) and 2 (man) instead of 0 and 1
# let's convert to binary
cdata$gender <- ifelse(cdata$gender == 2, 0, 1)

# patient age is given in days, so we will convert it to age with 1 decimal place
cdata$age <- round(cdata$age / 365.0, 1)
cdata$age <- as.numeric(cdata$age)

# instead of using both height and weight, we will use BMI
cdata$height<-cdata$height/100 
cdata$bmi<-round(cdata$weight/cdata$height^2,2)
# removing height and weight
cdata <- subset(cdata, select=-c(height, weight))
head(cdata)

# Exploring alcohol intake
alcohol <- cdata %>%
  group_by(alco) %>%
  summarise(count=n())
alcohol
# smoking
smoking <- cdata %>%
  group_by(smoke) %>%
  summarise(count=n())
smoking
# exercise
exercise <- cdata %>%
  group_by(active) %>%
  summarise(count=n())
exercise
# cholesterol
chol <- cdata %>%
  group_by(cholesterol) %>%
  summarise(count=n())
chol
# glucose
glucose <- cdata %>%
  group_by(gluc) %>%
  summarise(count=n())
glucose

# Distributions & Removing outliers
# Age
boxplot(cdata$age, main = "Box Plot of Age", ylab = "Age", col = "lightblue", border = "blue")
Q1a <- quantile(cdata$age, 0.25)
Q3a <- quantile(cdata$age, 0.75)
IQRa <- Q3a - Q1a
cdata <- cdata[cdata$age >= Q1a - 1.5 * IQRa & cdata$age <= Q3a + 1.5 * IQRa, ]
# ap_hi
boxplot(cdata$ap_hi, main = "Box Plot of Systolic blood pressure", ylab = "ap_hi", col = "lightblue", border = "blue")
Q1h <- quantile(cdata$ap_hi, 0.25)
Q3h <- quantile(cdata$ap_hi, 0.75)
IQRh <- Q3h - Q1h
cdata <- cdata[cdata$ap_hi >= Q1h - 1.5 * IQRh & cdata$ap_hi <= Q3h + 1.5 * IQRh, ]
# ap_lo
boxplot(cdata$ap_lo, main = "Box Plot of Diastolic blood pressure", ylab = "ap_lo", col = "lightblue", border = "blue")
Q1l <- quantile(cdata$ap_lo, 0.25)
Q3l <- quantile(cdata$ap_lo, 0.75)
IQRl <- Q3l - Q1l
cdata <- cdata[cdata$ap_lo >= Q1l - 1.5 * IQRl & cdata$ap_lo <= Q3l + 1.5 * IQRl, ]
# BMI
boxplot(cdata$bmi, main = "Box Plot of BMI", ylab = "bmi", col = "lightblue", border = "blue")
Q1b <- quantile(cdata$bmi, 0.25)
Q3b <- quantile(cdata$bmi, 0.75)
IQRb <- Q3b - Q1b
cdata <- cdata[cdata$bmi >= Q1b - 1.5 * IQRb  & cdata$bmi <= Q3b + 1.5 * IQRb, ]

dim(cdata)
# Now the dataset has 62,642 instances of data

# Target Variable: Cardio
# cardio is binary: 1 indiates presence of cardiovascular disease and 0 indicates absence
# no need to convert to factor
# distribution:
cdis <- cdata %>%
  group_by(cardio) %>%
  summarise(count=n())
cdis

ggplot(cdis, aes(x = factor(cardio, labels = c("No", "Yes")), y = count, fill = cardio, label = count)) +
  geom_bar(stat = 'identity', fill = c("No" = "lightcoral", "Yes" = "darkred")) +
  geom_text(aes(label = count), position = position_stack(vjust = 0.5), size = 4, color = "white") +
  labs(title = "Presence of Cardiovascular Disease", x = "Cardiovascular Disease", y = "Count") +
  theme(plot.title = element_text(hjust = 0.5))
# the two classes are perfectly balanced

# Correlation
library(corrplot)
cor_matrix <- cor(cdata)
cor_matrix
corrplot(cor_matrix, method = "color")
# The variables most correlated with cardiovascular disease are high systolic and diastolic blood pressure
# There is high correlation between diastolic and systolic blood pressure
# According to this study published in the NIH, systolic blood pressure is a better indicator of risk
# https://pubmed.ncbi.nlm.nih.gov/12698068/#:~:text=In%20this%20review%20we%20compare,a%20better%20predictor%20of%20risk.
# So we will remove diastolic bloor pressure from the dataset.
cd <- subset(cdata, select=-c(ap_lo))
# The rest of the variables do not exhibit extremely high correlation that could interfere with our models

# Training & Testing Sets
set.seed(444)
part <- createDataPartition(y = cd$cardio, p = 0.80, list = FALSE)
train <- cd[part, ]
test <- cd[-part, ]


# METHOD 1: Logistic Regression
# Fit a logistic regression model
logmod <- glm(cardio ~ ., data = train, family = binomial)
summary(logmod)

# binding the intercept and coefficients into a dataframe
intercept <- summary(logmod)$coefficients[1, "Estimate"]
coefficients <- summary(logmod)$coefficients[-1, "Estimate"]
coef_df <- data.frame(Variable = names(coefficients), Coefficient = coefficients)
coef_df <- rbind(data.frame(Variable = "(Intercept)", Coefficient = intercept), coef_df)
coef_df

# Make predictions on the test dataset
predicted_probs <- predict(logmod, newdata = test, type = "response")
predicted_classes <- ifelse(predicted_probs >= 0.5, 1, 0)

# Create a confusion matrix
logconf <- confusionMatrix(as.factor(test$cardio), as.factor(predicted_classes))
logconf

# ROC curve and AUC
# ROC shows the performance of a classification model at all classification thresholds
library(pROC)
roc_obj <- roc(test$cardio, predicted_probs)

# Print the ROC curve
plot(roc_obj, main = "ROC Curve")
abline(a = 0, b = 1, lty = 2)  # Add a diagonal reference line

# Calculate and print the AUC
auc_value <- auc(roc_obj)
cat("AUC:", auc_value, "\n") # 0.7837477
# the your model has some discriminatory power but it's not perfect


# METHOD 2: k-Nearest Neighbors
library(class)

# Identifying a ‘best guess’ value of k (the square root of the number of training observations)
ceiling(sqrt(nrow(train)))
# 224, so we can choose either 223 or 225
knn.pred1 <- knn(train = train[ ,-10], # we remove the target variable
                test = test[ ,-10], 
                cl = train$cardio, 
                k = 223)

knn_conf1 <- confusionMatrix(as.factor(knn.pred1), as.factor(test$cardio), positive = "1")
knn_conf1

# using 225
knn.pred2 <- knn(train = train[ ,-10], # we remove the target variable
                test = test[ ,-10], 
                cl = train$cardio, 
                k = 225)

knn_conf2 <- confusionMatrix(as.factor(knn.pred2), as.factor(test$cardio), positive = "1")
knn_conf2

# Accuracy = 90.96% in both cases


# METHOD 3: Decision Trees
# Decision trees are insensitive to multicollinearity so we will use the dataset with ap_lo
library(rpart)
library(rpart.plot)

set.seed(444)
part <- createDataPartition(y = cdata$cardio, p = 0.80, list = FALSE)
ttrain <- cdata[part, ]
ttest <- cdata[-part, ]

set.seed(831)
c.rpart <- rpart(formula = cardio ~ ., data = ttrain, method = "class")
c.rpart

c.rpart$variable.importance
##      ap_hi       ap_lo cholesterol         bmi         age        gluc
## 4461.53487  2595.40890   486.70884   404.88468   164.09423    80.48644 

# visualizing the rpart object
prp(x = c.rpart, extra = 2)
# the tree stops after splitting at the root node

preds <- as.factor(predict(c.rpart, newdata = ttest, type='class'))
testf <- as.factor(ttest$cardio)

c.conf <- confusionMatrix(preds, testf)
c.conf #~71% accuracy

