#Packages
library(DescTools)
library(dplyr)
library(ggplot2)
library(moments)
library(rstatix)
library(Hmisc)
library(purrr)
library(rpart)
library(rpart.plot)
library(caret)
library(naivebayes)
library(randomForest)
library(GGally)
library(gridExtra)
library(reshape2)

#importing dataset
L <- LoanDefaulters
#changing categorical variables to factors
L[,c(2:8, 13,15,17,18,19)] <- lapply(L[,c(2:8, 13,15,17,18,19)], factor)
L <- as.data.frame(L)

#check for missing values
map(L, ~sum(is.na(.)))

#imputing missing values
L[3] <- impute(L[3], fun = Mode)
L[10] <- impute(L[10], fun = median)
L[11] <- impute(L[11], fun = median)
L[12] <- impute(L[12], fun = Median)
L[15] <- impute(L[15], fun = Mode)
L[16] <- impute(L[16], fun = median)

#checking normality and outliers
skewness(L[c(9,10,11,12,14, 16)])
kurtosis(L[c(9,10,11,12,14, 16)])
boxplot(L[c(9,10,11,12,14, 16)])

##removing outliers in income
outinc <- boxplot(L$income)$out
L<- L[-which(L$income %in% outinc), ]

#check
skewness(L$income)
kurtosis(L$income)
length(boxplot(L$income)$out)

#treating outliers in other col

colnames(L[c(9,10,11,14, 16)])
columns_to_process <- c("loan_amount","rate_of_interest","property_value","Credit_Score","LTV")

# Looping through each column in the list and applying the quantile-based capping
for (col in columns_to_process) {
  x <- L[[col]]
  
  # Calculating quartiles and caps
  qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  caps <- quantile(x, probs = c(0.05, 0.95), na.rm = TRUE)
  
  # Calculating the tolerance for outliers based on IQR
  H <- 1.5 * IQR(x, na.rm = TRUE)
  
  # Cap outliers
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  
  # Assigning the modified data back to the data frame
  L[[col]] <- x
}

# Check the modified data frame
str(L)
skewness(L[c(9,10,11,12,14, 16)])
kurtosis(L[c(9,10,11,12,14, 16)])
boxplot(L[c(9,10,11,12,14, 16)])

##EDA
summary(L) #for var info

L <- L[-18] #dropping security type because only one factor level

#boxplot
x <- L[c(9,10,11,12,14, 16)]
par(mfrow=c(1,6))
for(i in 1:6) {
  boxplot(x[,i], main=names(x)[i])
}

#pairplot
ggpairs(x)

#heatmap
x <- L[c(9,10,11,12,14, 16)]
data <- cor(x)

data1 <- melt(data) #reshaping data

ggplot(data1, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  labs(title = "Correlation Heatmap",
       x = "X",
       y = "Y")

#Barplot
p1 <- ggplot(data = L) +geom_bar(mapping = aes(x = loan_type,fill=Status))
p2 <-ggplot(data = L) +geom_bar(mapping = aes(x = loan_purpose,fill=Status))
p3 <-ggplot(data = L) +geom_bar(mapping = aes(x = credit_type,fill=Status)) 
p4 <-ggplot(data = L) +geom_bar(mapping = aes(x = Credit_Worthiness,fill=Status)) 
grid.arrange(p1,p2,p3,p4 ,ncol= 2)

##Model Training

#Model 1 - Naive Bayes
set.seed(100)

intrain <- createDataPartition(y = L$Status, p = 0.8, list = FALSE)

#subset P to obtain training set
training <- L[intrain, ]
#subset the rest to obtain test dataset
testing <- L[-intrain, ]

#fit linear model
Model1 <- train(Status~.,data = training,  method = "naive_bayes")
summary(Model1)

predStatus = predict(Model1,newdata = testing)

#accuracy check
confusionMatrix(predStatus,testing$Status, positive = "1")

#Model 2 - Logistic regression

Model2 <- train(data = training, Status~.,  method = "glm", family="binomial")
summary(Model2)

predStatus1= predict(Model2,newdata = testing)
#accuracy check
confusionMatrix(predStatus1,testing$Status,positive="1")


#Model 3&4 - Decision Tree
#using gini index
Model3 <- train(data = training, Status~.,  method = "rpart")

#using information gain
Model4 <- train(data = training, Status~.,  method = "rpart", parms = list(split = "information"))

#plotting decision tree
par(mfrow=c(1,2))
rpart.plot(Model3$finalModel)
rpart.plot(Model4$finalModel)


predStatus3= predict(Model3,newdata = testing)
predStatus4= predict(Model4,newdata = testing)

#accuracy check
confusionMatrix(predStatus3,testing$Status,positive="1")
confusionMatrix(predStatus4,testing$Status, positive="1")

#Model 5 - Random Forest
Model5 <- train(data = training, Status~.,  method = "rf")
Model5

Model5$finalModel 
varImp(Model5)
plot(Model5)

predStatus5= predict(Model5,newdata = testing)
#accuracy check
confusionMatrix(predStatus5,testing$Status, positive="1")
