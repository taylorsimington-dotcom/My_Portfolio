## Homework 14
## 12/5/2022 Taylor Simington
## Attempt 1
##

# Setting working directory
setwd("C:\\Users\\Hopei\\OneDrive\\Desktop\\Senior year\\Fall\\Machine Learning\\Homework 14") #to change the working directory
getwd()

# Reading in new data sets
H_Data <- read.csv(file="Housing_Data.csv", stringsAsFactors=FALSE)

# summarize variables
summary(H_Data)

# Z-standardize the five quantitative predictor features.

for (i in c(1:5)) {
  H_Data[,i] <- as.numeric(scale(H_Data[,i]))  
}

# For exploratory data analyses, create scatterplots of the predictors (in 
# their original units) in relation to the outcome price, as well as a 
# correlation heatmap that includes the six quantitative features.

  #scatterplots

  #heatmap
library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

library(ggplot2)
ggplot(melted_cormat, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_classic() + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(axis.text.x = element_text(angle = 90))


# Partition the data set so that 70% of the records are included in the 
# training data set and 30% are included in the test data set. For each 
# data set, identify the total number of records and the typical value of 
# the outcome feature price.

set.seed(1353)
# Below assigning records to the training set by generating random uniform distribution between 0 and 1
# If the random number is less than 0.75 then that observation will be a part of the training set
train_ind <- runif(nrow(PC_wdbc)) < 0.75
table(train_ind)
table(train_ind)/length(train_ind)

PC_wdbc$train_ind <- factor(train_ind)


# Use the lm() function in R and the training data set to develop a simple 
# linear regression model using average age of houses in the area to 
# predict the price that the house sold for in dollars.
    
model.pca <- lm(y ~ PC1 + PC2 + PC3 + PC4)
vif(model.pca)

# Use the olsrr package to conduct stepwise regression to develop a 
# multiple linear regression model using all five predictor features. From 
# the stepwise regression output, select the multiple linear regression 
# model with one of the lower AIC values. There should be at least a 2-
# percentage point change in adjusted-R to include another predictor in 
# the model.

model01 <- lm(formula = y ~ Days.since.Purchase + Purchase.Visits + Days.on.File + Days.between.Purchases +
                Diff.Items.Purchased,data=X_z)

library(car)
vif(model01)

# For PCA, need to install psych package
library(psych)
# Below requesting 5 components
pca01 <- principal(r=X_z, rotate="varimax", nfactors=5)
print(pca01$loadings)
print(pca01$loadings, cutoff=0.49)


# Apply the CART regression method to the training data set to predict 
# the price that houses will sell for in dollars.
    # Obtain a graph of the CART regression tree. 
    # Calculate the MAE for the model on the test data set.

# Apply CART method to the PCA components
library(rpart)
library(rpart.plot)

# Below the CART model is built using rpart() command
cart01 <- rpart(formula = diag_outcome ~ .,data=wdbc_train_PCA, method="class")

# Below plotting the decision tree
rpart.plot(cart01, type=4, extra=102)

# Below predicting income values based on decision tree
predCART <- predict(object=cart01,newdata=wdbc_test_PCA,type="class")

# To use confusionMatrix - both predicted and actual need to be factor features
# library(caret)
confusionMatrix(predCART,wdbc_test_PCA$diag_outcome,positive="M")




