## Mental Health in Tech
## 11/7/22 Taylor Simington
## Attempt 2
## An analysis of data concerning the mental health of employees in
## Tech related occupations.
##

#Libraries

library(ggplot2)
library(gmodels)
library(C50)
library(rpart)
library(rpart.plot)
library(caret)
library(class)
library(gridExtra)
library(e1071)
library(reshape2)
library(pROC)
library(randomForest)
library(nnet)
library(NeuralNetTools)
library(reshape2)
library(car)
library(psych)

setwd("C:\\Users\\Hopei\\OneDrive\\Desktop\\Senior year\\Fall\\Machine Learning\\Term Project") #to change the working directory
getwd()

MHealth <- read.csv(file="Mental Health in Tech.csv", stringsAsFactors=FALSE)

# Drop work_interfere due to missing values.
summary(MHealth)
MHealth <- MHealth[,-c(6)]

#normalization
mm_normalize <- function(x, ...) {
  return((x - min(x, ...)) /(max(x, ...) - min(x, ...)))
}

MHealth$Age <- mm_normalize(MHealth$Age)

#factor for loop
for (i in c(2:15)) {
  MHealth[,i] <- factor(MHealth[,i])
}  

summary(MHealth)

#The exploratory data analysis needs to be updated.

  # Create variables that represent only tech companies or non-tech companies
MH_TY <- MHealth
MH_TY <- MH_TY[MH_TY$tech_company=="Yes",]
MH_TN <- MHealth
MH_TN <- MH_TN[MH_TN$tech_company=="No",]

summary(MH_TY$treatment)
summary(MH_TN$treatment)
  #treatment
pct1 <- table(MH_TY$treatment)/nrow(MH_TY)
pct1
barplot(pct1, main="Likelihood of seeking treatment by company type - tech", xlab="Treatment", ylab="Percent",horiz=FALSE)

pct2 <- table(MH_TN$treatment)/nrow(MH_TN)
pct2
barplot(pct2, main="Likelihood of seeking treatment by company type - non-tech", xlab="Treatment", ylab="Percent",horiz=FALSE)
  #gender
pct1 <- table(MH_TY$Gender)/nrow(MH_TY)
pct1
barplot(pct1, main="Likelihood of seeking treatment by gender - tech", xlab="Gender", ylab="Percent",horiz=FALSE)

pct2 <- table(MH_TN$Gender)/nrow(MH_TN)
pct2
barplot(pct2, main="Likelihood of seeking treatment by gender - non-tech", xlab="Gender", ylab="Percent",horiz=FALSE)
  #family history
pct1 <- table(MH_TY$family_history)/nrow(MH_TY)
pct1
barplot(pct1, main="Likelihood of seeking treatment by Family H.- tech", xlab="F. History", ylab="Percent",horiz=FALSE)

pct2 <- table(MH_TN$family_history)/nrow(MH_TN)
pct2
barplot(pct2, main="Likelihood of seeking treatment by Family H. - non-tech", xlab="F. History", ylab="Percent",horiz=FALSE)


#create test and train
set.seed(762)
MHealth_ind <- runif(nrow(MHealth)) < 0.75
table(MHealth_ind)
MH_train <- MHealth[MHealth_ind, ]
MH_test <- MHealth[!MHealth_ind, ]

summary(MH_train)
summary(MH_test)

#bar graph to confirm proportions.
ggplot(MHealth,aes(x = MHealth_ind)) + 
  geom_bar(aes(y = (..count..)/sum(..count..)),stat = "count",fill="blue",col="black") +
  geom_text(aes(label = round(..count../sum(..count..), digits = 3), # text of the label 
                y = ((..count..)/sum(..count..))), # location of the label
            stat = "count", 
            size = 5, # size of the label
            col="white", vjust = 1) +
  ylim(0,1) +
  labs(x="Belong to Training Data Set?",y="Proportion",title="Bar Chart of Training/Test Split") +
  theme_classic() 
table(MH_train$treatment)

summary(MHealth)

# Need to check that the outcome split is similar between training and test

# CART and C5.0 Decision tree (lab 7)
#C5.0
#treatment
treatment_C5 <- C5.0(formula = treatment ~ Age + Gender + Country + family_history + company_size + tech_company + mh_benefits + seek_help + anonymity + leave + mh_consequence + phys_consequence + mental_vs_physical + obs_consequence, data = MH_train, 
                     control=C5.0Control(minCases=30))

plot(treatment_C5)
summary(treatment_C5)
table(MH_test$treatment)

predC5_test <- predict(object=treatment_C5,newdata=MH_test)
confusionMatrix(predC5_test,MH_test$treatment,positive="Yes")

#CART
#treatment
treatment_tr <- rpart(formula = treatment ~ Age + Gender + Country + family_history + company_size + tech_company + mh_benefits + seek_help + anonymity + leave + mh_consequence + phys_consequence + mental_vs_physical + obs_consequence, data = MH_train, method = "class")
treatment_tr  
rpart.plot(treatment_tr, type = 4, extra = "auto")

predCART_tst <- predict(object=treatment_tr,newdata=MH_test,type="class")
table(predCART_tst)
table(predCART_tst,MH_test$treatment)

confusionMatrix(predCART_tst,MH_test$treatment,positive="Yes")



# KNN (lab 8) Note: Only Quant. data
table(MH_train$company_size)
# turning company_size into a quantitative feature for KNN analysis
MH_train_KNN <- MH_train[,c(1,6)]
MH_train_KNN$company_size_num <- NA
MH_train_KNN$company_size_num[MH_train_KNN$company_size=="100-500"] <- 300
MH_train_KNN$company_size_num[MH_train_KNN$company_size=="44737"] <- 15.5
MH_train_KNN$company_size_num[MH_train_KNN$company_size=="26-100"] <- 63
MH_train_KNN$company_size_num[MH_train_KNN$company_size=="44566"] <- 3
MH_train_KNN$company_size_num[MH_train_KNN$company_size=="500-1000"] <- 750
MH_train_KNN$company_size_num[MH_train_KNN$company_size=="More than 1000"] <- 1500
summary(MH_train_KNN$company_size_num)
MH_train_KNN$company_size_num <- mm_normalize(MH_train_KNN$company_size_num)
summary(MH_train_KNN$company_size_num)

MH_test_KNN <- MH_test[,c(1,6)]
MH_test_KNN$company_size_num <- NA
MH_test_KNN$company_size_num[MH_test_KNN$company_size=="100-500"] <- 300
MH_test_KNN$company_size_num[MH_test_KNN$company_size=="44737"] <- 15.5
MH_test_KNN$company_size_num[MH_test_KNN$company_size=="26-100"] <- 63
MH_test_KNN$company_size_num[MH_test_KNN$company_size=="44566"] <- 3
MH_test_KNN$company_size_num[MH_test_KNN$company_size=="500-1000"] <- 750
MH_test_KNN$company_size_num[MH_test_KNN$company_size=="More than 1000"] <- 1500
summary(MH_test_KNN$company_size_num)
MH_test_KNN$company_size_num <- mm_normalize(MH_test_KNN$company_size_num)
summary(MH_test_KNN$company_size_num)

table(MH_train$company_size)
table(MH_train$company_size_num)

# below is to extract the outcome feature
MH_train_KNN_outcome <- factor(MH_train[,c(5)])
MH_test_KNN_outcome <- factor(MH_test[,c(5)])

# below removing factor feature of company size and only keeping numeric feature
MH_train_KNN <- MH_train_KNN[,-c(2)]
MH_test_KNN <- MH_test_KNN[,-c(2)]

# Apply the k-NN algorithm using k=21 and k=10 to predict the diagnostic 
# outcome of the records in the test data set.
pr <- knn(train=MH_train_KNN,test=MH_test_KNN,cl=MH_train_KNN_outcome,k=30)
table(pr)
p1 <- table(pr)/nrow(MH_test_KNN)
round(p1,4)
table(pr,MH_test_KNN_outcome)

# Create the confusion matrix
tb <- table(pr,MH_test_KNN_outcome)

# Create a function called accuracy to compute the accuracy rate.
accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tb)
pr2 <- knn(train=MH_train_KNN,test=MH_test_KNN,cl=MH_train_KNN_outcome,k=15)
table(pr2,MH_test_KNN_outcome)

tb2 <- table(pr2,MH_test_KNN_outcome)
accuracy(tb2)
confusionMatrix(pr2,MH_test_KNN_outcome)




# Naïve Bayes (lab 9) Only Cat. data 

nb01 <- naiveBayes(formula = treatment ~ Age + Gender + Country + family_history + company_size + tech_company + mh_benefits + seek_help + anonymity + leave + mh_consequence + phys_consequence + mental_vs_physical + obs_consequence, data = MH_train)
nb01

ypred_tr_nb <- predict(object=nb01, newdata=MH_test,type = "class")

confusionMatrix(ypred_tr_nb,MH_test$treatment,positive="Yes")



# Logistic regression (lab 10) 
#train
logreg01 <- glm(treatment ~ Age + Gender + Country + family_history + company_size + tech_company + mh_benefits + seek_help + anonymity + leave + mh_consequence + phys_consequence + mental_vs_physical + obs_consequence, data = MH_train, family=binomial)
summary(logreg01)
exp(coef(logreg01))

# Provide the confusion matrix based on using a 0.5 threshold 
yprob_test_lr <- predict(object=logreg01, newdata=MH_test, type="response")
summary(yprob_test_lr)
ypred_test_lr <- factor(ifelse(yprob_test_lr >= 0.50,"Yes","No"))
table(ypred_test_lr)

table(ypred_test_lr,MH_test$treatment)

confusionMatrix(ypred_test_lr,MH_test$treatment,positive="Yes")

# Add code to use the roc() function from the pROC package to obtain 
# the receiver operating characteristic (ROC) curve and the area under 
# the curve (AUC) for the test data set.
test_roc <- roc(MH_test$treatment ~ yprob_test_lr, plot = TRUE, print.auc = TRUE)
as.numeric(test_roc$auc)
test_roc$thresholds



# Random forests (lab 12)

# Setting seed for reproducibility purposes.
summary(MH_train)

set.seed(981)
rf01_MH <- randomForest(formula = treatment ~ .,data=MH_train, 
                        ntree=500, mtry=5, importance=TRUE, type="Classification")

# print out of overall error rates and confusion matrix for random forest on training data set.
rf01_MH

# Plot the error info as a function of the tree number
(plot(rf01_MH))

varImpPlot(rf01_MH)

# Make predictions on the test data set
pred_test_RF <- predict(rf01_MH,newdata=MH_test)

confusionMatrix(pred_test_RF,MH_test$treatment,positive="Yes")

# Neural network (lab 13)

# Run neural network algorithm
set.seed(4475)
nnet01 <- nnet(treatment ~ Age + Gender + family_history + company_size + tech_company + mh_benefits + seek_help + anonymity + leave + mh_consequence + phys_consequence + mental_vs_physical + obs_consequence, data=MH_train, size=1)

# Plot the neural network
# Note this does not show the weights in the figure
plotnet(nnet01,pad_x = .3)

# Obtain the weights and number of nodes per layer
nnet01$wts
nnet01$n

# To obtain more information on variable importance
# Using the garson() function available in the NeuralNetTools package.
#Select most important features and plot!!!
garson(nnet01)

# Next check the accuracy rate on the test data set
ypred01_test_nn <- factor(predict(object=nnet01, newdata=MH_test,type="class"))

# Obtain the confusion matrix and performance measures
confusionMatrix(ypred01_test_nn,MH_test$tech_company,positive="Yes")

#second plot where you can see the variables
nnet02 <- nnet(treatment ~ Age + family_history + company_size + anonymity + obs_consequence, data=MH_train, size=1)
plotnet(nnet02,pad_x = .25)

nnet02$wts
nnet02$n
garson(nnet02)
ypred02_test_nn <- factor(predict(object=nnet02, newdata=MH_test,type="class"))
confusionMatrix(ypred02_test_nn,MH_test$tech_company,positive="Yes")




# Final method overview (lab 13?)
# Below is code to create a bar graph that compares the performance measures of
# accuracy, sensitivity, and specificity to the machine learning techniques applied.

#(Accuracy,Sensitivity,Specificity)
NNmetric <- c(48.38, 49.80, 42.86)
NNmetric2 <- c(41.23, 38.37, 52.38)
CARTmetric <- c(65.25, 63.92, 66.67)
C50metric <- c(65.26, 65.83, 64.67)
NBmetric <- c(71.75, 65.19, 78.67)
LRmetric <- c(68.18, 62.66, 74.00)
KNNmetric <- c(57.79, 64, 51.9)
RFmetric <- c(66.88, 67.09, 66.67)
MLbargraph <- as.matrix(cbind(NNmetric, NNmetric2,CARTmetric,C50metric,NBmetric,LRmetric,KNNmetric,RFmetric))

bp <- barplot(MLbargraph, beside = TRUE, main = "Performance Metrics by ML Technique", 
              col = c("lightblue", "mistyrose", "lavender"),
              xlab = "ML technique", names = c("NN","NN2","CART","C5.0","NB","LR","KNN","RF"), 
              ylab = "Percent", legend.text = c("Accuracy", "Sensitivity", "Specificity"), 
              args.legend = list(x = "topright", horiz=TRUE, cex = 1), ylim = c(0, 120))
text(bp, 0, round(MLbargraph,1),cex=.7,pos=3) 














