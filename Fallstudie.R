library(tidyverse)
library(caret)
library(magrittr)
library(corrgram)
library(randomForest)

# read csv
blood_train <- read.csv("data/bloodtrain.csv")
blood_test <- read.csv("data/bloodtest.csv")


colnames(blood_train) <- c("id","density","frequency","volume","donationPeriod","diddonate")
colnames(blood_test) <- c("id","density","frequency","volume","donationPeriod")



# remove volume
#blood_train <- subset( blood_train, select = -volume )


# scale verbessert die accuracy nicht
#train %>%
#  mutate_at(c(2,3,4), funs(c(scale(.))))




# data exploration

# bloxplot
numDonations <- ggplot(blood_train, aes(x = "", y = frequency)) +
  geom_boxplot(color = "blue", fill = "red") +
  ylab("Number of Donations") +
  ggtitle("Number of Donations") 

monthsSinceLastDonation <- ggplot(blood_train, aes(x = "", y = density)) +
  geom_boxplot(color = "orange", fill = "black") +
  ylab("Months since Last Donation") +
  ggtitle("Months since Last Donation") 

volumeDonatedTotal <- ggplot(blood_train, aes(x=volume)) + 
  geom_density() + 
  theme(legend.position="none") + 
  xlab("Total Volume of Blood Donated") + 
  ggtitle("Total Volume of Blood Donated Density") + 
  geom_vline(aes(xintercept=mean(volume)), color="blue", linetype="dashed", size=1)



volumeDonated <- ggplot(blood_train, aes(x = diddonate, y = volume)) +
  geom_boxplot(color = "blue", fill = "red") +
  ylab("Total Volume of Blood Donated") +
  ggtitle("Boxplot of the Total Volume of Blood Donated across the response") 



# get correlations
correlation <- cor(blood_train)

correlation2 <- corrgram(blood_train[-1], order=NULL, lower.panel=panel.shade, upper.panel=NULL, text.panel=panel.txt,
                        main="Korrelationen")

summary(blood_test$months_since_last_donation)

# prcomp
prcomp(blood_train, scale = FALSE)

# scatter blot
scatter = pairs(blood_train)




# Data Splitting
blood_train$diddonate = as.factor(blood_train$diddonate)
partition_blood <- createDataPartition(blood_train[,1], times = 1, p = 0.75,list = FALSE)
train <- blood_train[partition_blood,] # Create the training sample
test = blood_train[-partition_blood,] # Create the test sample

dim(train)
dim(test)







tune.grid=expand.grid(k=c(1:30))


# kNN
model.knn=train(diddonate~.,method='knn',data=train,tuneGrid=tune.grid)
score=predict(model,newdata=test)
confusionMatrix(score,test$diddonate)


# random forest
model.rf = randomForest(diddonate ~ .,data = train, ntree = 40)
score=predict(model.rf,newdata=test)
confusionMatrix(score,test$diddonate)


# kNN mit LogLoss
model.knnLogLoss=train(diddonate~.,method='knn',metric="LogLoss",data=train,tuneGrid=tune.grid)
score=predict(model,newdata=test)
confusionMatrix(score,test$diddonate)




# Vorhersage
set.seed(123)


# log loss - geeignet fuer Caret Package
# -----------------------------------------------------------
# Code von https://www.kaggle.com/c/otto-group-product-classification-challenge/discussion/13064#69102
LogLossSummary <- function (data, lev = NULL, model = NULL) {
  LogLoss <- function(actual, pred, eps = 1e-15) {
    # Check to see that two vectors are the same length
    stopifnot(all(dim(actual) == dim(pred)))
    # Bound probabilities (0,1) for computational purposes
    pred[pred < eps] <- eps
    pred[pred > 1 - eps] <- 1 - eps
    # Compute log loss
    -sum(actual * log(pred)) / nrow(pred)}
  # Convert into factors
  if (is.character(data$obs)) data$obs <- factor(data$obs, levels = lev)
  pred <- data[, "pred"]
  obs <- data[, "obs"]
  isNA <- is.na(pred)
  pred <- pred[!isNA]
  obs <- obs[!isNA]
  data <- data[!isNA, ]
  cls <- levels(obs)
  # Contruct loss function summary output
  if (length(obs) + length(pred) == 0) {out <- rep(NA, 2)} 
  else {
    pred <- factor(pred, levels = levels(obs))
    out <- unlist(e1071::classAgreement(table(obs, pred)))[c("diag", "kappa")]
    probs <- data[, cls]
    actual <- model.matrix(~ obs - 1)
    out2 <- LogLoss(actual = actual, pred = probs)
  }
  out <- c(out, out2)
  names(out) <- c("Accuracy", "Kappa", "LogLoss")
  if (any(is.nan(out))) out[is.nan(out)] <- NA 
  out
}


# create submission file
knn_control <- trainControl(method = "repeatedcv", number = 5, classProbs = TRUE, summaryFunction = LogLossSummary)
knn_pred <- predict(model.rf, blood_test, type = "prob")
knn_submission <- data.frame(blood_test$id, knn_pred$`1`)

colnames(knn_submission) <- c("id", "diddonate") 
write.csv(knn_submission, "knn_submission.csv", row.names = FALSE)  
