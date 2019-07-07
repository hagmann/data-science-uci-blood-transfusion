library(caret)
library(tibble)
library(dplyr)
library(ggplot2)
library(magrittr)
library(corrgram)
library(randomForest)

blood_train <- read.csv("data/bloodtrain.csv", stringsAsFactors = FALSE)
blood_test <- read.csv("data/bloodtest.csv", stringsAsFactors = FALSE)

colnames(blood_train) <- c("id","regency","frequency","volume","donationPeriod","didDonate")
colnames(blood_test) <- c("id","regency","frequency","volume","donationPeriod")

blood_train <- subset( blood_train, select = -volume )

head(blood_train)

blood_train$didDonate = as.factor(blood_train$didDonate)
partition_blood <- createDataPartition(blood_train[,1], times = 1, p = 0.75,list = FALSE)
train <- blood_train[partition_blood,] # Create the training sample
test = blood_train[-partition_blood,] # Create the test sample

# kNN
tune.grid=expand.grid(k=c(6))
model=train(didDonate~.,method='knn',data=train,tuneGrid=tune.grid)
model
score=predict(model,newdata=test)
confusionMatrix(score,test$didDonate)

train$didDonate[train$didDonate ==1] <- "yes"
train$didDonate[train$didDonate ==0] <- "no"


# random forest
model.rf = randomForest(didDonate ~ ., 
                        data = train, ntree = 40)
score=predict(model.rf,newdata=test)
confusionMatrix(score,test$didDonate)



# export to file in submission format




knn_cv_results <- data.frame(matrix(ncol = 6, nrow = 20))
knn_cv_results[,1] <- c(1:20)
colnames(knn_cv_results) <- c("k", "iter1", "iter2", "iter3", "iter4", "iter5")

# Perform repeated cross-validation for KNN to tune K
for (i in 1:20){
  for (j in 1:5){
    # Zuf�lligen Index f�r das Auswaheln von Subsamles definieren
    cv_idx <- sample(nrow(train), nrow(train)*split_size, replace = FALSE)
    
    # Split der Daten in Training-Set und Validation-Set, ID-Spalte weglassen
    cv_tr <- train[cv_idx,-1]
    cv_val <- train[-cv_idx,-1]
    
    # K festsetzen
    cv_grid <- expand.grid(k = c(i))
    
    # kNN-Modell trainieren
    knn_cv <- train(as.factor(diddonate) ~ Recency + Frequency + Volume + diddonate,
                    data = cv_tr,
                    method = "knn",
                    tuneGrid = cv_grid)
    
    # Vorhersage machen mit Hilfe es Validierungs-Set
    pred_cv <- predict(knn_cv, cv_val, type = "prob")
    
    # Resultate festhalten -- i-te Zeile, (j+1). Spalte
    knn_cv_results[i,j+1] <- log_loss(cv_val$diddonate, pred_cv$yes)
  }




# bloxplot
ggplot(blood_test, aes(x = "", y = number_of_donations)) +
  geom_boxplot(color = "blue", fill = "red") +
  ylab("Number of Donations") +
  ggtitle("Boxplot of the Number of Donations") 

# barchart
ggplot(blood_test, aes(x=months_since_last_donation,y=months_since_first_donation)) +
  geom_bar(stat="identity", color="white", fill="black")

ggplot(blood_test, aes(x = "", y = months_since_last_donation)) +
  geom_boxplot(color = "orange", fill = "black") +
  ylab("Months since Last Donation") +
  ggtitle("Boxplot of the Months since Last Donation") 


# get correlations
cor(blood_train)
cor(blood_test)

corrgram(blood_train[-1], order=NULL, lower.panel=panel.shade, upper.panel=NULL, text.panel=panel.txt,
         main="Corrgram of the data")

summary(blood_test$months_since_last_donation)

# prcomp
prcomp(blood_train, scale = FALSE)

# scatter blot
pairs(blood_train)


ggplot(data, aes(resp)) +
  geom_bar(stat = "count", aes(fill = resp)) + 
  ggtitle("Distribution of Response variable") + 
  theme(legend.position="none")


