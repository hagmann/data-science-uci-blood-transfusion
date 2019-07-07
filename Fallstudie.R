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


head(blood_train)




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
                        main="Corrgram of the data")

summary(blood_test$months_since_last_donation)

# prcomp
prcomp(blood_train, scale = FALSE)

# scatter blot
scatter = pairs(blood_train)




# Models
blood_train$diddonate = as.factor(blood_train$diddonate)
partition_blood <- createDataPartition(blood_train[,1], times = 1, p = 0.75,list = FALSE)
train <- blood_train[partition_blood,] # Create the training sample
test = blood_train[-partition_blood,] # Create the test sample

dim(train)
dim(test)









# kNN
tune.grid=expand.grid(k=c(1:30))
model=train(diddonate~.,method='knn',data=train,tuneGrid=tune.grid)
model
score=predict(model,newdata=test)
confusionMatrix(score,test$diddonate)



# random forest
model.rf = randomForest(diddonate ~ ., 
                        data = train, ntree = 40)
score=predict(model.rf,newdata=test)
confusionMatrix(score,test$diddonate)


