install.packages("dplyr")

library(caret)
library(tibble)
library(dplyr)
library(ggplot2)
library(magrittr)


submission_format = read.csv("data/submission_format.csv")
head(submission_format)
submission_format

# Tidy data
blood_test <- as_tibble(read.csv("data/bloodtest.csv", header =T, na.strings=c("","NA"))) %>%
  rename(
    months_since_last_donation = Months.since.Last.Donation,
    number_of_donations = Number.of.Donations,
    total_volume_donated = Total.Volume.Donated..c.c..,
    months_since_first_donation = Months.since.First.Donation
  )

# Tidy data
blood_train <- as_tibble(read.csv("data/bloodtrain.csv", header =T, na.strings=c("","NA"))) %>%
  rename(
    months_since_last_donation = Months.since.Last.Donation,
    number_of_donations = Number.of.Donations,
    total_volume_donated = Total.Volume.Donated..c.c..,
    months_since_first_donation = Months.since.First.Donation
  )

# Remove columns
useless=c()
blood_test = blood_test[,!(names(blood_test) %in% useless)]
blood_train = blood_test[,!(names(blood_test) %in% useless)]

blood_train



ggplot(blood_test, aes(x=months_since_last_donation,y=months_since_first_donation)) +
  geom_bar(stat="identity", color="white", fill="black")


no_model<-glm(donation_made_March_2007~1,data = trainset1, family=binomial)
summary(no_model)



model <- lm(months_since_last_donation ~ months_since_first_donation, data=blood_train)
predict(model,newdata = blood_train)
