# We will use KNN algorithm to classify traffic signs based on RGB intensity of sign image

# In the Traffic Sign data set: 
#Each previously observed street sign was divided into a 4x4 grid, and the red, green, and blue level for each of the 16 center pixels is recorded 
#The result is a dataset that records the sign_type as well as 16 x 3 = 48 color properties of each sign.

#Load Library Class to use knn()
library("class")

#Import the Data: Traffic Signs
library(readxl)
TrafficSigns <- read_excel("trafficSign-KNN.xlsx")
View(TrafficSigns)
str(TrafficSigns)

# Create Training data frame
train = TrafficSigns[TrafficSigns$sample == "train",]
str(train)
#Removing id and sample columns
train = train[,c(-1,-2)]
str(train)


#Creating Test data Frame
test = TrafficSigns[TrafficSigns$sample == "test",]
test = test[,c(-1,-2)]
str(test)


# Count the number of signs of each type in training and test data set
table(train[,'sign_type'])
table(test[,'sign_type'])

# Check r10's average red level by sign type
#to see whether the average red level might vary by sign type.
aggregate(r10 ~ sign_type, data = train, mean)

#Thus, stop sign tends to have higher average red value. 
#This is how KNN identifies similar signs

#Use KNN to identify test road signs
sign_types = train$sign_type  #Create vector of sign types

#Removing sign_type columns from train and test data set
k_1 = knn(train = train[-1], test = test[-1], cl = sign_types)

#Creating confusion matrix
signs_actual = test$sign_type
table(signs_actual,k_1)

# Compute the accuracy of model
mean(k_1 == signs_actual)

#Using Different Values of K = 7 and K = 15
#for k =7
k_7 = knn(train = train[-1], test = test[-1], cl = sign_types, k =7)
mean(k_7 == signs_actual)

#for k =15
k_15 = knn(train = train[-1], test = test[-1], cl = sign_types, k =15)
mean(k_15 == signs_actual)

#Thus, we can say K = 7 gives better accuracy

# Use the prob parameter to get the proportion of votes for the winning class
#for k =7
k_7 = knn(train = train[-1], test = test[-1], cl = sign_types, k =7, prob = TRUE)
mean(k_7 == signs_actual)

# Get the "prob" attribute from the predicted classes
k7_prob = attr(k_7, "prob")

# Examine the first several predictions
head(k_7)

# Examine the proportion of votes for the winning class
head(k7_prob)
