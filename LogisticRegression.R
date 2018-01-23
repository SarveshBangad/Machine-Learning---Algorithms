#Logistic Regression
#Donors data set: 93462 records : People mailed in a fundraising solicitation for paralyzed veterans
#Donated Column: Dependent Variable
#1: Made donation in response to mailing
#0: Not Made Donation

#Load Data Set:
library(readxl)
donors <- read_excel("donor-LogisticRegression.xlsx")
View(donors)

# Examine the dataset to identify potential independent variables
str(donors)

# Explore the dependent variable
table(donors$donated)
# Class of 1 is rare class (4711 records out of 88751)

#-------------------1. Simple Model------------------------
#Build a logistic model based on below 3 parameters:
# bad_address : set to 1 for an invalid mailing address and 0 otherwise, seems like it might reduce the chances of a donation
# interest_religion and interest_veterans : seems would be associated with greater charitable giving

donation_model <- glm(donated ~ bad_address + interest_religion + interest_veterans,data = donors, family = "binomial")

# Summarize the model results
summary(donation_model)


#predict() function to the model object to forecast future behaviour
#predict() outputs predictions in terms of log odds unless type = "response" is specified. 
#This converts the log odds to probabilities.

# Estimate the donation probability
donors$donation_prob <- predict(donation_model, type = "response")

# Find the donation probability of the average prospect
mean(donors$donated)

# Predict a donation if probability of donation is greater than average (0.0504)
donors$donation_pred <- ifelse(donors$donation_prob > 0.0504, 1, 0)

# Calculate the model's accuracy
mean(donors$donation_pred == donors$donated)
# With around 79% accuracy, model seems to be good. But is it true?
# The result is misleading due to rarity of the outcome
# In cases, where class of interest is rare, we might trade off the accuracy

# Thus, accuracy is a misleading measure of model performance on imbalanced datasets
# We can create ROC curve and compute AUC to evalute logistic regression model

#Load the pROC package
install.packages("pROC")
library("pROC")

# Create a ROC curve
ROC <- roc(donors$donated, donors$donation_pred)

# Plot the ROC curve
plot(ROC, col = "blue")
#Based on visualization, model isn't doing much better than baseline

# Calculate the area under the curve (AUC)
auc(ROC)


#--------Handling Missing Data in Age column--------
# Find the average age among non-missing values
donors$age = as.numeric(donors$age)
summary(donors$age)

# Impute missing age values with mean(age)
donors$imputed_age <- ifelse(is.na(donors$age), 61.65, donors$age)

# Create missing value indicator for age
donors$missing_age <- ifelse(is.na(donors$age), 1, 0)

#--Building another model on 3 different terms------------------
# Build a recency, frequency, and money (RFM) model
rfm_model <- glm(donated ~ money + recency * frequency, data = donors, family = 'binomial')

# Summarize the RFM model to see how the parameters were coded
summary(rfm_model)

# Compute predicted probabilities for the RFM model
rfm_prob <- predict(rfm_model, type = 'response')

# Plot the ROC curve and find AUC for the new model
library(pROC)
ROC <- roc(donors$donated,rfm_prob)
plot(ROC, col = "red")
auc(ROC)


#----Stepwise Regression Model-------------------------------
# Specify a null model with no predictors
null_model <- glm(donated ~ 1, data = donors, family = "binomial")

# Specify the full model using all of the potential predictors
full_model <- glm(donated ~ ., data = donors, family = "binomial")

# Use a forward stepwise algorithm to build a parsimonious model
step_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")

# Estimate the stepwise donation probability
step_prob <- predict(step_model, type = 'response')

# Plot the ROC of the stepwise model
library(pROC)
ROC <- roc(donors$donated,step_prob)
plot(ROC, col = "red")
auc(ROC)


