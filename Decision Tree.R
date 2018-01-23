#Decision Tree Implementation
#This Sample Code contains:
#1. Code to build Simple Decision Tree using Train and Test Data Sets
#2. Visualizing Classification Tree
#3. Pruning Tree based on maxdepth, minsplit and complexity parameter(cp)

#The loans dataset contains randomly-selected people who were applied for 
#and later received loans from Lending Club, a US-based peer-to-peer lending company

#Load the Data Set
loans <- read_excel("loans-DecisionTree.xlsx")
str(loans)

#pre-processing the data
loans2 = loans[c(-1,-2)]
str(loans2)

loans2$loan_amount = as.factor(loans2$loan_amount)
loans2$emp_length = as.factor(loans2$emp_length)
loans2$home_ownership = as.factor(loans2$home_ownership)
loans2$income = as.factor(loans2$income)
loans2$loan_purpose = as.factor(loans2$loan_purpose)
loans2$debt_to_income = as.factor(loans2$debt_to_income)
loans2$credit_score = as.factor(loans2$credit_score)
loans2$recent_inquiry = as.factor(loans2$recent_inquiry)
loans2$delinquent = as.factor(loans2$delinquent) 
loans2$credit_accounts = as.factor(loans2$credit_accounts) 
loans2$bad_public_record = as.factor(loans2$bad_public_record) 
loans2$credit_utilization = as.factor(loans2$credit_utilization)
loans2$past_bankrupt = as.factor(loans2$past_bankrupt)

loans2$default = as.factor(loans2$default)

#------------Creating Test and Training data set -------------------
nrow(loans2)

# Create a random sample of row IDs
sample_rows <- sample(nrow(loans2), nrow(loans2) * 0.75)

# Create the training dataset
loans_train <- loans[sample_rows, ]

# Create the test dataset
loans_test <- loans[-sample_rows, ]


#--------Building Large Tree--------------------
install.packages("rpart")
library("rpart")
# Grow a tree using all of the available applicant data
loan_model <- rpart(default ~ ., data = loans_train, method = "class", control = rpart.control(cp = 0))

# Make predictions on the test dataset
loans_test$pred <- predict(loan_model,loans_test,type = "class")

# Examine the confusion matrix
table(loans_test$default, loans_test$pred)

# Compute the accuracy on the test dataset
mean(loans_test$default == loans_test$pred)

#-------Visualizing Classfication Tree---------------
# Load the rpart.plot package
library("rpart.plot")

# Plot the loan_model with default settings
rpart.plot(loan_model)

# Plot the loan_model with customized settings
rpart.plot(loan_model, type = 3, box.palette = c("red", "green"), fallen.leaves = TRUE)

#--------Pruning Tree --------------------
#As we can see Tree has grown too much which hampers derving prediction rules
#So time to Prune back Tree

#Pre-Pruning Trees, using maxdepth and min split options

# Grow a tree with maxdepth of 6
loan_model2 <- rpart(default ~ ., data =  loans_train, method = "class", control = rpart.control(maxdepth = 6, cp = 0))

# Compute the accuracy of the simpler tree
loans_test$pred2 <- predict(loan_model2, loans_test, type = "class")
mean(loans_test$default == loans_test$pred2)

# Grow a tree with minsplit of 500
loan_model3 <- rpart(default ~ ., data =  loans_train, method = "class", control = rpart.control(minsplit = 500, cp = 0))


# Compute the accuracy of the simpler tree
loans_test$pred3 <- predict(loan_model3, loans_test, type = "class")
mean(loans_test$default == loans_test$pred3)

#Thus, Creating a Simple Decision Tree results in greater performance on test data set

#--Post-Pruning Tree : using complexity Parameter:

# Grow an overly complex tree
loan_model4 <- rpart(default ~ ., data =  loans_train, method = "class", control = rpart.control(cp = 0))

# Examine the complexity plot
plotcp(loan_model4)

# Prune the tree based on cp = 0.011 since after that error rate increases as shown in above plot
loan_model_pruned <- prune(loan_model4, cp = 0.011)

# Compute the accuracy of the pruned tree
loans_test$pred4 <- predict(loan_model_pruned, loans_test, type = "class")
mean(loans_test$default == loans_test$pred4)



