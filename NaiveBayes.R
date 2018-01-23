#Implementation of Naive Bayes Algorithm
#The Locations dataset records Sarvesh location every hour for 13 weeks. 
#Each hour, the tracking information includes the daytype (weekend or weekday) 
#as well as the hourtype(morning, afternoon, evening, or night)

#Using Naive Bayes, build a more sophisticated model to see 
#how Sarvesh's predicted location not only varies by the day of week but also by the time of day.

#Load the data set
library(readxl)
Locations <- read_excel("location-NaiveBayes.xlsx")
View(Locations)
str(Locations)

#Load naivebayes package
install.packages("naivebayes")
library("naivebayes")

#Build a Naive Bayes model of location with daytype and hourtype as predictors
locmodel = naive_bayes(location ~ daytype + hourtype, data = Locations)
print(locmodel)


#Create dummy data frame for predicting location on weekday afternoon
weekday_afternoon = data.frame(daytype = "weekday", hourtype = "afternoon")

predict(locmodel, weekday_afternoon)

