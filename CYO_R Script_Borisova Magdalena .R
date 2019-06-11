##################################################################
## "Seattle Police Department 911 Incidence Response" dataset
##################################################################

# Load packages 

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(Hmisc)) install.packages("Hmisc", repos = "http://cran.us.r-project.org")

# Read mini ".csv" file (years 2016-2017 only)

seattle_pd_911_mini <- read.csv("/Users/maggie/Documents/code/cyoproject/Seattle PD/Seattle_Police Department_911_Incident Response_2017.csv", 
                                header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "")


##################################################################
## Descriptive statistics
##################################################################

# Numbers

# Check dimensions of seattle_pd_911_mini dataset

dim(seattle_pd_911_mini)

# [1] 2135     19

# Check structure of seattle_pd_911_mini dataset

str(seattle_pd_911_mini)

# Check for distinct values and missing values

describe(seattle_pd_911_mini)

# At.Scene.Time 
# n       missing distinct 
# 2135        0   1763 

# Missing values in dataset are represented as Blanks in "At.Scene.Time" column 

# Create new dataset with no "At.Scene.Time" Blanks 

seattle_pd_911 <- seattle_pd_911_mini[!(seattle_pd_911_mini$At.Scene.Time == ""),]

# Check if "Blanks" are removed 

seattle_pd_911$At.Scene.Time == ""

# Check dimensions of new dataset without Blanks

dim(seattle_pd_911)

# [1] 1773   19

# Check structure of new seattle_pd_911 dataset

str(seattle_pd_911)

# Check first few observations 

seattle_pd_911 %>% as_tibble()

head(seattle_pd_911)

# Review dataset summary

summary(seattle_pd_911)


# Graphs

# Plot incident location (longitude, latitude) in Seattle 

longitude <- seattle_pd_911$Longitude
latitude <- seattle_pd_911$Latitude

length(latitude)
length(longitude)

plot(longitude, latitude, pch = 10, cex = 0.01)


# Split "At.Scene.Time" column to hours, dates, months and weekdays

seattle_pd_911$At.Scene.Hour.Min <- format(as.POSIXct(strptime(seattle_pd_911$At.Scene.Time,
                                                               "%m/%d/%y %H:%M",tz="")), format = "%H:%M")

seattle_pd_911$At.Scene.Hour <- format(as.POSIXct(strptime(seattle_pd_911$At.Scene.Time,
                                                           "%m/%d/%y %H:%M",tz="")), format = "%H")

seattle_pd_911$At.Scene.Min <- format(as.POSIXct(strptime(seattle_pd_911$At.Scene.Time,
                                                           "%m/%d/%y %H:%M",tz="")), format = "%M")

seattle_pd_911$At.Scene.Date <- format(as.POSIXct(strptime(seattle_pd_911$At.Scene.Time,
                                                           "%m/%d/%y %H:%M",tz="")), format = "%m/%d/%Y")

seattle_pd_911$At.Scene.Month <- format(as.POSIXct(strptime(seattle_pd_911$At.Scene.Time,
                                                            "%m/%d/%y %H:%M",tz="")), format = "%m")

seattle_pd_911$At.Scene.Weekday <- format(as.POSIXct(strptime(seattle_pd_911$At.Scene.Time,
                                                              "%m/%d/%y %H:%M",tz="")), format = "%u")


# Plot hours, dates, months, weekdays, districts, zones, incident groups, census tracts

barplot(table(seattle_pd_911$At.Scene.Weekday), main = "Seattle PD at Scene by Weekday", 
        xlab = "1 = Mon  2 = Tues  3 = Wed  4 = Thurs  5 = Fri  6 = Sat  7 = Sun")

barplot(table(seattle_pd_911$At.Scene.Month), main = "Seattle PD at Scene by Month", 
        xlab = "1 = Jan 2 = Feb 3 = Mar 4 = Apr 5 = May 6 = Jun 7 = Jul 8 = Aug")

barplot(table(seattle_pd_911$At.Scene.Hour), main = "Seattle PD at Scene by Hour", xlab = "24 hour clock")

barplot(table(seattle_pd_911$District.Sector), main = "Seattle Districts")

barplot(table(seattle_pd_911$Initial.Type.Group), main = "Incident Group")

barplot(table(seattle_pd_911$Zone.Beat), main = "Seattle Zones")

barplot(table(seattle_pd_911$Census.Tract), main = "Seattle Census Tracts")


# Check counts of incident groups

table(seattle_pd_911$Initial.Type.Group)

# Check data type 

str(seattle_pd_911$Initial.Type.Group)

# Create a violent variable from "Initial.Type. Group" column

# U.S. Department of Justice, Office of Justice Programs, Bureau of Justice Statistics:
# "Nonviolent crimes are defined as property, drug, and public order
# offenses which do not involve a threat of harm or an actual attack upon a
# victim. Typically, the most frequently identified nonviolent crimes
# involve drug trafficking, drug possession, burglary, and larceny."

seattle_pd_911$violent_incident <- c()
seattle_pd_911$violent_incident <- ifelse(seattle_pd_911$Initial.Type.Group == "ASSAULTS" |
                                            seattle_pd_911$Initial.Type.Group == "CASUALTIES" | 
                                            seattle_pd_911$Initial.Type.Group == "CRISIS CALL" |
                                            seattle_pd_911$Initial.Type.Group == "GUN CALLS" | 
                                            seattle_pd_911$Initial.Type.Group == "PERSON DOWN/INJURY" | 
                                            seattle_pd_911$Initial.Type.Group == "THERATS, HARRASMENT" | 
                                            seattle_pd_911$Initial.Type.Group == "WEAPON CALLS", 1, 0)

# Check count of violent incident variable 

table(seattle_pd_911$violent_incident)

describe(seattle_pd_911$violent_incident)

str(seattle_pd_911$violent_incident)

# Change data types for character variables in dataset

str(seattle_pd_911)

# $ At.Scene.Hour              : chr  "17" "21" "18" "14" ...
# $ At.Scene.Date              : chr  "01/04/2017" "08/22/2017" "08/22/2017" "03/16/2017" ...
# $ At.Scene.Month             : chr  "01" "08" "08" "03" ...
# $ At.Scene.Weekday           : chr  "3" "2" "2" "4" ...

seattle_pd_911$violent_incident <- as.factor(seattle_pd_911$violent_incident)
str(seattle_pd_911$violent_incident)
levels(seattle_pd_911$violent_incident)
length(seattle_pd_911$violent_incident)
table(seattle_pd_911$violent_incident)

# Check prevalence for outcome variable 

prev_1 <- mean(seattle_pd_911$violent_incident == "1")
prev_1
prev_0 <- mean(seattle_pd_911$violent_incident == "0")
prev_0

seattle_pd_911$hours <- as.factor(seattle_pd_911$At.Scene.Hour)
str(seattle_pd_911$hours)
levels(seattle_pd_911$hours)
length(seattle_pd_911$hours)

seattle_pd_911$mins <- as.factor(seattle_pd_911$At.Scene.Min)
str(seattle_pd_911$mins)
levels(seattle_pd_911$mins)
length(seattle_pd_911$mins)

seattle_pd_911$weekdays <- as.factor(seattle_pd_911$At.Scene.Weekday)
str(seattle_pd_911$weekdays)
levels(seattle_pd_911$weekdays)
length(seattle_pd_911$weekdays)

seattle_pd_911$months <- as.factor(seattle_pd_911$At.Scene.Month)
str(seattle_pd_911$months)
levels(seattle_pd_911$months)
length(seattle_pd_911$months)


##################################################################
## 2. train and test sets
##################################################################

## 2.1  Create train set and validation set

# test set will be 10% of the movieLens dataset
# train set will be 90% of the movielens dataset

set.seed(1)
test_index <- createDataPartition(y = seattle_pd_911$violent_incident, times = 1, p = 0.1, list = FALSE)
train <- seattle_pd_911[-test_index,]
test <- seattle_pd_911[test_index,]

## 2.2 Examine the sets

# Check the structure of the datasets

str(seattle_pd_911$violent_incident)
str(train$violent_incident)
str(test$violent_incident)

# Check the dimensions of the datasets after the creation of the train and test sets

table(seattle_pd_911$violent_incident)

table(train$violent_incident)

table(test$violent_incident)

# Use violent variable for modeling 

# Omit NA's if any missed earlier 

train <- na.omit(train)
test <- na.omit(test)


##################################################################
## Models
##################################################################

# KNN3 

# model 1

fit <- knn3(violent_incident ~ hours, data = train)
y_hat_knn <- predict(fit, test, type = "class")
confusionMatrix(y_hat_knn, test$violent_incident)$overall["Accuracy"]

## model 2 with diffrent k's

fit_2 <- knn3(violent_incident ~ hours + Incident.Location, data = train, k = 11)
y_hat_knn <- predict(fit_2, test, type = "class")
confusionMatrix(y_hat_knn, test$violent_incident)$overall["Accuracy"]

fit_2 <- knn3(violent_incident ~ hours + Incident.Location, data = train, k = 15)
y_hat_knn <- predict(fit_2, test, type = "class")
confusionMatrix(y_hat_knn, test$violent_incident)$overall["Accuracy"]

fit_2 <- knn3(violent_incident ~ hours + Incident.Location, data = train, k = 20)
y_hat_knn <- predict(fit_2, test, type = "class")
confusionMatrix(y_hat_knn, test$violent_incident)$overall["Accuracy"]

fit_2 <- knn3(violent_incident ~ hours + Incident.Location, data = train, k = 25)
y_hat_knn <- predict(fit_2, test, type = "class")
confusionMatrix(y_hat_knn, test$violent_incident)$overall["Accuracy"]

fit_2 <- knn3(violent_incident ~ hours + Incident.Location, data = train, k = 30)
y_hat_knn <- predict(fit_2, test, type = "class")
confusionMatrix(y_hat_knn, test$violent_incident)$overall["Accuracy"]

## 
fit_2 <- knn3(violent_incident ~ Incident.Location, data = train)
y_hat_knn <- predict(fit_2, test, type = "class")
confusionMatrix(y_hat_knn, test$violent_incident)$overall["Accuracy"]

fit_2 <- knn3(violent_incident ~ Census.Tract, data = train)
y_hat_knn <- predict(fit_2, test, type = "class")
confusionMatrix(y_hat_knn, test$violent_incident)$overall["Accuracy"]

# Find optimal k for chosen model 

# Takes time. Optimal k = 12, Accuray =  0.5730337
ks <- seq(3, 18, 3)

library(purrr)
accuracy <- map_df(ks, function(k){
  fit <- knn3(violent_incident ~ hours + Incident.Location, data = train, k = k)
  
  y_hat <- predict(fit, train, type = "class")
  cm_train <- confusionMatrix(data = y_hat, reference = train$violent_incident)
  train_error <- cm_train$overall["Accuracy"]
  
  y_hat <- predict(fit, test, type = "class")
  cm_test <- confusionMatrix(data = y_hat, reference = test$violent_incident)
  test_error <- cm_test$overall["Accuracy"]
  
  tibble(train = train_error, test = test_error)
})

ks[which.max(accuracy$test)]

max(accuracy$test)

fit_2 <- knn3(violent_incident ~ hours + Incident.Location, data = train, k = 12)
y_hat_knn <- predict(fit_2, test, type = "class")
confusionMatrix(y_hat_knn, test$violent_incident)$overall["Accuracy"]
confusionMatrix(y_hat_knn, test$violent_incident)


## RF

if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")

library(randomForest)
train_rf <- randomForest(violent_incident ~ hours, data=train)
confusionMatrix(predict(train_rf, test), test$violent_incident)$overall["Accuracy"]