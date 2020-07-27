setwd("C:/Users/Nawoda Wijebandara/Desktop/titanic")  #Setting the working directory (you have to set your computer working directory having "train.csv" and "test.csv" files to run this script)

titanic.train <- read.csv(file = "train.csv", stringsAsFactors = FALSE, header = TRUE) #Reading the "train.csv" file and assigning data into titanic.train
titanic.test <- read.csv(file = "test.csv", stringsAsFactors = FALSE, header = TRUE) #Reading the "test.csv" file and assigning data into titanic.test

titanic.train$IsTrainSet <- TRUE #Get a brandnew column as IsTrainSet and set the all values to TRUE in titanic.train data set
titanic.test$IsTrainSet <- FALSE #Get a brandnew column as IsTrainSet and set the all values to FALSE in titanic.test data set

titanic.test$Survived <- NA #Add a Survived column inside the titanic.test data set

titanic.full <- rbind(titanic.train, titanic.test) #combining "titanic.train" and "titanic.test" two data sets into "titanic.full"

titanic.full[titanic.full$Embarked=='', "Embarked"] <- 'S'  #Cleaning the missing values in "Embarked" column

age.median <- median(titanic.full$Age, na.rm = TRUE) #Finding the median of the age

titanic.full[is.na(titanic.full$Age), "Age"] <- age.median #Assigning the median age into missing age values


upper.bound <- boxplot.stats(titanic.full$Fare)$stats[5] #Finding the upper bound of Fare
outlier.filter <- titanic.full$Fare < upper.bound #Building the filter (here no need to have minimum bound because it is zero)
titanic.full[outlier.filter,] #getting all the rows that are not outliers

#building a model in order to predict fare-----------------------------------------------------
fare.equation = "Fare ~ Pclass + Sex + Age + SibSp + Parch + Embarked"
fare.model <- lm(
  formula = fare.equation,
  data = titanic.full[outlier.filter,]
)
#----------------------------------------------------------------------------------------------

#Finding missing values------------------------------------------------------------------------
fare.row<-titanic.full[
  is.na(titanic.full$Fare),
  c("Pclass", "Sex", "Age", "SibSp", "Parch", "Embarked")
]
#----------------------------------------------------------------------------------------------

# Predicting missing values--------------------------------------------------------------------
fare.predictions<-predict(fare.model, newdata = fare.row )
titanic.full[is.na(titanic.full$Fare), "Fare"] <- fare.predictions
#----------------------------------------------------------------------------------------------

#categorical casting except "survived" column--------------------------------------------------
titanic.full$Pclass <- as.factor(titanic.full$Pclass)
titanic.full$Sex<- as.factor(titanic.full$Sex)
titanic.full$Embarked<- as.factor(titanic.full$Embarked)
#----------------------------------------------------------------------------------------------

titanic.train<-titanic.full[titanic.full$IsTrainSet==TRUE,] #split data set back out into train data set
titanic.test<-titanic.full[titanic.full$IsTrainSet==FALSE,] #split data set back out into test data set

titanic.train$Survived <- as.factor(titanic.train$Survived) #casting "survived" into a category

survived.equation <- "Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked" #Building a equation in order to get prediction of "survived" column
survived.formula <- as.formula(survived.equation)  #Creating a formula according to above equation

install.packages("randomForest")  #installing "randomForest" package
library(randomForest) #importing "randomForest" library

titanic.model <- randomForest(formula = survived.formula, data = titanic.train, ntree = 500, mtry = 3, nodesize =0.01 * nrow(titanic.test)) #creating predictive model using randomForest library

features.equation <- "Pclass + Sex + Age + SibSp + Parch + Fare + Embarked" #specifying features because we need to use passengerId
Survived <- predict(titanic.model, newdata = titanic.test) #calling predict function and assigning results into "Survived"

PassengerId <- titanic.test$PassengerId #Assigning PassengerId
output.df <- as.data.frame(PassengerId) #Creating a data frame with PassengerId
output.df$Survived <- Survived #Outputing prediction results with PassengerId

write.csv(output.df, file="Submission.csv", row.names = FALSE) #Output the csv file with prediction results
