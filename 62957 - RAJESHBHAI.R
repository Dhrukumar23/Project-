######  VALAND DHRUKUMAR RAJESHBHAI
##### Student id - 62957
###### CLASSIFICATION
###### DECISION TREE

library(caret)

### importing the dataset through read.csv function

Data <- read.csv("liver_cirrhosis.csv")


### using head function to view the first six observation of the dataset with all the variables

head(Data)



# have seen first six observations with all 19 variables


### using str function to view structure of the dataset

str(Data)

### using summary function to view statistics of observation in the dataset

summary(Data)


### convert the character variables into factors using as.factor

Data$Status <- as.factor(Data$Status)

Data$Sex <- as.factor(Data$Sex)

Data$Ascites <- as.factor(Data$Ascites)

Data$Hepatomegaly <- as.factor(Data$Hepatomegaly)

Data$Edema <- as.factor(Data$Edema)

Data$Spiders <- as.factor(Data$Spiders)

Data$Stage <- as.factor(Data$Stage)

Data$Drug <- as.factor(Data$Drug)

### data Partition 
### supervised learning based on train and test concept(because of prediction the unknown values)



partition <- sample(2,nrow(Data),replace = TRUE, prob = c(0.7,0.3))

train <- Data[partition ==1,]
test <- Data[partition == 2,]


################### Decision Tree first algorithms ################################

library(rpart)
library(rpart.plot)

Algo_1 <- rpart(Stage ~.,
                data = train,
                method = "class",
                control = rpart.control(minsplit = 150),
                parms = list(split = "information"))
summary(Algo_1)


Algo_2 <- rpart(Stage ~ Prothrombin +  N_Days+ Hepatomegaly + Albumin + Bilirubin + SGOT  + Status + Edema + Platelets + Cholesterol + Ascites,
                data = train,
                method = "class",
                control = rpart.control(minsplit = 150),
                parms = list(split = "information"))

summary(Algo_2)                


rpart.plot(Algo_2, type = 4, extra = 2, faclen = 7, cex = 0.5 )


## prediction on unknown dataset

estimate <- predict(Algo_2, test, type = "class")

estimate


# confusion Matrix 

Matrix <- confusionMatrix(test$Stage, estimate)

Matrix


accuracy <- Matrix$overall['Accuracy']
print(accuracy)


# Decision tree  60.45 %

### Random Forest


library(randomForest)

Algo_3 <- randomForest(Stage ~., data = train)


## Print the summary of the model

print(Algo_3)

## Plot the model

plot(Algo_3)


## Tune Rf function for optimizing the OOb Error rate 

tuneRF(train[,-19],train[,19],stepFactor = 12, ntreeTry = 500, improve =0.5)



## Model building with optimum number of variables

Algo_4 <- randomForest(Stage ~., data =train, ntree = 500, mtry = 4)

## print the result
print(Algo_4)

## plot the model

plot(Algo_4)



# prediction on unknown dataset

rf_estimate <- predict(Algo_4, test, type = "class")

rf_estimate


# confusion Matrix 

Matrix_rf <- confusionMatrix(test$Stage, rf_estimate)

Matrix_rf


accuracy_2 <- Matrix_rf$overall['Accuracy']
print(accuracy_2)

# Random forest accuracy 95.26 

# in conclusion the Random forest model outperforms the Decision tree
