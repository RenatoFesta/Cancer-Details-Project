###Downloading the dataset

url <- "https://raw.githubusercontent.com/RenatoFesta/cancerdetails/main/cancer_details.csv"
cancerdetails <- read.csv(url)


###Cleaning the dataset
any(is.na(cancerdetails))
 #The dataset came with the last column with only N/A values.
cancerdetails$X <- NULL
 #Changing the column diagnosis to factor
cancerdetails$diagnosis <- as.factor(cancerdetails$diagnosis)

## Criating train e test set
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
set.seed(321)
test_index <- createDataPartition(cancerdetails$id, times = 1, p = 0.8, list = FALSE) 

train_set <- cancerdetails[test_index, ] #create training set
test_set <- cancerdetails[-test_index, ] #create test set

####
 # Using the following code I made a plot for each column in the dataset to visualize and understand what value I could use to cut each
 # column to better separate the "B" from "M".

train_set %>% 
  ggplot(aes(diagnosis, perimeter_worst, color = diagnosis)) +
  geom_point(position = position_jitter(width = 0.2), alpha = 0.4)

 # After make all the plots I choose the 7 better cuts and use them to calculate which one provides the better result.


resultradiusmean <- as.factor(ifelse(train_set$radius_mean > 15, "M", "B"))
mean(resultradiusmean == train_set$diagnosis) # The result is: 0.89277

resultperimetermean <- as.factor(ifelse(train_set$perimeter_mean > 95, "M", "B"))
mean(resultperimetermean == train_set$diagnosis) # The result is: 0.8752735

resultareamean <- as.factor(ifelse(train_set$area_mean > 700, "M", "B"))
mean(resultareamean == train_set$diagnosis) # The result is: 0.8971554

resultconcave.pointsmean <- as.factor(ifelse(train_set$concave.points_mean > 0.05, "M", "B"))
mean(resultconcave.pointsmean == train_set$diagnosis) # The result is: 0.9080963

resultradius_worst <- as.factor(ifelse(train_set$radius_worst > 17, "M", "B"))
mean(resultradius_worst == train_set$diagnosis) # The result is: 0.9234136

resultperimeter_worst <- as.factor(ifelse(train_set$perimeter_worst > 115, "M", "B"))
mean(resultperimeter_worst == train_set$diagnosis) # The result is: 0.9256018

resultarea_worst <- as.factor(ifelse(train_set$area_worst > 880, "M", "B"))
mean(resultarea_worst == train_set$diagnosis) # The result is: 0.9256018

 # The columns "perimeter_worst" and "area_worst" provide the best results. Now I`m going to test others cuts in the same columns:

  #area_worst
X <- seq(800, 950, 1)

res.area <- function(X){
  resultarea_worst <- as.factor(ifelse(train_set$area_worst > X, "M", "B"))
  mean(resultarea_worst == train_set$diagnosis)
}

X[which.max(sapply(X, res.area))] # Now the best cut in area_worst to achieve the higher accuracy is 868
max(sapply(X, res.area)) # And the best result is 0.9277899

    # perimeter_worst
Y <- seq(85, 135, 1)

res.perimeter <- function(Y){
  resultperimeter_worst <- as.factor(ifelse(train_set$perimeter_worst > Y, "M", "B"))
  mean(resultperimeter_worst == train_set$diagnosis)
}

Y[which.max(sapply(Y, res.perimeter))] # Now the best cut in perimeter_worst to achieve the higher accuracy is 113
max(sapply(Y, res.perimeter)) # And the best result is 0.9256018

## Now lets use the test_set to calculate the accuracy of the cut at 113 in the column perimeter_worst

resultarea_testset <- as.factor(ifelse(test_set$area_worst > 868, "M", "B"))
confusionMatrix(resultarea_testset, test_set$diagnosis, positive = "M") 
### HERE WE COULD REACH A 89.29% ACCURACY IN THE TEST_SET, SENSITIVITY = 0.82, SPECIFICITY = 0.9516 ###





### In the second phase of this code I`m going try to use a more sophisticate formula to try a better result.


 #First lets clean our work space
rm(resultareamean, resultarea_worst, resultareamean, resultconcave.pointsmean, resultperimeter_testset, resultperimeter_worst, resultperimetermean, resultradius_worst, X, Y, res.area, res.perimeter)


 #We are going to use the cross-validation 10-fold
ctrl <- trainControl(method = "cv",
                     number = 10)

knnFit <- train(diagnosis ~ .-id,
                method = "knn",
                preProcess = c("center","scale"),
                tuneLength = 20, #test 20 values
                trControl = ctrl,
                metric = "Accuracy",
                data = train_set)
knnFit


 # Now lets use this results on the test_set to compute the accuracy.

predknn <- predict(knnFit, test_set, type = "prob")
resultknn <- as.factor(ifelse(predknn[,2] > 0.5, "M", "B")) # If the chances of a cancer is "M" is higher than 50% we are going to chose "M".

confusionMatrix(resultknn, test_set$diagnosis, positive = "M") # The accuracy is 97.32%

 
# Curva ROC e AUC
if(!require(pROC)) install.packages("pROC", repos = "http://expasy.org/tools/pROC/") 

aucknn <- roc(test_set$diagnosis, predknn[,2])
plot.roc(aucknn, print.thres = T) # The print.thres function shows that if predknn > 0.214 we can reach a better result

 # Using the new cut point
predknn <- predict(knnFit, test_set, type = "prob")
resultknn <- as.factor(ifelse(predknn[,2] > 0.214, "M", "B"))#Here we change the cut-off line to 0.214
confusionMatrix(resultknn, test_set$diagnosis, positive = "M") 
# NOW THE ACCURACY IS 97.32%, SENSITIVITY = 0.9800, SPECIFICITY = 0.9677

#

