file_path_yiqi <- "C:/Users/98669/Desktop/Master/TERM3/Advanced R/github/r_advanced_group/"
data_backup <- read.csv(file.path(file_path_yiqi, "train.csv"), header=TRUE,sep=",");
prediction <- read.csv(file.path(file_path_yiqi, "test.csv"), header=TRUE,sep=",");
submission <- read.csv(file.path(file_path_yiqi, "submission.csv"), header=TRUE,sep=",");

############################# 0. Import libraries ###############################################################
# install.packages("class");
library(data.table);
library(dplyr);
library(foreach);
library(doParallel);
library(pROC);
library(cluster);
library(astsa);
library(forecast);
library(class);
library(e1071);
library(caTools);
library(caret);

############################# 1. Data Preparation & Split (Confirmed Cases) #####################################
data <- data_backup;
str(data);

# Delete Fatalities & set Confirmed as target
data <- data[,-6];
data <- data[,-1];
setnames(data, "ConfirmedCases", "Confirmed");

prediction <- prediction[,-1];

# Convert to right format
data$Date <- as.numeric(as.Date(data$Date, format = "%Y-%m-%d"));
data <- data %>% mutate_if(is.character, as.factor) %>% mutate_if(is.factor, as.numeric);
str(data);
# head(data);

prediction$Date <- as.numeric(as.Date(prediction$Date, format = "%Y-%m-%d"));
prediction <- prediction %>% mutate_if(is.character, as.factor) %>% mutate_if(is.factor, as.numeric);
str(prediction);
# head(prediction);

# Normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) }

data[,1:2] <- as.data.frame(lapply(data[,1:2], normalize));
prediction[,1:2] <- as.data.frame(lapply(prediction[,1:2], normalize));

# setting seed to reproduce results of random sampling
set.seed(100); 

# perc splitting
perc_split <- c(0.7, 0.15, 0.15);

# row indices for training data (70%)
train_index <- sample(1:nrow(data), perc_split[1]*nrow(data));  

# row indices for validation data (15%)
val_index <- sample(setdiff(1:nrow(data), train_index), perc_split[2]*nrow(data));  

# row indices for test data (15%)
test_index <- setdiff(1:nrow(data), c(train_index, val_index));

# split data
train <- data[train_index,]; 
val <- data[val_index,]; 
test  <- data[test_index,];

# Check dimensions
dim(data);
dim(train);
dim(val);
dim(test);

############################### 1.1 Train KNN for Confirmed cases & Predict ####################################
# Let's try with k=10 for train & test dataset:
classifier_knn <- knn(train = train, test = test, cl = train$Confirmed, k=10);

# u <- union(classifier_knn, test$target);
# t <- table(factor(classifier_knn, u), factor(test$target, u));
# confusionMatrix(t);

# Optimization based on accuracy for each k value (k=1 ~ k=30)
i=1
k.optm=1
for (i in 1:30){
  model <- knn(train=train, test=test, cl=train$Confirmed, k=i)
  k.optm[i] <- 100 * sum(test$Confirmed == model)/NROW(test$Confirmed)
  k=i
  cat(k,'=',k.optm[i],'')
}

# Accuracy plot
plot(k.optm, type="b", xlab="K- Value",ylab="Accuracy level");

# Select the final model to predict "prediction" data, k=1:
confirmed_prediction <- knn(train[,-4], prediction, train$Confirmed, k=1);
# View(confirmed_prediction);

############################# 2. Data Preparation & Split (Fatalities Cases) ##################################
data <- data_backup;
str(data);

# Delete Confirmed & set Fatalities as target
data <- data[,-5];
data <- data[,-1];

# Convert to right format
data$Date <- as.numeric(as.Date(data$Date, format = "%Y-%m-%d"));
data <- data %>% mutate_if(is.character, as.factor) %>% mutate_if(is.factor, as.numeric);
str(data);
# head(data);

# Normalization
data[,1:2] <- as.data.frame(lapply(data[,1:2], normalize));

#### Skip the split index part

# split data
train <- data[train_index,]; 
val <- data[val_index,]; 
test  <- data[test_index,];

# Check dimensions
dim(data);
dim(train);
dim(val);
dim(test);

############################### 2.1 Train KNN for Fatalities cases & Predict ####################################
# Let's try with k=10 for train & test dataset:
classifier_knn_2 <- knn(train = train, test = test, cl = train$Fatalities, k=10);

# Optimization based on accuracy for each k value (k=1 ~ k=30)
i=1
k.optm=1
for (i in 1:30){
  model <- knn(train=train, test=test, cl=train$Fatalities, k=i)
  k.optm[i] <- 100 * sum(test$Fatalities == model)/NROW(test$Fatalities)
  k=i
  cat(k,'=',k.optm[i],'')
}

#Accuracy plot
plot(k.optm, type="b", xlab="K- Value_2",ylab="Accuracy level");

# Select the final model to predict "prediction" data, k=1:
fatalities_prediction <- knn(train[,-4], prediction, train$Fatalities, k=1);
View(fatalities_prediction);

############################# 4. Build submission file & Save to csv ############################################
submission$ConfirmedCases <- confirmed_prediction;
submission$Fatalities <- fatalities_prediction;
# View(submission);
# write.csv(submission,"test_submission.csv", row.names = FALSE);